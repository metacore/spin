(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Video client that provides a direct path for video packets from
 *	the network to the framebuffer.  Defines a video guard and event
 *	handler and installs it on the Udp.PacketArrived event.
 *
 *)

(* Untrusted *) 
MODULE UdpVideoClient;

IMPORT Net;
IMPORT Udp, UdpDefault, UdpVideo;
IMPORT IpPktFormat;
IMPORT Fli;
IMPORT Video;

IMPORT Word;
IMPORT Mbuf;
IMPORT Ctypes;
IMPORT IO; 
IMPORT Fmt;

IMPORT Spy,SAL;

IMPORT CPUPrivate, Machine, Strand; (* strands *)
IMPORT Sema;


(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;
<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand);

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "UdpVideoClient");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN

    IF argc > 2 AND Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF argc > 2 AND Text.Equal(argv[1], "port") THEN
      port := Net.htons(SafeConvert.Atoi(argv[2]));
    ELSIF argc > 1 AND Text.Equal(argv[1], "on") THEN
      dpy := Video.New(0);
      height := Video.FrameHeight(dpy);
      width := Video.FrameWidth(dpy);
      Video.ClearScreen(dpy);
      Video.SetScreenPos(dpy,0,0);
      IO.Put("UdpVideoClient opened video device.\n");
    ELSE
      IO.Put("UdpRpc.RunCommand: no such command ");
      FOR i := 0 TO argc-1 DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;
    RETURN TRUE;
  END RunCommand;



CONST
  dataoffset = BYTESIZE(UdpDefault.Header) + BYTESIZE(UdpVideo.DataPacket);
  VideoPort = 6666;

VAR
  dataflow: Spy.T;
  videodatatimer: Spy.T;
  udptimer: Spy.T;

  videodata: REFANY;
  videoresp: REFANY;
  dpy: Video.T;
  height, width: Ctypes.unsigned_short;

  videoread: Sema.T;

  debug_level : Net.Level := Net.oLevel.NODEBUG;

(*

  for each UdpVideo.ProtoType create a whenclause and handler.

*)

(* WHEN CLAUSE CLOSURE ??? *)
VAR
  port: Ctypes.unsigned_short := Net.htons(VideoPort);
PROCEDURE WhenClause_VideoData (READONLY packet: Mbuf.T; READONLY payload: Udp.T):BOOLEAN = 
  BEGIN
    (* need imposed when clause to prevent user from looking at all packets.  *)
    WITH udpheader = VIEW(payload^,NewT),
         user_payload = SUBARRAY(payload^,BYTESIZE(UdpDefault.Header),BYTESIZE(UdpVideo.DataPacket)),
         data_packet = VIEW(user_payload,UdpVideo.DataPacket)
     DO

      (* #ifdef debug_level != NODEBUG *)
      IF debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO,"UdpVideoDataGuard port/type " & 
          Fmt.Unsigned(udpheader.dport) & " " & 
          Fmt.Unsigned(data_packet.type) & "\n");
      END;

      RETURN udpheader.dport = port AND data_packet.type = UdpVideo.Data;
    END;
  END WhenClause_VideoData;

PROCEDURE PacketArrived_VideoData (READONLY packet: Mbuf.T; READONLY payload: Udp.T):BOOLEAN =
  VAR
    fhbuf:ARRAY[1..Fli.FRAME_HEADER_SIZE] OF CHAR;
    offset: CARDINAL;
    lsb:BOOLEAN;
    tot_len:Ctypes.unsigned_short;
    csum:Ctypes.unsigned_short;
  BEGIN
    IF dpy = NIL THEN RETURN FALSE; END;

    Spy.Enter(udptimer);
    (*
      IO.Put("VideoClient received a " & Fmt.Int(tot_len) & " sized video packet.\n");
    *)

    Spy.Enter(videodatatimer);
    WITH udp = VIEW(payload^,NewT),
         fh = fhbuf, (* SUBARRAY(fhbuf,FIRST(fhbuf),Fli.FRAME_HEADER_SIZE) *)
         flibuffer = SUBARRAY(payload^,dataoffset,NUMBER(payload^)-dataoffset)
     DO

      (* XXX compute and verify checksum of the video data *)

      IF Fli.fli_read_frame_header(flibuffer,fh,offset,lsb) THEN

        WITH off = offset+Fli.FRAME_HEADER_SIZE,
             osize  = NUMBER(flibuffer)-off
         DO

          IF debug_level # Net.oLevel.NODEBUG THEN
            Net.Debug(debug_level, Net.oLevel.INFO,"[" & 
              Fmt.Unsigned(off) & "," & Fmt.Unsigned(osize) & "]");
          END;

          WITH flibuffer = SUBARRAY(flibuffer,off,osize) DO
            Fli.fli_display_frame(fh,dpy,200,320,flibuffer,lsb);
          END;
        END;

      ELSE
        IO.Put("Couldn't read Fli frame header from network packet.\n");
      END;
    END;
    Spy.Exit(videodatatimer);
    RETURN TRUE; (* TRUE if consuming packet *)
  END PacketArrived_VideoData; 

PROCEDURE WhenClause_VideoResp (READONLY packet: Mbuf.T; READONLY payload: Udp.T):BOOLEAN = 
  BEGIN
    RETURN FALSE; (* TRUE if consuming packet *)
  END WhenClause_VideoResp;

PROCEDURE PacketArrived_VideoResp (READONLY packet: Mbuf.T; READONLY payload: Udp.T):BOOLEAN =
  BEGIN
    RETURN FALSE; (* TRUE if consuming packet *)
  END PacketArrived_VideoResp; 

PROCEDURE Init() =
  BEGIN
    (* install echo handlers *)

    (* 
       This code should be run by the module that just has dynlinked
       this module to setup the port and to install the handler.
    *)
    dataflow := Spy.Create("udp_video data_flow");
    udptimer := Spy.Create("udp_input video");
    videodatatimer := Spy.Create("video fli disp");
    videodata := Udp.Install(Udp.PacketArrived,
                                    WhenClause_VideoData,
                                    PacketArrived_VideoData);
    IO.Put("UdpVideoClient video data handler installed.\n");

    (* this one probably would stay in user level -- but we don't have user-level, yet 
       videoresp := Udp.Install(Udp.PacketArrived,
       WhenClause_VideoResp,
       PacketArrived_VideoResp);
       IO.Put("UdpVideoClient video resp handler installed.\n");
    *)
    (* EVAL KThread.Fork(, 0, "VideoThread"); *)
  END Init;

BEGIN
END UdpVideoClient. 

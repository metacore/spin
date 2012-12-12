(*
  * Copyright 1994, University of Washington
  * All rights reserved.
  * See COPYRIGHT file for a full description
  *
  *
  * HISTORY
 * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Implements the server side of the video application.  Sends a
 *	video file to N video clients.
 *	
 *
*)

(* Untrusted *) 
MODULE UdpVideoApp;

IMPORT UdpPktFormat, IpPktFormat, UdpGen;
IMPORT UdpVideo;
IMPORT Video;
IMPORT Net;
IMPORT NetDb;
IMPORT Rofs;
IMPORT Fli;
IMPORT IO, Fmt;
IMPORT Word;
IMPORT Spy,SAL;
IMPORT Ctypes;
IMPORT Mbuf;


(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;
<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand);

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 0 AND Text.Equal(argv[0], "UdpVideoApp");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN

    argc := argc;
    IF Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF Text.Equal(argv[1],"cport") THEN
      client_port := Net.htons(SafeConvert.Atoi(argv[2]));
    ELSIF Text.Equal(argv[1],"sport") THEN
      server_port := Net.htons(SafeConvert.Atoi(argv[2]));
    ELSIF Text.Equal(argv[1],"nstreams") THEN
      nstreams := SafeConvert.Atoi(argv[2]);
    ELSIF Text.Equal(argv[1],"send") THEN
      VideoData(argv[1],argv[2],argv[3]);
    ELSE
      IO.Put("UdpRpc.RunCommand: no such command ");
      FOR i := 0 TO argc DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;
    RETURN TRUE;
  END RunCommand;


VAR
  fbuffer: ARRAY [0..Fli.FILE_HEADER_SIZE] OF CHAR;
  databuffer: ARRAY [0..8*1024] OF CHAR;
  dbuf: ARRAY [0..63] OF ARRAY [1..32*1024] OF CHAR;
  debug_level : Net.Level := Net.oLevel.NODEBUG;
  readfromdisk: Spy.T;
  sendtonet: Spy.T;
  client_port: Ctypes.unsigned_short;
  server_port: Ctypes.unsigned_short;
  nstreams : CARDINAL;

PROCEDURE VideoData(READONLY src,dst:TEXT; READONLY fname:TEXT) = 
  VAR
    vid: UNTRACED REF UdpVideo.DataPacket;
    dpy: Video.T;
    rc:Rofs.Errors;
    fd:Rofs.T;
    size, index, height, width, frames, bytesread: CARDINAL;
    lsb: BOOLEAN;
    offset: CARDINAL;
    start : INTEGER;

  CONST
    udp_hdr_len = BYTESIZE(UdpPktFormat.Header);
    ip_hdr_len  = BYTESIZE(IpPktFormat.Header);
  VAR
    data : Mbuf.T;
    ipaddr: Ctypes.unsigned_int;
    ip : IpPktFormat.Header;
    udp: UdpPktFormat.Header;
  BEGIN

    ipaddr := NetDb.GetHostByName(src);
    IF ipaddr # 0 THEN 
      ip.saddr := ipaddr;
    ELSE
      IO.Put("Couldn't find IP address for " & src & "\n");
      RETURN;
    END;
    ipaddr := NetDb.GetHostByName(dst);
    IF ipaddr # 0 THEN 
      ip.daddr := ipaddr;
    ELSE
      IO.Put("Couldn't find IP address for " & dst & "\n");
      RETURN;
    END;
    IO.Put("Sending " & fname & " video to " & dst & ".\n");

    (* open fli file *)
    
    rc := Rofs.Open_file(fd,fname);
    IF rc = Rofs.Errors.NONE THEN    
      dpy := Video.video_new(0);
      Video.video_clear_screen(dpy);

      (* read fli header *)
      index := 0;
      size := Fli.FILE_HEADER_SIZE;

      IO.Put("Reading fli header\n");

      Spy.On();

      rc := Rofs.Read_file(fd,index,fbuffer,size,bytesread);
      WITH f = fbuffer DO

        IF rc = Rofs.Errors.NONE AND bytesread = Fli.FILE_HEADER_SIZE AND Fli.fli_magic(f) THEN

          frames := Fli.fli_frames(f);
          height := Fli.fli_height(f);
          width  := Fli.fli_width(f);

          (* read the rest of fli file *)
          FOR i := 1 TO frames DO
            INC(index,bytesread);

            WITH netbuf = dbuf[i MOD LAST(dbuf)] DO 

              vid := VIEW(netbuf,UdpVideo.DataPacket);
              vid.type := UdpVideo.Data; (* set packet to contain video data *)
              
              WITH fhbuffer = SUBARRAY (netbuf, BYTESIZE(UdpVideo.DataPacket), Fli.FRAME_HEADER_SIZE+8) DO

                Spy.Enter(readfromdisk);
                (* read frame header bytes *)
                rc := Rofs.Read_file(fd,index,fhbuffer,Fli.FRAME_HEADER_SIZE+8,bytesread);

                (* verify that we got the right number of bytes *)
                IF rc = Rofs.Errors.NONE AND bytesread > Fli.FRAME_HEADER_SIZE THEN

                  (* find the frame header in the bytes just read in *)
                  WITH fh = fhbuffer DO
                    IF Fli.fli_read_frame_header(fh,fh,offset,lsb) THEN

                    (* compute size of this frame *)
                    size := Fli.fli_size(fh,Fli.FRAME_HEADER_SIZE_OFFSET,lsb);

                    (* compensate size with frameheader size *)
                    DEC(size,Fli.FRAME_HEADER_SIZE);

                    WITH req = SUBARRAY (netbuf,
                                         BYTESIZE(UdpVideo.DataPacket)+Fli.FRAME_HEADER_SIZE,
                                         size) DO

                      (* set index to start of frame data *)
                      INC(index,Fli.FRAME_HEADER_SIZE+offset);

                      (* read in the frame data *)

                      rc := Rofs.Read_file(fd,index,req,size,bytesread);

                      Spy.Exit(readfromdisk);

                      IF rc = Rofs.Errors.NONE AND bytesread = size THEN
                        
                        WITH vidsize = size + Fli.FRAME_HEADER_SIZE + BYTESIZE(UdpVideo.DataPacket) DO
                          (* IO.Put("["&Fmt.Int(i)&":"&Fmt.Int(vidsize)&"]"); *)
                          Spy.Enter(sendtonet);

                          data := Mbuf.mclgetoa_untraced(req);
                          IF data = NIL THEN
                            IO.Put("IpFrag PANIC Mbuf.mclgetoa_untraced returned NIL.\n");
                          END;

                          (* XXX need an ip header template generator *)
                          ip.tos := 0;
                          ip.tot_len := ip_hdr_len + udp_hdr_len + vidsize;
                          ip.id  := 0;
                          ip.frag_off := 0;
                          ip.ttl := 16_ff;
                          ip.protocol := IpPktFormat.IPPROTO_UDP;

                          (* set udp information *)
                          udp.sport := client_port;
                          udp.dport := server_port;
                          udp.len   := udp_hdr_len + vidsize;

                          FOR i := 0 TO nstreams DO 
                            UdpGen.PacketSend(ip, udp, data);
                          END;

                          Spy.Exit(sendtonet);
                        END;
                      ELSE

                        IO.Put("Could not read fli frame data.\n");
                        EXIT;

                      END;
                    END
                  ELSE
                    IO.Put("fli frame header incorrect.\n");
                    Fli.fli_dump(fh);
                    EXIT;
                  END;
                  END;
                ELSE
                  IO.Put("Could not read fli frame header.\n");
                  EXIT;
                END
              END; (* with fhbuffer *)
            END; (* with netbuf *)
          END;  (* for *)
        ELSE
          IO.Put("Could not read fli file header.\n");
        END;
      END; (* with f = fbuffer  *)
      Spy.Off();
      Rofs.Close_file(fd);
      (* 
         Video.video_dealloc(dpy);
      *)
    ELSE
      IO.Put("Could not open fli file.\n");
    END;
  END VideoData; 

PROCEDURE Init() =
  BEGIN

  readfromdisk := Spy.Create("video_server cam_read");
  sendtonet    := Spy.Create("video_server udp_output");
  client_port  := Net.htons(12121);
  server_port  := Net.htons(21212);
  nstreams     := 0;
  END Init;


BEGIN
END UdpVideoApp.

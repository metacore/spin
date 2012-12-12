(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

(* Untrusted *)
MODULE An1Classification;
IMPORT An1Trusted; (* Installing handler *)
IMPORT An1; (* event we are handling *)
IMPORT Ip;    (* event we are raising  *) 
IMPORT An1PktFormat; (* reusing default stuff *)
IMPORT An1Gen;
IMPORT Net;
IMPORT Mbuf;
IMPORT Spy;
IMPORT Fmt;

(* m3 libraries *)
IMPORT IO;
IMPORT Ctypes;

(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;
VAR shell := Dispatcher.Install(SpinShell.RunCommand,EC_Guard,RunCommand);

PROCEDURE EC_Guard(argc: INTEGER; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "An1Classification");
  END EC_Guard;

PROCEDURE RunCommand(argc: INTEGER; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN

   IF argc > 2 AND Text.Equal(argv[1], "debug") THEN
     debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
   ELSIF argc > 2 AND Text.Equal(argv[1], "install") THEN
     IF Text.Equal(argv[2], "ip") THEN
       (* Install the IP handler that funnels all IP packets up to the next level *)
       ip := An1Trusted.Install(An1.PacketArrived,WhenClause_IP,
                                  PacketArrived_IP);
       IO.Put("An1Classification() IP demuxer installed\n");
     END;
   ELSE
     IO.Put("An1Classification.RunCommand: no such command ");
     FOR i := 0 TO argc DO
       IO.Put(argv[i]);
     END;
     IO.Put("\n");
   END;
   RETURN TRUE;
  END RunCommand;



VAR
  ip_packet_counter   : INTEGER := 0;
  ip: REFANY;
  debug_level : Net.Level := Net.oLevel.NODEBUG;
  recvtimer   := Spy.Create("An1ClInput");
  dataflow    := Spy.Create("An1Packet.Deliver -> An1Cl.PA");

PROCEDURE WhenClause_IP(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN =
  BEGIN
    (* WITH eth = VIEW(payload^,T) DO *)
    WITH eth = VIEW(payload^,NewT) DO
      RETURN eth.type = An1PktFormat.AN1TYPE_IP;
    END;
  END WhenClause_IP;

PROCEDURE PacketArrived_IP(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN = 
  CONST
    header_len = BYTESIZE(An1PktFormat.Header);
  BEGIN
    (*
    Spy.Enter(recvtimer);
    WITH stop = Spy.rpcc() DO
      Spy.Hit(dataflow,packet.mh_hdr.mh_timer[0],stop);
      packet.mh_hdr.mh_timer[0] := stop;
    END;
    *)

    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,Fmt.Unsigned(packet.mh_hdr.mh_len) & "An1Ip ");
    END;

    IF packet.mh_hdr.mh_len > 0 THEN
      INC(ip_packet_counter,1);
      WITH ippayload = Mbuf.Array(packet) DO 

        (* Spy.Exit(recvtimer); *)
        EVAL Ip.PacketArrived(packet, ippayload);
      END;
    END;
    RETURN FALSE;
  END PacketArrived_IP;


PROCEDURE Init() =
  BEGIN
       ip := An1Trusted.Install(An1.PacketArrived,WhenClause_IP,
                                  PacketArrived_IP);
       IO.Put("An1Classification() IP demuxer installed\n");
  END Init;

BEGIN
END An1Classification. 


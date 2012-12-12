(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 21-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Active Message example implementation.  Implementation of the
 *	put/get active message routines that are used by SplitC.  
 *
 *)

(* Untrusted *)
MODULE T3AM;
IMPORT T3Trusted; (* Installing handler *)
IMPORT T3; (* event we are handling *)
IMPORT T3PktFormat;
IMPORT T3Gen;
IMPORT Net;
IMPORT Mbuf;
IMPORT Spy,SAL;

(* m3 libraries *)
IMPORT IO, Fmt;
IMPORT Ctypes;

(* DynShell support *)
IMPORT Text, SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;

CONST 
  header_len  = BYTESIZE(Header);

<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand);

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "T3AM");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
    data  : Mbuf.T;
    packet: Mbuf.T;
  BEGIN
    IF argc > 1 AND Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF argc > 1 AND Text.Equal(argv[1], "on") THEN
      Spy.On();
    ELSIF argc > 1 AND Text.Equal(argv[1], "off") THEN
      Spy.Off();
    ELSIF argc > 1 AND Text.Equal(argv[1], "dump") THEN
      Spy.Dump();
    ELSIF argc > 4 AND Text.Equal(argv[1], "send") THEN
      Spy.On();
      FOR i := 0 TO SafeConvert.Atoi(argv[4]) DO
        packet := Mbuf.m_get(Mbuf.M_WAIT,Mbuf.MT_DATA);
        packet.mh_hdr.mh_len := header_len;
        Mbuf.M_ALIGN(packet,header_len);
        data := packet;

        (* set t3 addr stuff *)
        WITH data_out = Mbuf.Array(data),
             am_out = VIEW(data_out^,NewT)
         DO
          am_out.hndlr  := GET;
          am_out.len    := BYTESIZE(INTEGER);
          am_out.timestamp := SAL.Timestamp();
        END;
        T3Gen.PacketSend(data); (* xxx packet freed by t3net device *)

        (* XXX ugh delay loop.  Use sleep instead. *)
        FOR j := 1 TO 100000 DO END;
      END;
      Spy.Off();
    ELSIF argc > 2 AND Text.Equal(argv[1], "install") THEN
      IF Text.Equal(argv[2], "get") THEN
        amget := T3Trusted.Install(T3.PacketArrived,
                                      WhenClause_AM_GET,
                                      PacketArrived_AM_GET);

        IO.Put("T3AM() Active Message GET handler installed\n");
      ELSIF Text.Equal(argv[2], "put") THEN
        (* Install the AM handler *)
        amput := T3Trusted.Install(T3.PacketArrived,
                                      WhenClause_AM_PUT,
                                      PacketArrived_AM_PUT);
        IO.Put("T3AM() Active Message PUT handler installed\n");

      END;
    ELSE
      IO.Put("T3Client.RunCommand: no such command ");
      FOR i := 0 TO argc-1 DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;

    RETURN TRUE;
  END RunCommand;


VAR
  amput: REFANY;
  amget: REFANY;
  amtimer: Spy.T := Spy.Create("t3_am_roundtrip");

  debug_level : Net.Level := Net.oLevel.NODEBUG;

(* WHEN CLAUSE CLOSURE ??? *)
PROCEDURE WhenClause_AM_GET(READONLY packet: Mbuf.T; READONLY payload: T3.T):BOOLEAN =
  BEGIN
    (* WITH am = VIEW(payload^,T) DO *)

    WITH am_in = Mbuf.Array(packet) DO
      WITH am = VIEW(am_in^,NewT) DO
        IF debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level,Net.oLevel.INFO,"AM_GET_WC handler = " & Fmt.Unsigned(am.hndlr) & " " );
        END;
        RETURN am.hndlr = GET;
      END;
    END;

  END WhenClause_AM_GET; 

PROCEDURE PacketArrived_AM_GET(READONLY packet: Mbuf.T; READONLY payload: T3.T):BOOLEAN = 
  VAR
    data   :Mbuf.T;
    outpacket :Mbuf.T;
  BEGIN
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"AM_GET_PA ");
    END;

    (* allocate the outgoing mbuf *)
    data := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
    data.mh_hdr.mh_len := header_len;
    Mbuf.M_ALIGN(data,header_len);
    outpacket := data;

    WITH data_in = Mbuf.Array(packet),
         am_in = VIEW(data_in^,NewT), 
         data_out = Mbuf.Array(data),
         am_out = VIEW(data_out^,NewT)
     DO

      am_out.hndlr := PUT;
      (* am_out.len   := BYTESIZE(INTEGER); *)
      am_out.timestamp := am_in.timestamp;
    END;
    T3Gen.PacketSend(outpacket); (* xxx packet freed by t3net device *)
    RETURN FALSE;
  END PacketArrived_AM_GET;

(* WHEN CLAUSE CLOSURE ??? *)
PROCEDURE WhenClause_AM_PUT(READONLY packet: Mbuf.T; READONLY payload: T3.T):BOOLEAN =
  BEGIN
    (* WITH am = VIEW(payload^,T) DO *)
    WITH am_in = Mbuf.Array(packet) DO
      WITH am = VIEW(am_in^,NewT) DO
        IF debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level,Net.oLevel.INFO,"AM_PUT_WC handler = " & Fmt.Unsigned(am.hndlr) & " " );
        END;
        RETURN am.hndlr = PUT;
      END;
    END;
  END WhenClause_AM_PUT; 


PROCEDURE PacketArrived_AM_PUT(READONLY packet: Mbuf.T; READONLY payload: T3.T):BOOLEAN = 
  BEGIN
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"AM_GET_PA ");
    END;

    (* extract the rpc timing data *)
    (* XXX assumes that data is in one contiguous mbuf *)
    WITH am_in = Mbuf.Array(packet),
         (* WITH rpc_data = VIEW(origpkt[0],T) DO *)
         am = VIEW(am_in^, NewT),
         stop = SAL.Timestamp()
     DO
      Spy.Hit(amtimer,am.timestamp,stop);
    END;
    RETURN FALSE;
  END PacketArrived_AM_PUT; 

PROCEDURE Init() = 
  BEGIN
    IO.Put("T3AM() start init\n");
    (* Install the AM handler *)
  END Init;


BEGIN
END T3AM.

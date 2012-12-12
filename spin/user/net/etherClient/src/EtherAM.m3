(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new IO interface.
 *
 * 21-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Active Message example implementation.  Implementation of the
 *	put/get active message routines that are used by SplitC.  
 *
 *)

(* Untrusted *)
MODULE EtherAM;

(*
IMPORT EtherTrusted; (* Installing handler *)
IMPORT Ether; (* event we are handling *)
IMPORT Ip;    (* event we are raising  *) 
IMPORT EtherPktFormat;
IMPORT EtherGen;
IMPORT Net;
IMPORT Mbuf;
IMPORT Spy,SAL;
IMPORT NetDb;
IMPORT IpPktFormat;
IMPORT EtherArp;

(* m3 libraries *)
IMPORT IO, Fmt;
IMPORT Ctypes;

(* DynShell support *)
IMPORT Text, SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;

CONST 
  header_len  = BYTESIZE(Header);
  eth_hdr_len = BYTESIZE(EtherPktFormat.Header);

<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand);

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "EtherAM");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
    ip_saddr, ip_daddr: IpPktFormat.Address;
    ether_saddr, ether_daddr: EtherPktFormat.Address;

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
      ip_saddr := NetDb.GetHostByName(argv[2]);
      ip_daddr := NetDb.GetHostByName(argv[3]);

      Spy.On();
      FOR i := 0 TO SafeConvert.Atoi(argv[4]) DO
        data := Mbuf.m_get(Mbuf.M_WAIT,Mbuf.MT_DATA);
        data.mh_hdr.mh_len := header_len;
        Mbuf.M_ALIGN(data,header_len);
        packet := Mbuf.m_prepend(data,eth_hdr_len,Mbuf.M_WAIT);

        (* set ether addr stuff *)
        WITH header_out = Mbuf.Array(packet),
             eth_out = VIEW(header_out^,EtherPktFormat.NewT),
             data_out = Mbuf.Array(data),
             am_out = VIEW(data_out^,NewT)
         DO
          TRY 
            eth_out.shost := EtherArp.Resolve(EtherArp.Ether, ip_saddr);
            eth_out.dhost := EtherArp.Resolve(EtherArp.Ether, ip_daddr);
          EXCEPT
          | EtherArp.NotResolved =>
            Mbuf.m_freem(packet);
            IO.Put("EtherAM. Cound not resolve ip address to ether address.\n");
            RETURN TRUE;
          END;
          eth_out.type  := Net.htons(ETHERTYPE_AM);

          am_out.hndlr  := GET;
          am_out.len    := BYTESIZE(INTEGER);
          am_out.timestamp := SAL.Timestamp();
        END;
        EtherGen.Output(packet); (* xxx packet freed by ethernet device *)

        (* XXX ugh delay loop.  Use sleep instead. *)
        FOR j := 1 TO 100000 DO END;
      END;
      Spy.Off();
    ELSIF argc > 2 AND Text.Equal(argv[1], "install") THEN
      IF Text.Equal(argv[2], "get") THEN
        amget := EtherTrusted.Install(Ether.PacketArrived,
                                      WhenClause_AM_GET,
                                      PacketArrived_AM_GET);

        IO.Put("EtherAM() Active Message GET handler installed\n");
      ELSIF Text.Equal(argv[2], "put") THEN
        (* Install the AM handler *)
        amput := EtherTrusted.Install(Ether.PacketArrived,
                                      WhenClause_AM_PUT,
                                      PacketArrived_AM_PUT);
        IO.Put("EtherAM() Active Message PUT handler installed\n");

      END;
    ELSE
      IO.Put("EtherClient.RunCommand: no such command ");
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
  amtimer: Spy.T;
  debug_level : Net.Level := Net.oLevel.NODEBUG;

(* WHEN CLAUSE CLOSURE ??? *)
PROCEDURE WhenClause_AM_GET(READONLY packet: Mbuf.T; READONLY payload: Ether.T):BOOLEAN =
  BEGIN
    WITH eth = VIEW(payload^,EtherPktFormat.NewT) DO
      WITH am_in = Mbuf.Array(packet) DO
        WITH am = VIEW(am_in^,NewT) DO
          IF debug_level # Net.oLevel.NODEBUG THEN
            Net.Debug(debug_level,Net.oLevel.INFO,"AM_GET_WC ether type = " & Fmt.Unsigned(eth.type) &
              " handler = " & Fmt.Unsigned(am.hndlr) & " " );
          END;
          RETURN eth.type = ETHERTYPE_AM AND am.hndlr = GET;
        END;
      END;
    END;

  END WhenClause_AM_GET; 

PROCEDURE PacketArrived_AM_GET(READONLY packet: Mbuf.T; READONLY payload: Ether.T):BOOLEAN = 
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
    outpacket := Mbuf.m_prepend(data,eth_hdr_len,Mbuf.M_WAIT);

    WITH data_in = Mbuf.Array(packet),
         am_in = VIEW(data_in^,NewT), 
         eth_in = VIEW(payload^,EtherPktFormat.NewT),

         header_out = Mbuf.Array(outpacket),
         eth_out = VIEW(header_out^,EtherPktFormat.NewT),
         data_out = Mbuf.Array(data),
         am_out = VIEW(data_out^,NewT)
     DO

      (* set ether addr stuff *)
      eth_out.dhost := eth_in.shost;
      eth_out.shost := eth_in.dhost;
      eth_out.type  := Net.htons(ETHERTYPE_AM);

      am_out.hndlr := PUT;
      (* am_out.len   := BYTESIZE(INTEGER); *)
      am_out.timestamp := am_in.timestamp;
    END;
    EtherGen.Output(outpacket); (* xxx packet freed by ethernet device *)
    RETURN FALSE;
  END PacketArrived_AM_GET;

(* WHEN CLAUSE CLOSURE ??? *)
PROCEDURE WhenClause_AM_PUT(READONLY packet: Mbuf.T; READONLY payload: Ether.T):BOOLEAN =
  BEGIN
    WITH eth = VIEW(payload^,EtherPktFormat.NewT) DO
      WITH am_in = Mbuf.Array(packet) DO
        WITH am = VIEW(am_in^,NewT) DO
          IF debug_level # Net.oLevel.NODEBUG THEN
            Net.Debug(debug_level,Net.oLevel.INFO,"AM_PUT_WC ether type = " & Fmt.Unsigned(eth.type) &
              " handler = " & Fmt.Unsigned(am.hndlr) & " " );
          END;
          RETURN eth.type = ETHERTYPE_AM AND am.hndlr = PUT;
        END;
      END;
    END;
  END WhenClause_AM_PUT; 


PROCEDURE PacketArrived_AM_PUT(READONLY packet: Mbuf.T; READONLY payload: Ether.T):BOOLEAN = 
  BEGIN
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"AM_GET_PA ");
    END;

    (* extract the rpc timing data *)
    (* XXX assumes that data is in one contiguous mbuf *)
    WITH am_in = Mbuf.Array(packet),
         am = VIEW(am_in^, NewT),
         stop = SAL.Timestamp()
     DO
      Spy.Hit(amtimer,am.timestamp,stop);
    END;
    RETURN FALSE;
  END PacketArrived_AM_PUT; 
*)

(* FIXME *)
PROCEDURE Init() = 
  BEGIN
    <* ASSERT FALSE *>
(*
    (* Install the AM handler *)
    amtimer := Spy.Create("ether_am_roundtrip");
*)
  END Init;

BEGIN
END EtherAM.

(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

(* Untrusted *) 
MODULE An1Default;

IMPORT IO, Fmt;
IMPORT Net;
IMPORT An1Trusted;
IMPORT An1;
IMPORT Ctypes;
IMPORT Mbuf;
IMPORT Spy;


(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Equal(argv[0], "An1Default");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN

   IF Text.Equal(argv[1], "debug") THEN
     debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
   ELSE
     IO.Put("An1Default.RunCommand: no such command ");
     FOR i := 0 TO argc DO
       IO.Put(argv[i]);
     END;
     IO.Put("\n");
   END;
   RETURN TRUE;
  END RunCommand;

VAR
  an1_packet_counter   : INTEGER := 0;

  dataflow : Spy.T;
  promiscuoustimer : Spy.T;
  promiscuous: REFANY;
  ip: REFANY;
  arp: REFANY;

  debug_level : Net.Level := Net.oLevel.NODEBUG;

(* WHEN CLAUSE CLOSURE ??? *)
VAR  
  an1type_ip:Ctypes.unsigned_short:=AN1TYPE_IP;
PROCEDURE WhenClause_IP(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN =
  BEGIN
    (* WITH eth = VIEW(payload^,T) DO *)
    WITH eth = VIEW(payload^,NewT) DO
      IF eth.type = an1type_ip THEN
        RETURN TRUE;
      END;
      RETURN FALSE;
    END;
  END WhenClause_IP;

PROCEDURE PacketArrived_IP(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN = 
  BEGIN
    (* Do protocol specific processing here. *)

    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"An1IP ");
    END;
    RETURN FALSE; (* TRUE if consuming packet *)
  END PacketArrived_IP;

(* WHEN CLAUSE CLOSURE ??? *)
VAR  
  an1type_arp:Ctypes.unsigned_short:=AN1TYPE_ARP;
PROCEDURE WhenClause_ARP(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN =
  BEGIN
    (* WITH eth = VIEW(payload^,T) DO *)
    WITH eth = VIEW(payload^,NewT) DO
      IF eth.type = an1type_arp THEN
        RETURN TRUE;
      END;
      RETURN FALSE;
    END;
  END WhenClause_ARP;

PROCEDURE PacketArrived_ARP(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN = 
  BEGIN
    (* Do protocol specific processing here. *)

    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"An1Arp ");
    END;

    RETURN FALSE; (* TRUE if consuming packet *)
  END PacketArrived_ARP;

(* not a protocol handler -- just counts *)
(* WHEN CLAUSE CLOSURE ??? *)
PROCEDURE WhenClause_PROMISCUOUS(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN =
  BEGIN
    Spy.Enter(promiscuoustimer);
    (* noop *)
    RETURN TRUE;
  END WhenClause_PROMISCUOUS;

PROCEDURE PacketArrived_PROMISCUOUS(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN = 
  BEGIN
    INC(an1_packet_counter,1);
    Spy.Exit(promiscuoustimer);

    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"An1 ");
    END;

    RETURN FALSE; (* TRUE if consuming packet *)
  END PacketArrived_PROMISCUOUS;

PROCEDURE Init() = 
  BEGIN
    (* Install handlers *)
    dataflow := Spy.Create("an1_counter data_flow");
    promiscuoustimer := Spy.Create("an1_input counter");
    promiscuous := An1Trusted.Install(An1.PacketArrived,
                                        WhenClause_PROMISCUOUS,
                                        PacketArrived_PROMISCUOUS);
    IO.Put("An1Default() an1 counter installed.\n");
    ip := An1Trusted.Install(An1.PacketArrived,
                               WhenClause_IP,
                               PacketArrived_IP);
    IO.Put("An1Default() an1IP counter installed.\n");
    arp := An1Trusted.Install(An1.PacketArrived,
                                WhenClause_ARP,
                                PacketArrived_ARP);
    IO.Put("An1Default() an1ARP counter installed.\n");
  END Init;

BEGIN
END An1Default.

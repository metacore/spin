(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
 *
 *)

(* Untrusted *)
MODULE UdpScale;
IMPORT Udp; (* event we are handling *)
IMPORT Net;
IMPORT Mbuf;
IMPORT Spy;

(* m3 libraries *)
IMPORT IO;

(* DynShell support *)
IMPORT Text, SafeConvert;
IMPORT SpinShell;

<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand,Help);

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "UdpScale");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN
    IF Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF Text.Equal(argv[1], "install") AND argc >= 2 THEN
      IF Text.Equal(argv[2], "empty") THEN
        
        FOR i:= 1 TO SafeConvert.Atoi(argv[3]) DO
          spindles[currspindle] := Udp.Install(Udp.PacketArrived,
                                                      empty,
                                                      pa);
          INC(currspindle);
        END;
      ELSIF Text.Equal(argv[2], "full") THEN
        
        FOR i:= 1 TO SafeConvert.Atoi(argv[3]) DO
          spindles[currspindle] := Udp.Install(Udp.PacketArrived,
                                                      all_or_none_guard,
                                                      pa);
          INC(currspindle);
        END;
      END;
    ELSIF Text.Equal(argv[1], "uninstall") AND argc >= 2 THEN
      WHILE currspindle > 0 DO
        DEC(currspindle);
        Udp.Uninstall(spindles[currspindle]);
        spindles[currspindle] := NIL;
      END;
    ELSIF Text.Equal(argv[1], "addone") THEN
      IF one_spindle = NIL THEN
        one_spindle := Udp.Install(Udp.PacketArrived,
                            one_guard,
                            pa);
      ELSE
        IO.Put("Go away! Handler already installed for you.\n");
      END;
    ELSIF Text.Equal(argv[1], "delone") THEN
      IF one_spindle = NIL THEN
        IO.Put("No handler installed.\n");
      ELSE
        Udp.Uninstall(one_spindle);
        one_spindle := NIL;
      END;
    ELSIF Text.Equal(argv[1], "toggleall") THEN
      all_or_none := NOT all_or_none;
    ELSIF Text.Equal(argv[1], "togglefire") THEN
      firefirefire := NOT firefirefire;
    ELSIF Text.Equal(argv[1], "toggleone") THEN
      one := NOT one;
    ELSE
      IO.Put("UdpScale.RunCommand: no such command ");
      FOR i := 0 TO argc DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;

    RETURN TRUE;
  END RunCommand;

PROCEDURE Help () =
  BEGIN
  END Help;

VAR
  spindles: ARRAY [1..100] OF REFANY;
  one_spindle: REFANY := NIL;
  currspindle : [FIRST(spindles) .. LAST(spindles)] := FIRST(spindles);
  debug_level : Net.Level := Net.oLevel.NODEBUG;
  all_or_none: BOOLEAN := FALSE;
  one : BOOLEAN := FALSE;
  firefirefire :BOOLEAN := FALSE;
  timer : Spy.T;
  port := Net.htons(1);


PROCEDURE all_or_none_guard(<* UNUSED *> READONLY packet: Mbuf.T; READONLY payload: Udp.T):BOOLEAN =
  BEGIN
    (* WITH VIEW(payload,UdpPktFormat.T) DO *)
    WITH udp_header = VIEW(payload^,NewT) DO
      IF udp_header.dport = port THEN
        INC(port);
      END;
    END;
    RETURN all_or_none;
  END all_or_none_guard;

PROCEDURE one_guard(<* UNUSED *> READONLY packet: Mbuf.T; READONLY payload: Udp.T):BOOLEAN =
  BEGIN
    WITH udp_header = VIEW(payload^,NewT) DO
      IF udp_header.dport = port THEN
        INC(port);
      END;
    END;
    RETURN one;
  END one_guard;

PROCEDURE empty(<* UNUSED *> READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: Udp.T):BOOLEAN =
  BEGIN
    RETURN firefirefire;
  END empty; 

PROCEDURE pa(<* UNUSED *> READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: Udp.T):BOOLEAN = 
  BEGIN
    RETURN FALSE;
  END pa;


PROCEDURE Init() = 
  BEGIN
    IO.Put("UdpScale() initialized\n");
    timer := Spy.Create("UdpScale timers");
  END Init;

BEGIN
END UdpScale.

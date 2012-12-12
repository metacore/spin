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
MODULE TcpScale;
IMPORT Tcp; (* event we are handling *)
IMPORT Net;
IMPORT Mbuf;
IMPORT Spy;

(* m3 libraries *)
IMPORT IO;
IMPORT Ctypes;

(* DynShell support *)
IMPORT Text, SafeConvert;
IMPORT SpinShell;

<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand,Help);

PROCEDURE EC_Guard(<* UNUSED *> argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Equal(argv[0], "TcpScale");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN
    IF Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF Text.Equal(argv[1], "install") THEN
      FOR i:= 1 TO SafeConvert.Atoi(argv[2]) DO
        spindles[currspindle] := Tcp.Install(Tcp.PacketArrived,
                                                      all_or_none_guard,
                                                      pa);
        INC(currspindle);
      END;
    ELSIF Text.Equal(argv[1], "uninstall") THEN
      FOR i:= 1 TO SafeConvert.Atoi(argv[2]) DO
        Tcp.Uninstall(spindles[currspindle]);
        spindles[currspindle] := NIL;
        DEC(currspindle);
      END;
    ELSIF Text.Equal(argv[1], "addone") THEN
      IF one_spindle = NIL THEN
        one_spindle := Tcp.Install(Tcp.PacketArrived,
                            one_guard,
                            pa);
      ELSE
        IO.Put("Go away! Handler already installed for you.\n");
      END;
    ELSIF Text.Equal(argv[1], "delone") THEN
      IF one_spindle = NIL THEN
        IO.Put("No handler installed.\n");
      ELSE
        Tcp.Uninstall(one_spindle);
        one_spindle := NIL;
      END;
    ELSIF Text.Equal(argv[1], "all") THEN
      all_or_none := TRUE;
    ELSIF Text.Equal(argv[1], "none") THEN
      all_or_none := FALSE;
    ELSIF Text.Equal(argv[1], "toggleone") THEN
      one := NOT one;
    ELSE
      IO.Put("TcpScale.RunCommand: no such command ");
      FOR i := 0 TO argc-1 DO
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
  timer : Spy.T;
  port : Ctypes.unsigned_short := 1;

PROCEDURE all_or_none_guard(<* UNUSED *> READONLY packet: Mbuf.T; READONLY payload: Tcp.T):BOOLEAN =
  BEGIN
    Spy.Enter(timer);
    (* WITH VIEW(payload,UdpPktFormat.T) DO *)
    WITH tcp_header = VIEW(payload^,NewT) DO
      IF tcp_header.dport = port THEN
        INC(port);
      END;
    END;
    RETURN all_or_none;
  END all_or_none_guard;

PROCEDURE one_guard(<* UNUSED *> READONLY packet: Mbuf.T; READONLY payload: Tcp.T):BOOLEAN =
  BEGIN
    Spy.Enter(timer);
    WITH tcp_header = VIEW(payload^,NewT) DO
      IF tcp_header.dport = port THEN
        INC(port);
      END;
    END;
    RETURN one;
  END one_guard;

PROCEDURE pa(<* UNUSED *> READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: Tcp.T):BOOLEAN = 
  BEGIN
    Spy.Exit(timer);
    RETURN FALSE;
  END pa;


PROCEDURE Init() = 
  BEGIN
    timer := Spy.Create("TcpScale timers");
    IO.Put("TcpScale() initialized\n");
  END Init;

BEGIN
END TcpScale.

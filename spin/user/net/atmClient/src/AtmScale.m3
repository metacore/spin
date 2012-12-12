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
MODULE AtmScale;
IMPORT AtmTrusted; (* Installing handler *)
IMPORT Atm; (* event we are handling *)
IMPORT Net;
IMPORT Mbuf;

(* m3 libraries *)
IMPORT IO;

(* DynShell support *)
IMPORT Text, SafeConvert;
IMPORT SpinShell;

<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand,Help);

PROCEDURE EC_Guard(<* UNUSED *> argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Equal(argv[0], "AtmScale");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN
    IF Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF Text.Equal(argv[1], "install") THEN
      FOR i:= 1 TO SafeConvert.Atoi(argv[2]) DO
        spindles[currspindle] := AtmTrusted.Install(Atm.PacketArrived,
                                                      all_or_none_guard,
                                                      pa);
        INC(currspindle);
      END;
    ELSIF Text.Equal(argv[1], "uninstall") THEN
      FOR i:= 1 TO SafeConvert.Atoi(argv[2]) DO
        AtmTrusted.Uninstall(spindles[currspindle]);
        spindles[currspindle] := NIL;
        DEC(currspindle);
      END;
    ELSIF Text.Equal(argv[1], "addone") THEN
      IF one_spindle = NIL THEN
        one_spindle := AtmTrusted.Install(Atm.PacketArrived,
                            one_guard,
                            pa);
      ELSE
        IO.Put("Go away! Handler already installed for you.\n");
      END;
    ELSIF Text.Equal(argv[1], "delone") THEN
      IF one_spindle = NIL THEN
        IO.Put("No handler installed.\n");
      ELSE
        AtmTrusted.Uninstall(one_spindle);
        one_spindle := NIL;
      END;
    ELSIF Text.Equal(argv[1], "all") THEN
      all_or_none := TRUE;
    ELSIF Text.Equal(argv[1], "none") THEN
      all_or_none := FALSE;
    ELSIF Text.Equal(argv[1], "toggleone") THEN
      one := NOT one;
    ELSE
      IO.Put("AtmScale.RunCommand: no such command ");
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

PROCEDURE all_or_none_guard(<* UNUSED *> READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: Atm.T):BOOLEAN =
  BEGIN
    RETURN all_or_none;
  END all_or_none_guard;

PROCEDURE one_guard(<* UNUSED *> READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: Atm.T):BOOLEAN =
  BEGIN
    RETURN one;
  END one_guard;

PROCEDURE pa(<* UNUSED *> READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: Atm.T):BOOLEAN = 
  BEGIN
    RETURN FALSE;
  END pa;


PROCEDURE Init() = 
  BEGIN
    IO.Put("AtmScale() initialized\n");
  END Init;

BEGIN
END AtmScale.

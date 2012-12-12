OBSOLETE


(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
 *
 *)

(* Untrusted *)
MODULE EtherScale;
IMPORT Ether; (* event we are handling *)
IMPORT Net;
IMPORT Mbuf;
IMPORT Spy;

(* m3 libraries *)
IMPORT IO;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* etherdefault *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSE
        Usage();
      END;
    EXCEPT
      ParseParams.Error => Usage();
    END;
   RETURN TRUE;
 END Run;

PROCEDURE Usage() = 
  BEGIN
    IO.Put("Usage: " & CommandName & ":" & CommandHelp & "\n");
  END Usage;
    ELSIF argc > 2 AND Text.Equal(argv[1], "install") THEN
      FOR i:= 1 TO SafeConvert.Atoi(argv[2]) DO
        spindles[currspindle] := Ether.Install(Ether.PacketArrived,
                                               all_or_none_guard,
                                               pa);
        INC(currspindle);
      END;
    ELSIF argc > 2 AND Text.Equal(argv[1], "uninstall") THEN
      FOR i:= 1 TO SafeConvert.Atoi(argv[2]) DO
        Ether.Uninstall(spindles[currspindle]);
        spindles[currspindle] := NIL;
        DEC(currspindle);
      END;
    ELSIF argc > 1 AND Text.Equal(argv[1], "addone") THEN
      IF one_spindle = NIL THEN
        one_spindle := Ether.Install(Ether.PacketArrived,
                            one_guard,
                            pa);
      ELSE
        IO.Put("Go away! Handler already installed for you.\n");
      END;
    ELSIF argc > 1 AND Text.Equal(argv[1], "delone") THEN
      IF one_spindle = NIL THEN
        IO.Put("No handler installed.\n");
      ELSE
        Ether.Uninstall(one_spindle);
        one_spindle := NIL;
      END;
    ELSIF argc > 1 AND Text.Equal(argv[1], "all") THEN
      all_or_none := TRUE;
    ELSIF argc > 1 AND Text.Equal(argv[1], "none") THEN
      all_or_none := FALSE;
    ELSIF argc > 1 AND Text.Equal(argv[1], "toggleone") THEN
      one := NOT one;
    ELSE
      IO.Put("EtherScale.RunCommand: no such command ");
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
  timer: Spy.T;


FUNCTIONAL
PROCEDURE all_or_none_guard(<* UNUSED *> READONLY packet: Mbuf.T; READONLY payload: Ether.T):BOOLEAN =
  VAR b:BOOLEAN;
  BEGIN
    Spy.Enter(timer);
    WITH eth = VIEW(payload^,NewT) DO
      b := eth.type = 0;
      RETURN all_or_none;
    END;
  END all_or_none_guard;

FUNCTIONAL
PROCEDURE one_guard(<* UNUSED *> READONLY packet: Mbuf.T; READONLY payload: Ether.T):BOOLEAN =
  VAR b:BOOLEAN;
  BEGIN
    Spy.Enter(timer);
    WITH eth = VIEW(payload^,NewT) DO
      b := eth.type = 0;
      RETURN one;
    END;
  END one_guard;

PROCEDURE pa(<* UNUSED *> READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: Ether.T):BOOLEAN = 
  BEGIN
    RETURN FALSE;
  END pa;


PROCEDURE Init() = 
  BEGIN
    timer := Spy.Create("EtherScale timer");
    IO.Put("EtherScale() initialized\n");
  END Init;

BEGIN
END EtherScale.

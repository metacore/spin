(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

UNSAFE MODULE DispImpose;
IMPORT IO, Fmt, Sirpa, Dispatcher, DispatcherPrivate;

<* FATAL Dispatcher.Error *>

CONST
  MAX_H = 6; (* has to be divisible by 2 and 3 *)

FUNCTIONAL PROCEDURE GuardTRUE (a1 : INTEGER; a2 : INTEGER): BOOLEAN 
                     RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> GuardTRUE : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
*)
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    RETURN TRUE;
  END GuardTRUE;

FUNCTIONAL PROCEDURE GuardFALSE (a1 : INTEGER; a2 : INTEGER): BOOLEAN 
                      RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> GuardFALSE : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
*)
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    RETURN FALSE;
  END GuardFALSE;

PROCEDURE Test (i: INTEGER): BOOLEAN = 
  VAR
    result: BOOLEAN := TRUE;
    ok: BOOLEAN;
  BEGIN
    ok := TRUE;
    Sirpa.cnt := 0;
    TRY
      Sirpa.Sirpa2(111,222);
    EXCEPT
    | Sirpa.Exception => 
      IO.Put(" exception ");
      ok := FALSE;
    END;
    IF Sirpa.cnt # i THEN
      IO.Put(" cnt mismatch ");
      ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;
    RETURN result;
  END Test;

VAR
  binding : ARRAY [1..MAX_H] OF Dispatcher.Binding;
  guard   : ARRAY [1..MAX_H] OF Dispatcher.ImposedGuard;

(*
 * test invocations
 *)

PROCEDURE DoSingle (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("Test 1\n");
    result := result AND Test (1);
    RETURN result;
  END DoSingle;
    
PROCEDURE DoNILGuards (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("Test 2\n");
    FOR i := 1 TO MAX_H DO
      IO.Put("Test 2 " & Fmt.Int(i) & "\n");
      binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2, NIL, Sirpa.Sirpa2);
      guard[i] := Dispatcher.ImposeGuard(binding[i], GuardTRUE);
      result := result AND Test (i+1);
    END;
    Uninstall();
    RETURN result;
  END DoNILGuards;

PROCEDURE DoTRUEGuards (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("Test 3\n");
    FOR i := 1 TO MAX_H DO
      IO.Put("Test 3 " & Fmt.Int(i) & "\n");
      binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2, 
                                              GuardTRUE, Sirpa.Sirpa2);
      guard[i] := Dispatcher.ImposeGuard(binding[i], GuardTRUE);
      result := result AND Test(i+1);
    END;
    Uninstall();
    RETURN result;
  END DoTRUEGuards;

PROCEDURE DoFALSEGuards (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("Test 4\n");
    FOR i := 1 TO MAX_H DO
      IO.Put("Test 4 " & Fmt.Int(i) & "\n");
      binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2, 
                                              GuardFALSE, Sirpa.Sirpa2);
      guard[i] := Dispatcher.ImposeGuard(binding[i], GuardTRUE);
      result := result AND Test(1);
    END;
    Uninstall();
    RETURN result;
  END DoFALSEGuards;

PROCEDURE DoMixedGuards (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("Test 5\n");
    FOR i := 1 TO MAX_H DIV 2 DO
      IO.Put("Test 5 " & Fmt.Int(i) & "\n");
      binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2,
                                              GuardFALSE, Sirpa.Sirpa2);
      guard[i] := Dispatcher.ImposeGuard(binding[i], GuardTRUE);
      binding[i + (MAX_H DIV 2)] := 
          Dispatcher.InstallHandler (Sirpa.Sirpa2, GuardTRUE, Sirpa.Sirpa2);
      guard[i + (MAX_H DIV 2)] := 
          Dispatcher.ImposeGuard(binding[i + (MAX_H DIV 2)], GuardFALSE);
      result := result AND Test(1);
    END;
    Uninstall();

    RETURN result;
  END DoMixedGuards;

PROCEDURE DoMultipleGuards (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
    k: INTEGER;
  BEGIN
    IO.Put("Test 6\n");
    k := 1;
    FOR i := 1 TO MAX_H DO
      IO.Put("Test 6 " & Fmt.Int(i) & "\n");
      binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2, 
                                              GuardTRUE, Sirpa.Sirpa2);
      guard[i] := Dispatcher.ImposeGuard(binding[i], GuardTRUE);
      guard[i] := Dispatcher.ImposeGuard(binding[i], GuardTRUE);
      IF i MOD 2 = 0 THEN
        guard[i] := Dispatcher.ImposeGuard(binding[i], GuardFALSE);
      ELSE
        INC(k);
      END;
      guard[i] := Dispatcher.ImposeGuard(binding[i], GuardTRUE);
      guard[i] := Dispatcher.ImposeGuard(binding[i], GuardTRUE);
      result := result AND Test(k);
    END;
    Uninstall();

    RETURN result;
  END DoMultipleGuards; 

PROCEDURE Start(i: INTEGER): BOOLEAN =
  BEGIN
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa2, i-1);
    RETURN TRUE;
  END Start;

PROCEDURE Uninstall () =
  BEGIN
    FOR i := 1 TO MAX_H DO
      Dispatcher.Uninstall(binding[i]);
    END;
  END Uninstall;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END DispImpose.

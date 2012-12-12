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
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

UNSAFE MODULE DispCancel;
IMPORT IO, Fmt, Sirpa, Dispatcher, DispatcherPrivate;

<* FATAL Dispatcher.Error *>

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
      IO.Put(" cnt mismatch " & Fmt.Int(i) & " " & Fmt.Int(Sirpa.cnt) & " " );
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

CONST
  MAX_H = 4;

VAR
  binding : ARRAY [1..MAX_H] OF Dispatcher.Binding;
(*
 * test invocations
 *)

PROCEDURE DoSingle (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("Test 1\n");
    FOR i := 1 TO MAX_H DO
      IO.Put("Test 2 " & Fmt.Int(i) & "\n");
      IF i = 1 THEN
        binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2, 
                                                NIL, Sirpa.Sirpa2,
                                                options := Dispatcher.Options{
                                                     Dispatcher.Opt.First,
                                                     Dispatcher.Opt.Cancel});
      ELSE
        binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2, 
                                                NIL, Sirpa.Sirpa2);
      END;
    END;
    result := result AND Test (1);
    Uninstall();
    RETURN result;
  END DoSingle;
    
PROCEDURE DoNILGuards (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("Test 2\n");
    FOR i := 1 TO MAX_H DO
      IO.Put("Test 2 " & Fmt.Int(i) & "\n");
      IF i = 2 THEN
        binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2, 
                                                NIL, Sirpa.Sirpa3,
                                                options := Dispatcher.Options{
                                                     Dispatcher.Opt.Cancel});
      ELSE
        binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2, 
                                                NIL, Sirpa.Sirpa2);
      END;
    END;
    result := result AND Test (3);
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
      IF i = 2 THEN
        binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2, 
                                                NIL, Sirpa.Sirpa2,
                                                options := Dispatcher.Options{
                                                       Dispatcher.Opt.Cancel});
      ELSE
        binding[i] := Dispatcher.InstallHandler(Sirpa.Sirpa2,
                                                NIL, Sirpa.Sirpa2);
      END;
    END;
    result := result AND Test (3);
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
      binding[i+(MAX_H DIV 2)] := Dispatcher.InstallHandler(Sirpa.Sirpa2, 
                                                   GuardTRUE, Sirpa.Sirpa2);
      result := result AND Test(i+1);
    END;
    Uninstall();

    RETURN result;
  END DoMixedGuards;

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
END DispCancel.



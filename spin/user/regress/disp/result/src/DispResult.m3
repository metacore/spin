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

UNSAFE MODULE DispResult;
IMPORT IO, Fmt, Sirpa, Dispatcher, DispatcherPrivate, SpinException;

<* FATAL Dispatcher.Error *>

FUNCTIONAL PROCEDURE IntGuard (a1 : INTEGER; a2 : INTEGER): BOOLEAN 
                     RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> IntGuard : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
*)
    IF a1 # 111 AND a1 # 333 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    RETURN a1 = 111;
  END IntGuard;

<* UNUSED *>
PROCEDURE GuardTRUE (a1 : INTEGER; a2 : INTEGER): BOOLEAN 
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

<* UNUSED *>
PROCEDURE GuardFALSE (a1 : INTEGER; a2 : INTEGER): BOOLEAN 
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

<* UNUSED *>
PROCEDURE OrThem(VAR final: BOOLEAN; this: BOOLEAN) =
  BEGIN
    IO.Put("----> OrThem  : " & Fmt.Bool(final) & " " & Fmt.Bool(this) & "\n");
    final := final OR this;
  END OrThem; 

PROCEDURE AndThem(VAR final: BOOLEAN; this: BOOLEAN;
                  last: BOOLEAN; <* UNUSED *>VAR arg: REFANY;
                  <* UNUSED *> arg1, arg2: INTEGER) =
  BEGIN
    IO.Put("----> AndThem : " & Fmt.Bool(final) & " " & Fmt.Bool(this) & " " &
      Fmt.Bool(last) & "\n");
    final := final AND this;
    IO.Put("----> AndThem : " & Fmt.Bool(final) & "\n");
  END AndThem; 

PROCEDURE GetAverage(cl: REF INTEGER; VAR final: INTEGER; this: INTEGER;
                     last: BOOLEAN; VAR arg: REFANY;
                     <* UNUSED *> arg1, arg2: INTEGER) 
                     RAISES { Sirpa.Exception } =
  BEGIN
    IF arg = NIL THEN
      IO.Put("----> GetAverage : " & Fmt.Int(final) & " " & 
        Fmt.Int(this) & " " & Fmt.Int(cl^) & " " & 
        Fmt.Bool(last) & "\n");
    ELSE
      IO.Put("----> GetAverage : " & Fmt.Int(final) & " " & 
        Fmt.Int(this) & " " & Fmt.Int(cl^) & " " & 
        Fmt.Int(NARROW(arg, REF INTEGER)^) & " " &
        Fmt.Bool(last) & "\n");
    END;

    IF cl = NIL OR cl^ # 13 + Sirpa.cnt - 1 THEN
      RAISE Sirpa.Exception;
    END;
    INC(cl^);
    IF arg = NIL THEN
      final := 0;
      arg := NEW(REF INTEGER);
      NARROW(arg, REF INTEGER)^ := 1;
    ELSE
      INC(NARROW(arg, REF INTEGER)^);
    END;
    INC(final, this);
    IF last THEN
      final := final * 10000 DIV NARROW(arg, REF INTEGER)^;
    END;
  END GetAverage;

PROCEDURE DoNothing() =
  BEGIN
    INC(Sirpa.cnt);
    IO.Put("DoNothing\n");
  END DoNothing; 

PROCEDURE TestBool(i: INTEGER; expected: BOOLEAN): BOOLEAN = 
  VAR
    result: BOOLEAN := TRUE;
    actual: BOOLEAN;
    ok: BOOLEAN;
  BEGIN
    ok := TRUE;
    Sirpa.cnt := 0;
    TRY
      actual := Sirpa.SirpaTRUE(111,222);
    EXCEPT
    | Sirpa.Exception => 
      IO.Put(" exception ");
      ok := FALSE;
    END;
    IF Sirpa.cnt # i THEN
      IO.Put(" cnt mismatch ");
      ok := FALSE;
    ELSIF actual # expected THEN
      IO.Put(" result mismatch: " & Fmt.Bool(actual) & " " & 
        Fmt.Bool(expected) & "\n");
      ok := FALSE
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;
    RETURN result;
  END TestBool;

PROCEDURE TestInt(i: INTEGER; true: BOOLEAN; expected: INTEGER): BOOLEAN = 
  VAR
    result: BOOLEAN := TRUE;
    actual: INTEGER := -1;
    ok: BOOLEAN;
  BEGIN
    ok := TRUE;
    Sirpa.cnt := 0;
    TRY
      IF true THEN
        actual := Sirpa.SirpaOne(111,222);
      ELSE
        actual := Sirpa.SirpaOne(333,222);
      END;
    EXCEPT
    | Sirpa.Exception => 
      IO.Put(" exception ");
      ok := FALSE;
    | SpinException.Exception =>
      IF i # 0 THEN
        IO.Put(" no result exception ");
      END;
    END;
    IF Sirpa.cnt # i THEN
      IO.Put(" cnt mismatch " & Fmt.Int(Sirpa.cnt) & " " & 
        Fmt.Int(i) & "\n");
      ok := FALSE;
    ELSIF actual # expected THEN
      IO.Put(" result mismatch: " & Fmt.Int(actual) & " " & 
        Fmt.Int(expected) & "\n");
      ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;
    RETURN result;
  END TestInt;

PROCEDURE TestVoid(i: INTEGER; true: BOOLEAN): BOOLEAN = 
  VAR
    result: BOOLEAN := TRUE;
    ok: BOOLEAN;
  BEGIN
    ok := TRUE;
    Sirpa.cnt := 0;
    TRY
      IF true THEN
        Sirpa.SirpaVoid(111,222);
      ELSE
        Sirpa.SirpaVoid(333,222);
      END;
    EXCEPT
    | Sirpa.Exception => 
      IO.Put(" exception ");
      ok := FALSE;
    | SpinException.Exception =>
      IF i # 0 THEN
        IO.Put(" no result exception ");
      END;
    END;
    IF Sirpa.cnt # i THEN
      IO.Put(" cnt mismatch " & Fmt.Int(Sirpa.cnt) & " " & 
        Fmt.Int(i) & "\n");
      ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;
    RETURN result;
  END TestVoid;

VAR
  binding : ARRAY [1..12] OF Dispatcher.Binding;
(*
 * test invocations
 *)

PROCEDURE DoSimple (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("DoSimple\n");
    result := result AND TestBool (1, TRUE);
    RETURN result;
  END DoSimple;
    
PROCEDURE DoResult1 (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
    defResult := NEW(REF BOOLEAN);
  BEGIN
    IO.Put("DoResult1\n");
    defResult^ := TRUE;
    Dispatcher.SetDefaultResult(Sirpa.SirpaTRUE, defResult, NIL);
    Dispatcher.InstallResultHandler(Sirpa.SirpaTRUE, AndThem, NIL);
    binding[1] := Dispatcher.InstallHandler(Sirpa.SirpaTRUE,
                                            NIL, Sirpa.SirpaTRUE);
    result := result AND TestBool (2, TRUE);

    binding[2] := Dispatcher.InstallHandler(Sirpa.SirpaTRUE, 
                                            NIL, Sirpa.SirpaFALSE);
    result := result AND TestBool (3, FALSE);

    binding[3] := Dispatcher.InstallHandler(Sirpa.SirpaTRUE, 
                                            NIL, Sirpa.SirpaTRUE);
    result := result AND TestBool (4, FALSE);

    Uninstall(3);
    defResult^ := FALSE;
    Dispatcher.SetDefaultResult(Sirpa.SirpaTRUE, defResult, NIL);
    Dispatcher.InstallResultHandler(Sirpa.SirpaTRUE, NIL, NIL);
    RETURN result;
  END DoResult1; 

PROCEDURE DoResult2 (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
    defResult := NEW(REF INTEGER);
  BEGIN
    IO.Put("DoResult2\n");
    defResult^ := 2;
    TRY
      TRY
        Dispatcher.SetDefaultResult(Sirpa.SirpaOne, defResult, NIL);
        result := result AND TestInt (1, TRUE, 1);
        binding[1] := Dispatcher.GetOriginalHandler(Sirpa.SirpaOne);
        Dispatcher.Uninstall(binding[1]);
        result := result AND TestInt (0, TRUE, -1);
        Dispatcher.InstallDefaultHandler(Sirpa.SirpaOne, Sirpa.SirpaThree,NIL);
        result := result AND TestInt (1, TRUE, 3);
        binding[2] := Dispatcher.InstallHandler(Sirpa.SirpaOne, IntGuard, 
                                                Sirpa.SirpaFour);
        result := result AND TestInt (1, TRUE, 4);
        result := result AND TestInt (1, FALSE, 3);
        Dispatcher.Uninstall(binding[2]);
        result := result AND TestInt (1, TRUE, 3);
        Dispatcher.InstallDefaultHandler(Sirpa.SirpaOne, NIL, NIL);
        result := result AND TestInt (0, TRUE, -1);
        Dispatcher.Install(binding[1]);
        result := result AND TestInt (1, TRUE, 1);
      EXCEPT
      | Dispatcher.Error =>
        IO.PutError("dispatcher exception\n");
        RETURN FALSE;
      | Sirpa.Exception =>
        IO.PutError("sirpa exception\n");
        RETURN FALSE;
      END;
    FINALLY
      defResult^ := 0;
      Dispatcher.SetDefaultResult(Sirpa.SirpaOne, defResult, NIL);
    END;
    RETURN result;
  END DoResult2;

PROCEDURE DoResult3 (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
    defResult := NEW(REF INTEGER);
    cl: REF INTEGER;
  BEGIN
    IO.Put("DoResult3\n");
    defResult^ := 2;
    TRY
      TRY
        Dispatcher.SetDefaultResult(Sirpa.SirpaOne, defResult, NIL);
        Dispatcher.InstallDefaultHandler(Sirpa.SirpaOne, Sirpa.SirpaThree,NIL);
        cl := NEW(REF INTEGER);
        cl^ := 13;
        Dispatcher.InstallResultHandler(Sirpa.SirpaOne, GetAverage, NIL, cl);
        FOR i := 1 TO 3 DO 
          binding[i] := Dispatcher.InstallHandler(Sirpa.SirpaOne, IntGuard, 
                                                  Sirpa.SirpaFour);
        END;
        result := result AND TestInt (4, TRUE, 32500);
      EXCEPT
      | Dispatcher.Error =>
        IO.PutError("dispatcher exception\n");
        RETURN FALSE;
      | Sirpa.Exception =>
        IO.PutError("sirpa exception\n");
        RETURN FALSE;
      END;
    FINALLY
      defResult^ := 0;
      Dispatcher.SetDefaultResult(Sirpa.SirpaOne, defResult, NIL);
      Dispatcher.InstallDefaultHandler(Sirpa.SirpaOne, NIL, NIL);
      Dispatcher.InstallResultHandler(Sirpa.SirpaOne, NIL, NIL);
      Uninstall(3);
    END;
    RETURN result;
  END DoResult3;

PROCEDURE DoNoResult (): BOOLEAN =
  VAR
    result : BOOLEAN := TRUE;
  BEGIN
    IO.Put("DoNoResult\n");
    TRY
      result := result AND TestVoid (1, TRUE);
      binding[1] := Dispatcher.GetOriginalHandler(Sirpa.SirpaVoid);
      Dispatcher.Uninstall(binding[1]);
      result := result AND TestVoid (0, TRUE);
      Dispatcher.InstallDefaultHandler(Sirpa.SirpaVoid,Sirpa.SirpaVoidToo,NIL);
      result := result AND TestVoid (1, TRUE);
      binding[2] := Dispatcher.InstallHandler(Sirpa.SirpaVoid, 
                                              IntGuard,
                                              Sirpa.SirpaVoidToo);
      result := result AND TestVoid (1, TRUE);
      result := result AND TestVoid (1, FALSE);
      Dispatcher.Uninstall(binding[2]);
      result := result AND TestVoid (1, TRUE);
      Dispatcher.InstallDefaultHandler(Sirpa.SirpaVoid, NIL, NIL);
      result := result AND TestVoid (0, TRUE);
      Dispatcher.Install(binding[1]);
      result := result AND TestVoid (1, TRUE);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("dispatcher exception\n");
      RETURN FALSE;
    | Sirpa.Exception =>
      IO.PutError("sirpa exception\n");
      RETURN FALSE;
    END;
    RETURN result;
  END DoNoResult;

PROCEDURE DoNoArgs (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("DoNoArgs\n");
    TRY
      TRY
        result := result AND TestInt (1, TRUE, 1);
        Dispatcher.InstallResultHandler(Sirpa.SirpaOne, DoNothing, NIL);
        result := result AND TestInt (2, TRUE, 1);
        binding[1] := Dispatcher.InstallHandler(Sirpa.SirpaOne, IntGuard, 
                                                Sirpa.SirpaOne);
        result := result AND TestInt (4, TRUE, 1);
        binding[2] := Dispatcher.InstallHandler(Sirpa.SirpaOne, IntGuard, 
                                                Sirpa.SirpaTwo);
        result := result AND TestInt (6, TRUE, 2);
        binding[3] := Dispatcher.InstallHandler(Sirpa.SirpaOne, IntGuard, 
                                                Sirpa.SirpaThree);
        result := result AND TestInt (8, TRUE, 3);
        binding[4] := Dispatcher.InstallHandler(Sirpa.SirpaOne, IntGuard, 
                                                Sirpa.SirpaFour);
        result := result AND TestInt (10, TRUE, 4);
      EXCEPT
      | Dispatcher.Error =>
        IO.PutError("dispatcher exception\n");
        RETURN FALSE;
      | Sirpa.Exception =>
        IO.PutError("sirpa exception\n");
        RETURN FALSE;
      END;
    FINALLY
      Uninstall(4);
      Dispatcher.InstallResultHandler(Sirpa.SirpaOne, NIL, NIL);
    END;
    RETURN result;
  END DoNoArgs;

PROCEDURE Start(i: INTEGER): BOOLEAN =
  BEGIN
    DispatcherPrivate.SetOptLevel(Sirpa.SirpaTRUE, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.SirpaOne, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.SirpaVoid, i-1);
    RETURN TRUE;
  END Start;

PROCEDURE Uninstall (n: INTEGER) =
  BEGIN
    FOR i := 1 TO n DO
      Dispatcher.Uninstall(binding[i]);
    END;
  END Uninstall;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END DispResult.

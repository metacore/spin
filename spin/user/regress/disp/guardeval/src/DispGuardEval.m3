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

UNSAFE MODULE DispGuardEval;
IMPORT IO, Sirpa, Dispatcher, Fmt, DispatcherPrivate;

<* FATAL Dispatcher.Error *>

VAR
  gCnt: INTEGER;
  hCnt: INTEGER;
  ok: BOOLEAN;
  Bindings: ARRAY [1..3] OF Dispatcher.Binding;
  first: BOOLEAN;

FUNCTIONAL PROCEDURE GTest(i: INTEGER) =
  BEGIN
    IF first THEN
      IF gCnt # i THEN
(*
        IO.Put("[guard " & Fmt.Int(i) & " was " & Fmt.Int(gCnt) & "]\n");
        ok := FALSE;
*)
      END;
      IF hCnt # 1 THEN
(*
        IO.Put("[guard " & Fmt.Int(i) & " was run after a handler]\n");
        ok := FALSE;
*)
      END;
    ELSE
      IF gCnt # i THEN
(*
        IO.Put("[guard " & Fmt.Int(i) & " was " & Fmt.Int(gCnt) & "]\n");
        ok := FALSE;
*)
      END;
      IF hCnt # i THEN
(*
        IO.Put("[guard " & Fmt.Int(i) & " was " & Fmt.Int(hCnt) & "]\n");
        ok := FALSE;
*)
      END;
    END;
(*
    INC(gCnt);
*)
  END GTest;

PROCEDURE HTest(i: INTEGER) =
  BEGIN
    IF first THEN
      IF hCnt # i THEN
        IO.Put("[handler " & Fmt.Int(i) & " was " & Fmt.Int(gCnt) & "]\n");
        ok := FALSE;
      END;
      IF gCnt # 4 THEN
        IO.Put("[handler " & Fmt.Int(i) & " was run before some guard]\n");
        ok := FALSE;
      END;
    ELSE
      IF gCnt # i+1 THEN
        IO.Put("[guard " & Fmt.Int(i) & " was " & Fmt.Int(gCnt) & "]\n");
        ok := FALSE;
      END;
      IF hCnt # i THEN
        IO.Put("[guard " & Fmt.Int(i) & " was " & Fmt.Int(hCnt) & "\n");
        ok := FALSE;
      END;
    END;
    INC(hCnt);
  END HTest;

FUNCTIONAL PROCEDURE G1 (): BOOLEAN = 
  BEGIN
    GTest(1);
    RETURN TRUE;
  END G1;

FUNCTIONAL PROCEDURE G2 (): BOOLEAN = 
  BEGIN
    GTest(2);
    RETURN TRUE;
  END G2;

FUNCTIONAL PROCEDURE G3 (): BOOLEAN = 
  BEGIN
    GTest(3);
    RETURN TRUE;
  END G3;

PROCEDURE H1 () = 
  BEGIN
    HTest(1);
  END H1;

PROCEDURE H2 () = 
  BEGIN
    HTest(2);
  END H2;

PROCEDURE H3 () = 
  BEGIN
    HTest(3);
  END H3;

PROCEDURE Test (): BOOLEAN =
  BEGIN
    Bindings[1] := Dispatcher.InstallHandler(Sirpa.Proc, G1, H1);
    Bindings[2] := Dispatcher.InstallHandler(Sirpa.Proc, G2, H2);
    Bindings[3] := Dispatcher.InstallHandler(Sirpa.Proc, G3, H3);
    
    Sirpa.Proc();

    RETURN ok;
  END Test;

PROCEDURE Start(i: INTEGER): BOOLEAN =
  BEGIN
    first := i <= 4;
    DispatcherPrivate.SetOptLevel(Sirpa.Proc, i-1);
    gCnt := 1;
    hCnt := 1;
    ok := TRUE;
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    FOR i := 1 TO 3 DO 
      Dispatcher.Uninstall(Bindings[i])
    END;
    RETURN TRUE;
  END End;

BEGIN
END DispGuardEval.

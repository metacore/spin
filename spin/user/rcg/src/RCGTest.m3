(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 *
 *)

UNSAFE MODULE RCGTest;

IMPORT Stdio, Wr, Thread, Fmt, Word;
IMPORT Guard;
IMPORT rpcc;

<* FATAL Thread.Alerted, Wr.Failure *>


TYPE
  Proc1 = PROCEDURE (x: INTEGER) : Word.T;
  Proc2 = PROCEDURE (VAR x: INTEGER) : Word.T;
  Proc3 = PROCEDURE (VAR x: ARRAY [0..7] OF [0..255]) : Word.T;


(* test procedures *)

PROCEDURE A1 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 111;
  END A1;

PROCEDURE A2 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 222;
  END A2;

PROCEDURE A3 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 333;
  END A3;

PROCEDURE LinearTest () =
  VAR
    testArray : REF ARRAY OF PROCANY;
    p : PROCANY;
    x : Word.T;
  BEGIN
    testArray := NEW(REF ARRAY OF PROCANY, 5);
    testArray[0] := NIL;
    testArray[1] := A1;
    testArray[2] := A2;
    testArray[3] := NIL;
    testArray[4] := A3;

    (* compute the resulting expression from joining these guards *)
    p := Guard.OptimizeAll (testArray);

    x := LOOPHOLE (p, Proc1) (111);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, Proc1) (222);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, Proc1) (333);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, Proc1) (1);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Unsigned (x) & "\n");
  END LinearTest;

(* test jump table generation *)

PROCEDURE Aa1 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1001;
  END Aa1;

PROCEDURE Aa2 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1002;
  END Aa2;

PROCEDURE Aa3 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1003;
  END Aa3;

PROCEDURE Aa4 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1005;
  END Aa4;

PROCEDURE Aa5 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1055;
  END Aa5;

PROCEDURE Aa6 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1070;
  END Aa6;

PROCEDURE Aa7 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1111;
  END Aa7;

PROCEDURE Aa8 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1234;
  END Aa8;

PROCEDURE Aa9 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1239;
  END Aa9;

PROCEDURE Aa10 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1250;
  END Aa10;

PROCEDURE Aa11 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1280;
  END Aa11;

PROCEDURE Aa12 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1299;
  END Aa12;

PROCEDURE Aa13 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1300;
  END Aa13;

PROCEDURE Aa14 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1355;
  END Aa14;

PROCEDURE Aa15 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1400;
  END Aa15;

PROCEDURE Aa16 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1480;
  END Aa16;

PROCEDURE Aa17 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1500;
  END Aa17;

PROCEDURE Aa18 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1555;
  END Aa18;

PROCEDURE Aa19 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1600;
  END Aa19;

PROCEDURE Aa20 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1612;
  END Aa20;

PROCEDURE Aa21 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1613;
  END Aa21;

PROCEDURE Aa22 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1614;
  END Aa22;

PROCEDURE Aa23 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1615;
  END Aa23;

PROCEDURE Aa24 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1616;
  END Aa24;

PROCEDURE Aa25 (a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1617;
  END Aa25;

PROCEDURE TestJT1 () =
  VAR
    testArray : REF ARRAY OF PROCANY;
    p : PROCANY;
    x : Word.T;
  BEGIN
    testArray := NEW(REF ARRAY OF PROCANY, 4);
    testArray[0] := Aa1;
    testArray[1] := Aa2;
    testArray[2] := Aa3;
    testArray[3] := Aa4;
    
    (* compute the resulting expression from joining these guards *)
    p := Guard.OptimizeAll (testArray);
    
    x := LOOPHOLE (p, Proc1) (1001);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, Proc1) (1002);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, Proc1) (1003);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, Proc1) (1004);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, Proc1) (1005);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, Proc1) (1006);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");

    FOR i := 0 TO 10000 DO
      x := LOOPHOLE (p, Proc1)(i);
    END;

  END TestJT1;


PROCEDURE MeasureJT1 () =
  VAR
    testArray : REF ARRAY OF PROCANY;
    p : PROCANY;
    x : BOOLEAN;
    y : Word.T;
    
    TimerStart, TimerEnd: Word.T;
    runs : ARRAY [0..6] OF INTEGER;

    merged: Proc1;
  CONST
    iters = 10000;

  BEGIN
    runs[0] := 1;
    runs[1] := 2;
    runs[2] := 5;
    runs[3] := 10;
    runs[4] := 15;
    runs[5] := 20;
    runs[6] := 25;

    testArray := NEW (REF ARRAY OF PROCANY, 25);
    testArray[0] := Aa1;
    testArray[1] := Aa2;
    testArray[2] := Aa3;
    testArray[3] := Aa4;
    testArray[4] := Aa5;
    testArray[5] := Aa6;
    testArray[6] := Aa7;
    testArray[7] := Aa8;
    testArray[8] := Aa9;
    testArray[9] := Aa10;
    testArray[10] := Aa11;
    testArray[11] := Aa12;
    testArray[12] := Aa13;
    testArray[13] := Aa14;
    testArray[14] := Aa15;
    testArray[15] := Aa16;
    testArray[16] := Aa17;
    testArray[17] := Aa18;
    testArray[18] := Aa19;
    testArray[19] := Aa20;
    testArray[20] := Aa21;
    testArray[21] := Aa22;
    testArray[22] := Aa23;
    testArray[23] := Aa24;
    testArray[24] := Aa25;

    TimerStart := rpcc.rpcc ();
    FOR i := 0 TO iters DO
      x := Aa1 (i);
    END;
    TimerEnd := rpcc.rpcc ();
    Wr.PutText (Stdio.stdout,
                "Time for 1 guard: " &
                Fmt.Int ((TimerEnd - TimerStart) DIV iters) & "\n");

    TimerStart := rpcc.rpcc ();
    FOR i := 0 TO iters DO
      x := Aa1 (i);
      x := Aa2 (i);
    END;
    TimerEnd := rpcc.rpcc ();
    Wr.PutText (Stdio.stdout,
                "Time for 2 guard: " &
                Fmt.Int ((TimerEnd - TimerStart) DIV iters) & "\n");

    TimerStart := rpcc.rpcc ();
    FOR i := 0 TO iters DO
      x := Aa1 (i);
      x := Aa2 (i);
      x := Aa3 (i);
      x := Aa4 (i);
      x := Aa5 (i);
    END;
    TimerEnd := rpcc.rpcc ();
    Wr.PutText (Stdio.stdout,
                "Time for 5 guards: " &
                Fmt.Int ((TimerEnd - TimerStart) DIV iters) & "\n");

    TimerStart := rpcc.rpcc ();
    FOR i := 0 TO iters DO
      x := Aa1 (i);
      x := Aa2 (i);
      x := Aa3 (i);
      x := Aa4 (i);
      x := Aa5 (i);
      x := Aa6 (i);
      x := Aa7 (i);
      x := Aa8 (i);
      x := Aa9 (i);
      x := Aa10 (i);
    END;
    TimerEnd := rpcc.rpcc ();
    Wr.PutText (Stdio.stdout,
                "Time for 10 guards: " &
                Fmt.Int ((TimerEnd - TimerStart) DIV iters) & "\n");

    TimerStart := rpcc.rpcc ();
    FOR i := 0 TO iters DO
      x := Aa1 (i);
      x := Aa2 (i);
      x := Aa3 (i);
      x := Aa4 (i);
      x := Aa5 (i);
      x := Aa6 (i);
      x := Aa7 (i);
      x := Aa8 (i);
      x := Aa9 (i);
      x := Aa10 (i);
      x := Aa11 (i);
      x := Aa12 (i);
      x := Aa13 (i);
      x := Aa14 (i);
      x := Aa15 (i);
    END;
    TimerEnd := rpcc.rpcc ();
    Wr.PutText (Stdio.stdout,
                "Time for 15 guards: " &
                Fmt.Int ((TimerEnd - TimerStart) DIV iters) & "\n");

    TimerStart := rpcc.rpcc ();
    FOR i := 0 TO iters DO
      x := Aa1 (i);
      x := Aa2 (i);
      x := Aa3 (i);
      x := Aa4 (i);
      x := Aa5 (i);
      x := Aa6 (i);
      x := Aa7 (i);
      x := Aa8 (i);
      x := Aa9 (i);
      x := Aa10 (i);
      x := Aa11 (i);
      x := Aa12 (i);
      x := Aa13 (i);
      x := Aa14 (i);
      x := Aa15 (i);
      x := Aa16 (i);
      x := Aa17 (i);
      x := Aa18 (i);
      x := Aa19 (i);
      x := Aa20 (i);
    END;
    TimerEnd := rpcc.rpcc ();
    Wr.PutText (Stdio.stdout,
                "Time for 20 guards: " &
                Fmt.Int ((TimerEnd - TimerStart) DIV iters) & "\n");

    TimerStart := rpcc.rpcc ();
    FOR i := 0 TO iters DO
      x := Aa1 (i);
      x := Aa2 (i);
      x := Aa3 (i);
      x := Aa4 (i);
      x := Aa5 (i);
      x := Aa6 (i);
      x := Aa7 (i);
      x := Aa8 (i);
      x := Aa9 (i);
      x := Aa10 (i);
      x := Aa11 (i);
      x := Aa12 (i);
      x := Aa13 (i);
      x := Aa14 (i);
      x := Aa15 (i);
      x := Aa16 (i);
      x := Aa17 (i);
      x := Aa18 (i);
      x := Aa19 (i);
      x := Aa20 (i);
      x := Aa21 (i);
      x := Aa22 (i);
      x := Aa23 (i);
      x := Aa24 (i);
      x := Aa25 (i);
    END;
    TimerEnd := rpcc.rpcc ();
    Wr.PutText (Stdio.stdout,
                "Time for 25 guards: " &
                Fmt.Int ((TimerEnd - TimerStart) DIV iters) & "\n");

    
    (* compute the resulting expression from joining these guards *)
    p := Guard.OptimizeAll (testArray);

    Wr.PutText (Stdio.stdout,
                "Testing merged guard for all 25 at " &
                Fmt.Unsigned (LOOPHOLE (p, Word.T)) & "\n");
    FOR i := 0 TO iters DO
      y := LOOPHOLE (p, Proc1) (i);
      IF y # 0 THEN
        Wr.PutText (Stdio.stdout,
                    "i = " & Fmt.Int (i) & ": " & Fmt.Unsigned (y) & "\n");
      END;
    END;

    FOR j := 0 TO LAST (runs) DO
      Wr.PutText (Stdio.stdout,
                  "Measuring merge of " & Fmt.Int (runs[j]) & " guards\n");
      p := Guard.OptimizeGuards (testArray, runs[j]);
      merged := LOOPHOLE (p, Proc1);

      TimerStart := rpcc.rpcc ();
      FOR i := 0 TO iters DO
        y := merged (i);
      END;
      TimerEnd := rpcc.rpcc ();
      Wr.PutText (Stdio.stdout,
                  "Time for merged guard: " &
                  Fmt.Int ((TimerEnd - TimerStart) DIV iters) & "\n");
    END;

  END MeasureJT1;

(******************** test jump table 2 ***********************)

PROCEDURE Ab1 (VAR a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1001;
  END Ab1;

PROCEDURE Ab2 (VAR a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1002;
  END Ab2;

PROCEDURE Ab3 (VAR a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1003;
  END Ab3;

PROCEDURE Ab4 (VAR a:INTEGER) : BOOLEAN =
  BEGIN
    RETURN a = 1005;
  END Ab4;


PROCEDURE TestJT2 () =
  VAR
    testArray : REF ARRAY OF PROCANY;
    p : PROCANY;
    x : Word.T;
  BEGIN

    testArray := NEW(REF ARRAY OF PROCANY, 4);
    testArray[0] := Ab1;
    testArray[1] := Ab2;
    testArray[2] := Ab3;
    testArray[3] := Ab4;
    
    (* compute the resulting expression from joining these guards *)
    p := Guard.OptimizeAll (testArray);
    
    x := 1001;
    x := LOOPHOLE (p, Proc2) (x);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := 1002;
    x := LOOPHOLE (p, Proc2) (x);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := 1003;
    x := LOOPHOLE (p, Proc2) (x);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := 1004;
    x := LOOPHOLE (p, Proc2) (x);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := 1005;
    x := LOOPHOLE (p, Proc2) (x);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := 1006;
    x := LOOPHOLE (p, Proc2) (x);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
  END TestJT2;

(************************* test jump table 3 **************************)

PROCEDURE Ac1 (VAR a:ARRAY [0..7] OF [0..255]) : BOOLEAN =
  BEGIN
    RETURN VIEW (a, INTEGER) = 1001;
  END Ac1;

PROCEDURE Ac2 (VAR a:ARRAY [0..7] OF [0..255]) : BOOLEAN =
  BEGIN
    RETURN VIEW (a, INTEGER) = 1002;
  END Ac2;

PROCEDURE Ac3 (VAR a:ARRAY [0..7] OF [0..255]) : BOOLEAN =
  BEGIN
    RETURN VIEW (a, INTEGER) = 1003;
  END Ac3;

PROCEDURE Ac4 (VAR a:ARRAY [0..7] OF [0..255]) : BOOLEAN =
  BEGIN
    RETURN VIEW (a, INTEGER) = 1005;
  END Ac4;


PROCEDURE TestJT3 () =
  VAR
    testArray : REF ARRAY OF PROCANY;
    p : PROCANY;
    x : Word.T;
    y : ARRAY [0..7] OF [0..255];
  BEGIN
    testArray := NEW(REF ARRAY OF PROCANY, 4);
    testArray[0] := Ac1;
    testArray[1] := Ac2;
    testArray[2] := Ac3;
    testArray[3] := Ac4;
    
    (* compute the resulting expression from joining these guards *)
    p := Guard.OptimizeAll (testArray);
    
    VIEW (y, INTEGER) := 1001;
    x := LOOPHOLE (p, Proc3) (y);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    VIEW (y, INTEGER) := 1002;
    x := LOOPHOLE (p, Proc3) (y);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    VIEW (y, INTEGER) := 1003;
    x := LOOPHOLE (p, Proc3) (y);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    VIEW (y, INTEGER) := 1004;
    x := LOOPHOLE (p, Proc3) (y);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    VIEW (y, INTEGER) := 1005;
    x := LOOPHOLE (p, Proc3) (y);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    VIEW (y, INTEGER) := 1006;
    x := LOOPHOLE (p, Proc3) (y);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
  END TestJT3;

(*************************** test exception raising *********************)

EXCEPTION Foo;
TYPE ProcE = PROCEDURE (a: INTEGER) : Word.T RAISES {Foo};

PROCEDURE RaiseTest () =
  VAR
    testArray := NEW (REF ARRAY OF PROCANY, 3);
    p : PROCANY;
    x : Word.T;
  BEGIN
    testArray[0] := AE1;
    testArray[1] := AE2;
    testArray[2] := AE3;
    
    p := Guard.OptimizeAll (testArray);

    
    x := LOOPHOLE (p, ProcE) (6);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, ProcE) (7);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, ProcE) (8);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Unsigned (x) & "\n");

    TRY
      x := LOOPHOLE (p, ProcE) (5);
      Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    EXCEPT
      Foo => Wr.PutText (Stdio.stdout, "Foo was raised\n");
    END;

  END RaiseTest;

PROCEDURE AE1 (a: INTEGER) : BOOLEAN RAISES {Foo} =
  BEGIN
    IF a = 5 THEN RAISE Foo; END;
    RETURN a = 6;
  END AE1;

PROCEDURE AE2 (a: INTEGER) : BOOLEAN RAISES {Foo} =
  BEGIN
    IF a = 5 THEN RAISE Foo; END;
    RETURN a = 7;
  END AE2;

PROCEDURE AE3 (a: INTEGER) : BOOLEAN RAISES {Foo} =
  BEGIN
    IF a = 5 THEN RAISE Foo; END;
    RETURN a = 8;
  END AE3;


(*************************** test try blocks *********************)

EXCEPTION Bar;
TYPE ProcT = PROCEDURE (a: INTEGER) : Word.T RAISES {Foo, Bar};

PROCEDURE TryTest () =
  VAR
    testArray := NEW (REF ARRAY OF PROCANY, 3);
    p : PROCANY;
    x : Word.T;
  BEGIN
    testArray[0] := AT1;
    testArray[1] := AT2;
    testArray[2] := AT3;
    
    p := Guard.OptimizeAll (testArray);

    
    x := LOOPHOLE (p, ProcT) (6);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, ProcT) (7);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    x := LOOPHOLE (p, ProcT) (8);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Unsigned (x) & "\n");

    TRY
      x := LOOPHOLE (p, ProcT) (5);
      Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
    EXCEPT
    | Bar => Wr.PutText (Stdio.stdout, "Bar was raised\n");
    END;

  END TryTest;

PROCEDURE AT1 (a: INTEGER) : BOOLEAN RAISES {Bar} =
  BEGIN
    TRY
      IF a = 5 THEN RAISE Foo; END;
    EXCEPT
      Foo => RAISE Bar;
    END;
    RETURN a = 6;
  END AT1;

PROCEDURE AT2 (a: INTEGER) : BOOLEAN RAISES {Bar} =
  BEGIN
    TRY
      IF a = 5 THEN RAISE Foo; END;
    EXCEPT
      Foo => RAISE Bar;
    END;
    RETURN a = 7;
  END AT2;

PROCEDURE AT3 (a: INTEGER) : BOOLEAN RAISES {Bar} =
  BEGIN
    TRY
      IF a = 5 THEN RAISE Foo; END;
    EXCEPT
      Foo => RAISE Bar;
    END;
    RETURN a = 8;
  END AT3;



(*************************** test locking *********************)

TYPE
  LockedInteger = MUTEX OBJECT
    field: INTEGER;
  END;

  ProcLock = PROCEDURE (x: LockedInteger) : Word.T;

PROCEDURE LockTest () =
  VAR
    testArray := NEW (REF ARRAY OF PROCANY, 3);
    p : PROCANY;
    x : Word.T;

    li := NEW (LockedInteger);
  BEGIN
    testArray[0] := AL1;
    testArray[1] := AL2;
    testArray[2] := AL3;
    
    p := Guard.OptimizeAll (testArray);
    
    li.field := 5;
    x := LOOPHOLE (p, ProcLock) (li);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");

    li.field := 77;
    x := LOOPHOLE (p, ProcLock) (li);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");

    li.field := 79;
    x := LOOPHOLE (p, ProcLock) (li);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Unsigned (x) & "\n");

    li.field := 64;
    x := LOOPHOLE (p, ProcLock) (li);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Unsigned (x) & "\n");

    li.field := 1298;
    x := LOOPHOLE (p, ProcLock) (li);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Unsigned (x) & "\n");

  END LockTest;

PROCEDURE AL1 (li: LockedInteger) : BOOLEAN =
  BEGIN
    LOCK li DO
      RETURN li.field = 5;
    END;
  END AL1;

PROCEDURE AL2 (li: LockedInteger) : BOOLEAN =
  BEGIN
    LOCK li DO
      RETURN li.field = 77;
    END;
  END AL2;

PROCEDURE AL3 (li: LockedInteger) : BOOLEAN =
  BEGIN
    LOCK li DO
      RETURN li.field = 79;
    END;
  END AL3;


(****************** alternate test *****************)

PROCEDURE Aalt1 (arg: INTEGER) : BOOLEAN =
  BEGIN
    RETURN arg = 5;
  END Aalt1;

PROCEDURE Aalt2 (arg: INTEGER) : BOOLEAN =
  BEGIN
    RETURN arg # 5;
  END Aalt2;

TYPE
  ProcAlt = PROCEDURE (arg: INTEGER) : Word.T;

PROCEDURE AltTest () =
  VAR
    testArray := NEW (REF ARRAY OF PROCANY, 2);
    p : PROCANY;
    x : Word.T;

  BEGIN
    testArray[0] := Aalt1;
    testArray[1] := Aalt2;
    
    p := Guard.OptimizeAll (testArray);
    
    x := LOOPHOLE (p, ProcAlt) (5);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");

    x := LOOPHOLE (p, ProcAlt) (6);
    Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & "\n");
  END AltTest;


BEGIN
END RCGTest.


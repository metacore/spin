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

UNSAFE MODULE DispType;
IMPORT IO, Sirpa, Dispatcher, Thread;

(*
 * guards and handlers
 *)

FUNCTIONAL PROCEDURE G1 (a1: Sirpa.RRT1; 
              <* UNUSED *> a2: Sirpa.RRT2): BOOLEAN =
  BEGIN
    RETURN a1 = NIL;
  END G1;

FUNCTIONAL PROCEDURE G2 (a1: Sirpa.RRT1; 
              <* UNUSED *> a2: REF Sirpa.RRT2): BOOLEAN =
  BEGIN
    RETURN a1 = NIL;
  END G2;

FUNCTIONAL PROCEDURE G3 (a1: Sirpa.RRT1; 
              <* UNUSED *> VAR a2: Sirpa.RRT2): BOOLEAN =
  BEGIN
    RETURN a1 = NIL;
  END G3;

FUNCTIONAL PROCEDURE G4 (a1: Sirpa.RRT1; 
              <* UNUSED *> READONLY a2:  Sirpa.RRT2): BOOLEAN =
  BEGIN
    RETURN a1 = NIL;
  END G4;

PROCEDURE H1 (<* UNUSED *> a1: Sirpa.RRT1; 
              <* UNUSED *> a2: Sirpa.RRT2) : Sirpa.RRT1 =
  BEGIN
    RETURN NIL;
  END H1;

PROCEDURE H2 (<* UNUSED *> a1: Sirpa.RRT1; 
              <* UNUSED *> a2: REF Sirpa.RRT2) : Sirpa.RRT1 =
  BEGIN
    RETURN NIL;
  END H2;

PROCEDURE H3 (<* UNUSED *> a1: Sirpa.RRT1; 
              <* UNUSED *> VAR a2: Sirpa.RRT2) : Sirpa.RRT1 =
  BEGIN
    RETURN NIL;
  END H3;

PROCEDURE H4 (<* UNUSED *> a1: Sirpa.RRT1; 
              <* UNUSED *> READONLY a2: Sirpa.RRT2) : Sirpa.RRT1 =
  BEGIN
    RETURN NIL;
  END H4;

PROCEDURE H5 (<* UNUSED *> a1: Sirpa.RRT1; 
              <* UNUSED *> a2: Sirpa.RRT2;
              <* UNUSED *> a3: Sirpa.RRT2) : Sirpa.RRT1 =
  BEGIN
    RETURN NIL;
  END H5;

FUNCTIONAL PROCEDURE GC1 (<* UNUSED *> closure: REFANY;
               a1: Sirpa.RRT1; 
               <* UNUSED *> a2: Sirpa.RRT2): BOOLEAN =
  BEGIN
    RETURN a1 = NIL;
  END GC1;

FUNCTIONAL PROCEDURE GC2 (<* UNUSED *> closure: MUTEX;
               a1: Sirpa.RRT1; 
               <* UNUSED *> a2: Sirpa.RRT2): BOOLEAN =
  BEGIN
    RETURN a1 = NIL;
  END GC2;

PROCEDURE HC1 (<* UNUSED *> closure: REFANY;
               <* UNUSED *> a1: Sirpa.RRT1; 
               <* UNUSED *> a2: Sirpa.RRT2) : Sirpa.RRT1 =
  BEGIN
    RETURN NIL;
  END HC1;

PROCEDURE HC2 (<* UNUSED *> closure: MUTEX;
               <* UNUSED *> a1: Sirpa.RRT1; 
               <* UNUSED *> a2: Sirpa.RRT2) : Sirpa.RRT1 =
  BEGIN
    RETURN NIL;
  END HC2;

FUNCTIONAL PROCEDURE GX(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END GX;

FUNCTIONAL PROCEDURE GY(c: REFANY): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END GY;

(*
 * objects
 *)

TYPE
  T = MUTEX OBJECT
    x: INTEGER
  END;

VAR closure1 := NEW(MUTEX);
VAR closure2 := NEW(T);                (* subtype of MUTEX *)
VAR closure3 := NEW(Thread.Condition); (* not a subtype of MUTEX *)

(*
 * run a test
 *)

PROCEDURE Test (event, guard, handler: PROCANY; 
                expected: BOOLEAN;
                guardClosure: REFANY := NIL;
                handlerClosure: REFANY := NIL): BOOLEAN =
  VAR
    ok, failed: BOOLEAN;
    binding: Dispatcher.Binding;
  BEGIN
    ok := TRUE;
    failed := FALSE;

    TRY
      binding := Dispatcher.InstallHandler(event, guard, handler,
                                           guardClosure, handlerClosure);
    EXCEPT
    | Dispatcher.Error(code) => 
      CASE code OF 
      | Dispatcher.ErrorCode.TypeError =>
        IO.Put("type error ");
        ok := FALSE;
      ELSE
        IO.Put("other dispatcher error ");
        ok := FALSE;
        failed := TRUE;
      END;
    END;

    IF ok AND NOT failed THEN
      Dispatcher.Uninstall(binding);
    END;

    IF ok = expected AND NOT failed THEN
      IO.Put("=> OK\n");
      RETURN TRUE;
    ELSE
      IO.Put("=> ERROR\n");
      RETURN FALSE;
    END;
  END Test;

(*
 * try the various installations
 *)

(* 
 * correct simple installations 
 *)

PROCEDURE SimpleCorrect1 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation: ");
    RETURN Test(Sirpa.Qpa1, G1, H1, TRUE);
  END SimpleCorrect1;

PROCEDURE SimpleCorrect2 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation (REF): ");
    RETURN Test(Sirpa.Qpa2, G2, H2, TRUE);
  END SimpleCorrect2;

PROCEDURE SimpleCorrect3 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation (VAR): ");
    RETURN Test(Sirpa.Qpa3, G3, H3, TRUE);
  END SimpleCorrect3;

PROCEDURE SimpleCorrect4 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation (READONLY): ");
    RETURN Test(Sirpa.Qpa4, G4, H4, TRUE);
  END SimpleCorrect4;

PROCEDURE SimpleCorrect5 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation of NIL guard: ");
    RETURN Test(Sirpa.Qpa1, NIL, H1, TRUE);
  END SimpleCorrect5;

(*
 *  correct installations of closures 
 *)

PROCEDURE ClosureCorrect1 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation of 2 closures (NIL closures): ");
    RETURN Test(Sirpa.Qpa1, GC1, HC1, TRUE, NIL, NIL);
  END ClosureCorrect1;

PROCEDURE ClosureCorrect2 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation of 2 closures (NIL guard closure): ");
    RETURN Test(Sirpa.Qpa1, GC1, HC1, TRUE, closure1, NIL);
  END ClosureCorrect2;

PROCEDURE ClosureCorrect3 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation of 2 closures (NIL handler closure): ");
    RETURN Test(Sirpa.Qpa1, GC1, HC1, TRUE, NIL, closure1);
  END ClosureCorrect3;

PROCEDURE ClosureCorrect4 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation of 2 closures (two closures): ");
    RETURN Test(Sirpa.Qpa1, GC1, HC1, TRUE, closure1, closure1);
  END ClosureCorrect4;

PROCEDURE ClosureCorrect5 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation of closures which are subtypes: ");
    RETURN Test(Sirpa.Qpa1, GC2, HC2, TRUE, closure2, closure2);
  END ClosureCorrect5;

PROCEDURE ClosureCorrect6 (): BOOLEAN =
  BEGIN
    IO.Put("correct installation of 1 closure (handler only): ");
    RETURN Test(Sirpa.Qpa1, G1, HC1, TRUE, NIL, closure1);
  END ClosureCorrect6;

(*
 * argument passing mode mismatches 
 *)

PROCEDURE ArgTypes1 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of a handler (REF argument): ");
    RETURN Test(Sirpa.Qpa1, G1, H2, FALSE);
  END ArgTypes1;

PROCEDURE ArgTypes2 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of a handler (VAR argument): ");
    RETURN Test(Sirpa.Qpa1, G1, H3, FALSE);
  END ArgTypes2;

PROCEDURE ArgTypes3 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of a handler (READONLY argument): ");
    RETURN Test(Sirpa.Qpa1, G1, H4, FALSE);
  END ArgTypes3;

PROCEDURE ArgTypes4 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of a handler (READONLY instead of VAR): ");
    RETURN Test(Sirpa.Qpa3, G3, H4, FALSE);
  END ArgTypes4;

PROCEDURE ArgTypes5 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of a handler (VAR instead of REF): ");
    RETURN Test(Sirpa.Qpa2, G2, H3, FALSE);
  END ArgTypes5;

(* 
 * type mismatches 
 *)

PROCEDURE SimpleIncorrect1 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of a handler (completely different): ");
    RETURN Test(Sirpa.Qpa1, G1, Test, FALSE);
  END SimpleIncorrect1;

PROCEDURE SimpleIncorrect2 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of a handler (extra argument): ");
    RETURN Test(Sirpa.Qpa1, G1, H5, FALSE);
  END SimpleIncorrect2;

PROCEDURE SimpleIncorrect3 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of guard: ");
    RETURN Test(Sirpa.Qpa1, Test, H1, FALSE);
  END SimpleIncorrect3;

PROCEDURE SimpleIncorrect4 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of guard (not BOOLEAN result): ");
    RETURN Test(Sirpa.Qpa1, H1, H1, FALSE);
  END SimpleIncorrect4;

PROCEDURE SimpleIncorrect5 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of return result: ");
    RETURN Test(Sirpa.Qpa5, G1, H1, FALSE);
  END SimpleIncorrect5;

(* 
 * incorrect installation of closures 
 *)

PROCEDURE ClosureIncorrect1 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect installation of 1 closure (guard only): ");
    RETURN Test(Sirpa.Qpa1, GC1, H1, FALSE, NIL, closure1);
  END ClosureIncorrect1;

PROCEDURE ClosureIncorrect2 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of guard closure: ");
    RETURN Test(Sirpa.Qpa1, GC2, H2, FALSE, closure1, NIL);
  END ClosureIncorrect2;

PROCEDURE ClosureIncorrect3 (): BOOLEAN =
  BEGIN
    IO.Put("incorrect type of handler closure: ");
    RETURN Test(Sirpa.Qpa1, GC2, H2, FALSE, NIL, closure3);
  END ClosureIncorrect3;

(*
 * no args
 *)

PROCEDURE NoArgs1 (): BOOLEAN =
  BEGIN
    IO.Put("no args guard: ");
    RETURN Test(Sirpa.Qpa1, GX, H1, TRUE, NIL, NIL);
  END NoArgs1; 

PROCEDURE NoArgs2 (): BOOLEAN =
  BEGIN
    IO.Put("no args guard: ");
    RETURN Test(Sirpa.Qpa1, GY, H1, TRUE, closure1, NIL);
  END NoArgs2;

(*
 * asynchronous events
 *)

PROCEDURE Asynch1 (): BOOLEAN =
  VAR
    ok: BOOLEAN;
    ref: PROCANY;
    alias: PROCEDURE (a: Sirpa.RRT1; b: Sirpa.RRT2);
  BEGIN
    ok := TRUE;
    IO.Put("correct type of asynch event: ");
    TRY 
      ref := Dispatcher.GetAsynchAlias(Sirpa.Qpa6);
      (* alias := NARROW(ref, REF PROCEDURE (a: Sirpa.RRT1; b: Sirpa.RRT2))^;*)
      alias := ref;
    EXCEPT
    | Dispatcher.Error(code) =>
      CASE code OF
      | Dispatcher.ErrorCode.IllegalAsynchEvent =>
        IO.Put("asynch type error ");
        ok := FALSE;
      ELSE
        IO.Put("other dispatcher error ");
        ok := FALSE;
      END;
    END;

    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
    END;

    RETURN ok;
  END Asynch1;

PROCEDURE Asynch2 (): BOOLEAN =
  VAR
    ok: BOOLEAN := FALSE;
  BEGIN
    IO.Put("event returns result: ");
    TRY 
      EVAL Dispatcher.GetAsynchAlias(Sirpa.Qpa1);
    EXCEPT
    | Dispatcher.Error(code) =>
      CASE code OF
      | Dispatcher.ErrorCode.IllegalAsynchEvent =>
        IO.Put("asynch type error ");
        ok := TRUE;
      ELSE
        IO.Put("other dispatcher error ");
      END;
    END;

    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
    END;

    RETURN ok;
  END Asynch2;

PROCEDURE Asynch3 (): BOOLEAN =
  VAR
    ok: BOOLEAN := FALSE;
  BEGIN
    IO.Put("event takes VAR argument: ");
    TRY 
      EVAL Dispatcher.GetAsynchAlias(Sirpa.Qpa3);
    EXCEPT
    | Dispatcher.Error(code) =>
      CASE code OF
      | Dispatcher.ErrorCode.IllegalAsynchEvent =>
        IO.Put("asynch type error ");
        ok := TRUE;
      ELSE
        IO.Put("other dispatcher error ");
      END;
    END;

    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
    END;

    RETURN ok;
  END Asynch3;

PROCEDURE Asynch4 (): BOOLEAN =
  VAR
    ok: BOOLEAN := FALSE;
  BEGIN
    IO.Put("event takes READONLY argument: ");
    TRY 
      EVAL Dispatcher.GetAsynchAlias(Sirpa.Qpa4);
    EXCEPT
    | Dispatcher.Error(code) =>
      CASE code OF
      | Dispatcher.ErrorCode.IllegalAsynchEvent =>
        IO.Put("asynch type error ");
        ok := TRUE;
      ELSE
        IO.Put("other dispatcher error ");
      END;
    END;

    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
    END;

    RETURN ok;
  END Asynch4;

(*
 *
 *)

PROCEDURE Start(<* UNUSED *>i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END DispType.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

MODULE Dyn;
IMPORT IO, Fmt, DynTypes;

REVEAL
  OT1 = T BRANDED OBJECT
  OVERRIDES
    m1 := M1;
    m2 := M2;
    m3 := M3;
  END;

PROCEDURE NarrowTest(): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
    ok: BOOLEAN;
    x: REFANY;
    o1: DynTypes.OT1;
    o2: OT1;
  BEGIN
    ok := TRUE;
    TRY
      IO.Put ("NARROW (should succeed): ");
      x := NEW(OT1);
      o2 := NARROW(x, DynTypes.OT1);
    EXCEPT
    ELSE
      IO.Put("exception ");
      ok := FALSE;
    END;
    IF ok THEN
      IO.Put(" => OK\n");
    ELSE
      IO.Put(" => ERROR\n");
      result := FALSE;
    END;

    ok := FALSE;
    TRY
      IO.Put ("NARROW (should fail): ");
      x := NEW(DynTypes.OT1);
      o1 := NARROW(x, OT1);
    EXCEPT
    ELSE
      IO.Put("exception ");
      ok := TRUE;
    END;
    IF ok THEN
      IO.Put(" => OK\n");
    ELSE
      IO.Put(" => ERROR\n");
      result := FALSE;
    END;

    RETURN result;
  END NarrowTest;

PROCEDURE TypecaseTest(): BOOLEAN =
  VAR
    ok : BOOLEAN;
    result: BOOLEAN := TRUE;
    x: REFANY;
  BEGIN
    (* NIL should be caught by the first case no matter what type *)
    ok := TRUE;
    IO.Put("N|L - ");
    x := NIL;
    TYPECASE x OF
    | DynTypes.OT1 => IO.Put("DynTypes.OT1"); 
    | DynTypes.T => IO.Put("DynTypes.T"); 
    | OT1 => IO.Put("OT1"); ok := FALSE;
    | T => IO.Put("T"); ok := FALSE;
    | NULL => IO.Put("NULL"); ok := FALSE;
    ELSE IO.Put("unknown"); ok := FALSE;
    END;
    IF ok THEN
      IO.Put(" => OK\n");
    ELSE
      IO.Put(" => ERROR\n");
      result := FALSE;
    END;

    (* does the first line catch *)
    ok := TRUE;
    IO.Put("DynTypes.OT1 - ");
    x := NEW(DynTypes.OT1);
    TYPECASE x OF
    | DynTypes.OT1 => IO.Put("DynTypes.OT1");
    | DynTypes.T => IO.Put("DynTypes.T"); ok := FALSE;
    | OT1 => IO.Put("OT1"); ok := FALSE;
    | T => IO.Put("T"); ok := FALSE;
    | NULL => IO.Put("NULL"); ok := FALSE;
    ELSE IO.Put("unknown"); ok := FALSE;
    END;
    IF ok THEN
      IO.Put(" => OK\n");
    ELSE
      IO.Put(" => ERROR\n");
      result := FALSE;
    END;

    (* does the second line catch *)
    ok := TRUE;
    IO.Put("DynTypes.T - ");
    x := NEW(DynTypes.T);
    TYPECASE x OF
    | DynTypes.OT1 => IO.Put("DynTypes.OT1"); ok := FALSE;
    | DynTypes.T => IO.Put("DynTypes.T");
    | OT1 => IO.Put("OT1"); ok := FALSE;
    | T => IO.Put("T"); ok := FALSE;
    | NULL => IO.Put("NULL"); ok := FALSE;
    ELSE IO.Put("unknown"); ok := FALSE;
    END;
    IF ok THEN
      IO.Put(" => OK\n");
    ELSE
      IO.Put(" => ERROR\n");
      result := FALSE;
    END;

    (* do the subtypes work *)
    ok := TRUE;
    IO.Put("T - ");
    x := NEW(T);
    TYPECASE x OF
    | DynTypes.OT1 => IO.Put("DynTypes.OT1"); 
    | DynTypes.T => IO.Put("DynTypes.T"); ok := FALSE;
    | OT1 => IO.Put("OT1"); ok := FALSE;
    | T => IO.Put("T"); ok := FALSE;
    | NULL => IO.Put("NULL"); ok := FALSE;
    ELSE IO.Put("unknown"); ok := FALSE;
    END;
    IF ok THEN
      IO.Put(" => OK\n");
    ELSE
      IO.Put(" => ERROR\n");
      result := FALSE;
    END;

    (* does the ELSE work *)
    ok := TRUE;
    IO.Put("REF ARRAY - ");
    x := NEW(REF ARRAY OF INTEGER, 10);
    TYPECASE x OF
    | DynTypes.OT1 => IO.Put("DynTypes.OT1"); ok := FALSE;
    | DynTypes.T => IO.Put("DynTypes.T"); ok := FALSE;
    | OT1 => IO.Put("OT1"); ok := FALSE;
    | T => IO.Put("T"); ok := FALSE;
    | NULL => IO.Put("NULL"); ok := FALSE;
    ELSE IO.Put("unknown");
    END;
    IF ok THEN
      IO.Put(" => OK\n");
    ELSE
      IO.Put(" => ERROR\n");
      result := FALSE;
    END;
    
    RETURN result;
  END TypecaseTest; 

PROCEDURE M1(self: OT1) =
  BEGIN
    IO.Put ("Dyn.M1: " & Fmt.Int(self.f1) & 
                " " & Fmt.Int(self.f2) & "\n");
  END M1;

PROCEDURE M2(self: OT1; i: INTEGER): INTEGER =
  VAR 
    k := self.f1;
  BEGIN
    IO.Put ("Dyn.M2: " & Fmt.Int(self.f1) & " " & Fmt.Int(self.f2) & 
                " " & Fmt.Int(i) & "\n");
    self.f1 := i;
    RETURN k;
  END M2;

PROCEDURE M3(self: OT1; i: INTEGER): INTEGER =
  VAR 
    k := self.f2;
  BEGIN
    IO.Put ("Dyn.M3: " & Fmt.Int(self.f1) & " " & Fmt.Int(self.f2) & 
                " " & Fmt.Int(i) & "\n");
    self.f2 := i;
    RETURN k;
  END M3;

BEGIN
END Dyn.



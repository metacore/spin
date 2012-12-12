(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)

GENERIC MODULE DynamicArray (Elem, Sorter);

IMPORT Wr, Stdio, Thread;

<* FATAL Thread.Alerted, Wr.Failure *>

TYPE
  RAE = REF ARRAY OF Elem.T;

REVEAL T = BRANDED REF RECORD
  count: CARDINAL := 0;
  array: RAE;
END;

PROCEDURE New () : T =
  VAR n := NEW (T);
  BEGIN
    n.count := 0;
    n.array := NEW (RAE, 10);
    RETURN n;
  END New;

PROCEDURE Put (t: T; i: CARDINAL; e: Elem.T) =
  BEGIN
    <* ASSERT i < t.count *>
    t.array[i] := e;
  END Put;

PROCEDURE Get (t: T; i: CARDINAL) : Elem.T =
  BEGIN
    <* ASSERT i < t.count *>
    RETURN t.array[i];
  END Get;

PROCEDURE Size (t: T) : CARDINAL =
  BEGIN
    RETURN t.count;
  END Size;

PROCEDURE Add (t: T; e: Elem.T) =
  VAR
    new: RAE;
  BEGIN
    IF t.count > LAST (t.array^) THEN
      new := NEW (RAE, 2*t.count);
      SUBARRAY (new^, 0, t.count) := t.array^;
      t.array := new;
    END;

    t.array[t.count] := e;
    INC (t.count);
  END Add;

PROCEDURE Sort (t: T; compare: PROCEDURE (a, b: Elem.T) : [-1 .. 1]) =
  BEGIN
    Sorter.Sort (SUBARRAY (t.array^, 0, t.count-1), compare);
  END Sort;

PROCEDURE Print (t: T; s: Wr.T := NIL) =
  BEGIN
    IF s = NIL THEN s := Stdio.stdout; END;

    Wr.PutText (s, "Printing dynamic array:\n");
    FOR i := 0 TO t.count-1 DO
      Elem.Print (t.array[i], s);
      Wr.PutText (s, "\n");
    END;
    Wr.PutText (s, "Done printing dynamic array\n");
  END Print;

BEGIN
END DynamicArray.

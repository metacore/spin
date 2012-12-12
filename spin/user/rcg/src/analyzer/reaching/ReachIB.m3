(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)

MODULE ReachIB;

IMPORT Wr, Stdio, Fmt, Thread;

<* FATAL Thread.Alerted, Wr.Failure *>

PROCEDURE Compare (<* UNUSED *> a: T; <*UNUSED *> b: T) : [-1 .. 1] =
  BEGIN
    (* should never be called *)
    <* ASSERT FALSE *>
  END Compare;

PROCEDURE Print (t: T; s: Wr.T := NIL) =
  BEGIN
    IF s = NIL THEN s := Stdio.stdout; END;
    Wr.PutText (s, "RIB: " & Fmt.Int (t.inst) & " " & Fmt.Int (t.bb.index));
  END Print;

BEGIN
END ReachIB.

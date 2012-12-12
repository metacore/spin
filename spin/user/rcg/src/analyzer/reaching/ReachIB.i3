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

INTERFACE ReachIB;

IMPORT Wr, BasicBlock;

TYPE
  (* types for computing reaching results *)
  T = RECORD inst: CARDINAL; bb: BasicBlock.T; END;

PROCEDURE Compare (a, b: T) : [-1 .. 1];

PROCEDURE Print (t: T; s: Wr.T := NIL);

END ReachIB.

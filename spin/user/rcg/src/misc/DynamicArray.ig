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

GENERIC INTERFACE DynamicArray (Elem);

IMPORT Wr;

TYPE T <: REFANY;

PROCEDURE New () : T;

PROCEDURE Put (t: T; i: CARDINAL; e: Elem.T);
PROCEDURE Get (t: T; i: CARDINAL) : Elem.T;

PROCEDURE Size (t: T) : CARDINAL;

(* modifies t *)
PROCEDURE Add (t: T; e: Elem.T);

(* modifies t *)
PROCEDURE Sort (t: T; compare: PROCEDURE (a, b: Elem.T) : [-1 .. 1]);

PROCEDURE Print (t: T; s: Wr.T := NIL);

END DynamicArray.

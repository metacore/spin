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

GENERIC INTERFACE DynamicSet (Elem);

IMPORT Word, Wr;

TYPE
  InitT = RECORD
    field : Word.T := 0;
  END;

  T = REF ARRAY OF InitT;

PROCEDURE New (n: CARDINAL): T;
PROCEDURE NewComplete (n: CARDINAL): T;

PROCEDURE Union (a, b: T): T;
PROCEDURE Intersection (a, b: T): T;
PROCEDURE Diff (a, b: T): T;         (* a MINUS b *)
PROCEDURE SymDiff (a, b: T): T;

PROCEDURE IsEqual (a, b: T): BOOLEAN;
PROCEDURE IsSubset (a, b: T): BOOLEAN;
PROCEDURE IsProperSubset (a, b: T): BOOLEAN;
PROCEDURE IsMember (e: Elem.T; s: T): BOOLEAN;
PROCEDURE IsEmpty (a: T) : BOOLEAN;

(* mutates set argument *)
PROCEDURE Add (e: Elem.T; s: T);
PROCEDURE Remove (e: Elem.T; s: T);
PROCEDURE UnionIn (a, b: T);  (* modifies a *)
PROCEDURE Clear (s: T);

PROCEDURE Copy (s: T) : T;
 
PROCEDURE Print (s: T; ss : Wr.T := NIL);

END DynamicSet.

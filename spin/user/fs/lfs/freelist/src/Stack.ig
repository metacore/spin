(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)
GENERIC INTERFACE Stack(Elem);
	(* where Elem.T is not an open array type. *)
	TYPE T <: REFANY;
	PROCEDURE Create(): T;
	PROCEDURE Push(VAR s: T; x: Elem.T);
	PROCEDURE Pop(VAR s: T):Elem.T;
END Stack.

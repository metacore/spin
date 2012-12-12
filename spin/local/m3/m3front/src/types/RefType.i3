(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RefType.i3                                            *)
(* Last Modified On Mon Jul 25 08:37:37 PDT 1994 By kalsow     *)

(*
 * HISTORY
 * 01-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added ref counting code
 *
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added name argument to New
 *
 *)

INTERFACE RefType;

IMPORT Type, Expr, M3String;

PROCEDURE Parse (): Type.T;

PROCEDURE ParseBrand (): Expr.T;

PROCEDURE New (target: Type.T;  traced: BOOLEAN;  brand: Expr.T;
               name: TEXT := NIL): Type.T;

PROCEDURE Is (t: Type.T): BOOLEAN;

PROCEDURE IsBranded (t: Type.T): BOOLEAN;

PROCEDURE Split (t: Type.T;  VAR target: Type.T): BOOLEAN;

PROCEDURE NoteBrand (t: Type.T;  b: M3String.T);

PROCEDURE NoteRefName (t: Type.T;  name: TEXT);
(* record a user name for the ref type 't' *)

PROCEDURE InitTypecell (t: Type.T;  offset, prev: INTEGER);

PROCEDURE Reset ();

PROCEDURE GenRC (t: Type.T; definitelyGlobal: BOOLEAN; noOverlap := FALSE);
(* reference counting *)

END RefType.

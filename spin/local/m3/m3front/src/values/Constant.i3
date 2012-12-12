(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Constant.i3                                           *)
(* Last Modified On Tue Dec 20 14:57:43 PST 1994 By kalsow     *)
(*      Modified On Thu Dec 13 01:35:56 1990 By muller         *)

(*
 * HISTORY
 * 13-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added Constant.Split in order to support checking if
 *       an expression is NIL
 *      added for CVAR support
 *
 *)

INTERFACE Constant;

IMPORT Expr, Decl, Value;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes);

PROCEDURE Declare (name: TEXT;  value: Expr.T;  reserved: BOOLEAN);

PROCEDURE Split (v: Value.T; VAR value: Expr.T): BOOLEAN;

END Constant.

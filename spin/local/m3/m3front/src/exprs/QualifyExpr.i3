(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: QualifyExpr.i3                                        *)
(* Last Modified On Fri Jun 24 08:57:39 PDT 1994 By kalsow     *)
(*      Modified On Sat Aug 18 01:13:38 1990 By muller         *)

(*
 * HISTORY
 * 05-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for restricting TYPE.METHOD
 *
 *)

INTERFACE QualifyExpr;

IMPORT M3ID, Expr, Value, Type;

PROCEDURE New (a: Expr.T;  id: M3ID.T): Expr.T;

PROCEDURE Split (e: Expr.T; VAR v: Value.T): BOOLEAN;

PROCEDURE SplitQID (e: Expr.T; VAR module, item: M3ID.T): BOOLEAN;

PROCEDURE PassObject (e: Expr.T): BOOLEAN;

PROCEDURE MethodType (e: Expr.T): Type.T;

PROCEDURE IsExplicitMethod (e: Expr.T; VAR t: Type.T): BOOLEAN;

END QualifyExpr.

(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CastExpr.i3                                           *)
(* Last Modified On Thu Apr 30 13:24:03 PDT 1992 By kalsow     *)

(*
 * HISTORY
 * 31-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	most of the code copied from CastExpr.m3
 *
 *)

INTERFACE ViewExpr;

IMPORT Expr, Type;

PROCEDURE New (a: Expr.T;  t: Type.T): Expr.T;

END ViewExpr.

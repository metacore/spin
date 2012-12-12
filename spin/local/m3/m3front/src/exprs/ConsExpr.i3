(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ConsExpr.i3                                           *)
(* Last Modified On Fri Jan  5 17:53:17 1990 By kalsow         *)

INTERFACE ConsExpr;

IMPORT Expr;

PROCEDURE New (type: Expr.T;  args: Expr.List;  dots: BOOLEAN): Expr.T;

END ConsExpr.

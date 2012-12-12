(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: DerefExpr.i3                                          *)
(* Last Modified On Thu Jun 28 03:43:00 1990 By muller         *)
(*      Modified On Wed Jul 26 08:50:54 1989 By kalsow         *)

(* HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Added Is predicate.
 *
 *)

INTERFACE DerefExpr;

IMPORT Expr;

PROCEDURE New (a: Expr.T): Expr.T;

PROCEDURE Is (e: Expr.T): BOOLEAN;
PROCEDURE SetOffset (e: Expr.T; n: INTEGER);

END DerefExpr.

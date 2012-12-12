(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Subarray.i3                                           *)
(* Last Modified On Fri Jun 30 08:48:23 1989 By kalsow         *)

(*
 * HISTORY
 * 21-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	move type Z out to interface, need it in CallExpr.IsGlobal
 *
 *)

INTERFACE Subarray;

IMPORT CallExpr;

VAR Z: CallExpr.MethodList;

PROCEDURE Initialize ();

END Subarray.

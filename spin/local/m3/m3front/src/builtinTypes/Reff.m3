(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Reff.m3                                               *)
(* Last Modified On Fri Jun 24 09:44:39 PDT 1994 By kalsow     *)
(*      Modified On Thu Nov  2 21:29:23 1989 By muller         *)


(*
 * HISTORY
 * 15-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY moved to Procany module
 *
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *            call RefType.New with type name
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	add PROCANY as an alias for REFANY
 *
 *)


MODULE Reff;

IMPORT RefType, Tipe, M3String, TextExpr;

PROCEDURE Initialize () =
  BEGIN
    T := RefType.New (NIL, TRUE, TextExpr.New (M3String.Add ("$refany$")),
                      name := "REFANY");
    Tipe.Define ("REFANY", T, TRUE);
  END Initialize;

BEGIN
END Reff.

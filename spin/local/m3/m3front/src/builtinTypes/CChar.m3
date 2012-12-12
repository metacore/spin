(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CChar.m3                                              *)
(* Last Modified On Fri Jun 24 09:43:18 PDT 1994 By kalsow     *)
(*      Modified On Fri Dec 21 01:27:12 1990 By muller         *)

(*
 * HISTORY
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *      call EnumType.New with type name
 *
 *)

MODULE CChar;

IMPORT M3, M3ID, EnumType, Tipe, Scope;

PROCEDURE Initialize () =
  VAR elts: Scope.T;  cs := M3.OuterCheckState;
  BEGIN
    elts := Scope.PushNew (FALSE, M3ID.Add ("CHAR"));
    T := EnumType.New (256, elts, name := "CHAR");
    Scope.PopNew ();
    Scope.TypeCheck (elts, cs);
    Tipe.Define ("CHAR", T, TRUE);
  END Initialize;

BEGIN
END CChar.

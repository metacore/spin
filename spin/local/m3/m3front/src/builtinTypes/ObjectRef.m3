(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ObjectRef.m3                                          *)
(* Last Modified On Fri Jun 24 09:44:26 PDT 1994 By kalsow     *)
(*      Modified On Fri May 11 11:49:51 1990 By muller         *)

(*
 * HISTORY
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *      call ObjectType.New with type name
 *
 *)

MODULE ObjectRef;

IMPORT M3ID, ObjectType, Tipe, Scope;

PROCEDURE Initialize () =
  VAR s: Scope.T;
  BEGIN
    s := Scope.PushNew (FALSE, M3ID.NoID);
    Scope.PopNew ();
    T := NIL;   (* the value is used by ObjectType.New ! *)
    T := ObjectType.New (NIL, TRUE, NIL, s, s, name := "ROOT");
    Tipe.Define ("ROOT", T, TRUE);
  END Initialize;

BEGIN
END ObjectRef.

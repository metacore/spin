(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ObjectAdr.m3                                          *)
(* Last Modified On Fri Jun 24 09:44:06 PDT 1994 By kalsow     *)
(*      Modified On Tue Nov 20 20:35:05 1990 By muller         *)

(*
 * HISTORY
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *      call ObjectType.New with type name
 *
 *)


MODULE ObjectAdr;

IMPORT M3ID, ObjectType, Tipe, Scope;

PROCEDURE Initialize () =
  VAR s: Scope.T;
  BEGIN
    s := Scope.PushNew (FALSE, M3ID.NoID);
    Scope.PopNew ();
    T := NIL;   (* the value is used by ObjectType.New ! *)
    T := ObjectType.New (NIL, FALSE, NIL, s, s, name := "UNTRACED ROOT");
    Tipe.Define ("_UNTRACED_ROOT", T, FALSE);
  END Initialize;

BEGIN
END ObjectAdr.

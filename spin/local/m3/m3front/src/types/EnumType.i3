(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Enum.i3                                               *)
(* Last Modified On Fri Jun 24 09:25:11 PDT 1994 By kalsow     *)
(*      Modified On Fri Feb  9 07:38:15 1990 By muller         *)

(*
 * HISTORY
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *      added name argument to New
 *
 *)

INTERFACE EnumType;

IMPORT M3ID, Type, Scope, Value;

PROCEDURE Parse (): Type.T;

PROCEDURE New (nElts: INTEGER;  elts: Scope.T; name: TEXT := NIL): Type.T;

PROCEDURE Is (t: Type.T): BOOLEAN;

PROCEDURE LookUp (t: Type.T;  name: M3ID.T;  VAR value: Value.T): BOOLEAN;

PROCEDURE NumElts (t: Type.T): INTEGER;

END EnumType.

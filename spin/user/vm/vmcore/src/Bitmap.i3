(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Bitmap provides functionality similar to
 
   "REF ARRAY OF BITS 1 FOR BOOLEAN;"

 This module is here because m3 really doesn't allow above declaration.

 *)

INTERFACE Bitmap;

TYPE
  T <: REFANY;

PROCEDURE Create(size: CARDINAL): T;
  
PROCEDURE Get(t: T; i: CARDINAL): BOOLEAN;
  (* Get the value of "i"th element. *)
  
PROCEDURE Set(t: T; i: CARDINAL);
  (* Set TRUE to the "i"th element. *)
  
PROCEDURE Reset(t: T; i: CARDINAL);
  (* Set FALSE to the "i"th element. *)
  
PROCEDURE FindTrue(t: T; VAR i: CARDINAL): BOOLEAN;
  (* Find first TRUE after(excluding) "i".
   Returns TRUE if found. *)
  
PROCEDURE FindFalse(t: T; VAR i: CARDINAL): BOOLEAN;
  (* Find first FALSE after(excluding) "i".
   Returns TRUE if found. *)
  
END Bitmap.

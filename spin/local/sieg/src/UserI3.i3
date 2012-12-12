(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* Outputs user-side  codes. *)

INTERFACE UserI3;
IMPORT Module;
IMPORT Type;

PROCEDURE Output(READONLY m : Module.T);

(* Return the exteral symbol name that implements the client side
 * of the procedure.
 *)
PROCEDURE ExternalName(READONLY intf, name : TEXT; proc : Type.Proc) : TEXT;

END UserI3.

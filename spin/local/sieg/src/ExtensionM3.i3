(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(* Outputs Extension side codes.
 *  They include type pack/unpack procedures, and dispatcher.
 *)

(*
 * HISTORY
 * 07-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)
INTERFACE ExtensionM3;
IMPORT Module;

PROCEDURE Output(READONLY m : Module.T);

END ExtensionM3.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Nov-94  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Interface to the interrupt management code.
 *) 

(* "Core" 

   This is a privileged, private interface by nature.
*)

INTERFACE Core;

PROCEDURE Init(verbose: BOOLEAN);
  (* "Init" redirects traps to go through the SPIN handlers and defines
     a nonpreemptible region for all of the trap handlers in Core.s. *)

END Core.

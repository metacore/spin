(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY 
 * 11-Oct-95  David Becker (becker) at the University of Washington
 *	Created
 *)

(* "SpinStart"

   SpinStart is part of the SPIN M3 initialization.

   SpinStart.Init() is called in RTLinker mainbody _before_ regular
   mainbodies.  Init() is implemented in spin/kernel/start/src/Boot.m3
   which calls the Init() routines of the core SPIN services.

   Initializing the SPIN services early allows regular main bodies
   to use SPIN services.  This allows statically linked code to use
   main bodies just as the dynamic code can.
*)

INTERFACE SpinStart;
PROCEDURE Init();  (* Init() run directly by RTLinker before main body *)
END SpinStart.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 for CVS.  Added CONSTs.
 *
 *) 

(* "Syscall" interface -- dynamically linked extension *)

(* The "Syscall" interface exports the procedure which is installed upon
   the "TrapSyscall.Syscall" event.  Its implementation is the main
   driver for the Unix server syscall extensions. *)

INTERFACE Syscall;
IMPORT Strand, CPU;

CONST
(* Return codes.*)

(* stolen from Mach *)
  KERN_SUCCESS = 16_0;
  KERN_INVALID_ARGUMENT = 16_4;

(* General failure of syscall - not a Mach error code *)
  Failure = -1;

PROCEDURE Syscall(strand: Strand.T; VAR ms: CPU.SavedState);

END Syscall.

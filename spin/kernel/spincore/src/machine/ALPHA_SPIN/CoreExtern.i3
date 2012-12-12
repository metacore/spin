(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *      Added corebegin and coreend symbols which are used to set up
 *      the nonpreemptible code region.
 * 
 * 28-Nov-94  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Interface to the interrupt management code.
 *) 

UNSAFE (* to define externals *)
INTERFACE CoreExtern;
IMPORT CPU;

(* "CoreExtern" defines the interface to the interrupt management
   code.  Misuse of this interface can cause unchecked runtime errors,
   hence it needs to be unsafe. *)

(* SPIN trap handlers for the alpha *)
<* EXTERNAL *> PROCEDURE TRAP_entInt();
<* EXTERNAL *> PROCEDURE TRAP_entArith();
<* EXTERNAL *> PROCEDURE TRAP_entMM();
<* EXTERNAL *> PROCEDURE TRAP_entIF();
<* EXTERNAL *> PROCEDURE TRAP_entUna();
<* EXTERNAL *> PROCEDURE TRAP_entSys();

(* The PAL call to redirect traps. *)
<* EXTERNAL *> PROCEDURE Wrent(proc: PROCEDURE(); s: CPU.TrapType);

(* non-preemptible SPIN core *)
<* EXTERNAL *> PROCEDURE corebegin();
<* EXTERNAL *> PROCEDURE coreend();
(* "corebegin" and "coreend" are not actual procedures.  
 * They are used to demarcate the spin core code region.
 *)

END CoreExtern.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 15-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added UserSpaceThreadDied.
 *	
 * 30-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Moved default system call handlers here.
 *
 * 28-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Broke out machine independent traps here. 
 *
 * 9-Jan-95 Przemek Pardyak (pardy) at the University of Washington
 *	Switched to a single dispatcher exception.
 *
 * 1-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Added dispatcher exceptions to the list of exceptions raised
 *	by procedures that interface clients with handler installation.
 *
 * 27-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Nuked InstallProtectionFaultHandler
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Interface to trap events.
 *)

(* This module defines kernel trap events. *)
 
INTERFACE Trap;
IMPORT Strand, CPU;
IMPORT Translation;

TYPE AuthKey = REF RECORD
  strand: Strand.T;	(* for this strand (all if nil, must be privileged) *)
  minProcID: INTEGER;   (* and this min proc *)	(* XX Why do we care?? *)
  maxProcID : INTEGER;  (* and this max proc *)
END;
(* "AuthKey" is an authorization key passed to when installing a handler on 
   "Syscall" event.
 XXX the name shound be changed.
 *)
TYPE FaultHandlerAuthKey = Translation.T;
  
(*
 * Memory management traps
 *)
CONST
  (* XXX they are Alpha specific values *)
  Execute = -1;
  Read	  = 0;
  Write	  = 1;

PROCEDURE InvalidTranslation(strand: Strand.T;
			     VAR ss: CPU.SavedState;
			     map: Translation.T;
  			     addr: CPU.VirtAddress;
			     type: INTEGER;
			     VAR done : BOOLEAN);
(* Page table entry invalid *)
  
PROCEDURE AccessViolation(strand: Strand.T;
			  VAR ss: CPU.SavedState;
			  map : Translation.T;
  			  addr: CPU.VirtAddress;
			  type: INTEGER;
			  VAR done : BOOLEAN);
(* Access to kernel memory. *)
  
PROCEDURE ProtectionFault(strand: Strand.T;
			  VAR ss: CPU.SavedState;
			  map : Translation.T;
  			  addr: CPU.VirtAddress;
			  type: INTEGER;
			  VAR done : BOOLEAN);
  
(*
 "done" parameter is used to indicate whether the page fault
 handling was successful.

 Each handler should check if "done=TRUE" at the beginning, and
 if so, it should just return. Otherwise, it should go ahead and
 try to page in. If the pagein is successful, "done" should be set "TRUE".

 This way, in the final handler, we can check if the event was successfully
 handled or not, and if not, we can crash the offender.

 You may ask, why don't we just use a guard to
 determine if each handler should be called and use the default
 result to crash the process?
 The answer is that guard is not useful in this situation.
 For example, suppose the handler should be called when a memory
 object is mapped on the fault address. We can't check this condition
 in guard, since the condition may change after the guard finishes
 but before the handler is called. In other words, the problem happens
 because guard and handler are not called atomically.
 
 *)
 
PROCEDURE UnalignedAccess(strand: Strand.T;
			  VAR ss: CPU.SavedState;
			  map : Translation.T;
  			  addr: CPU.VirtAddress);

(*
 * Instruction traps
 *)
PROCEDURE Breakpoint(strand: Strand.T;
                     VAR ss: CPU.SavedState; 
  		     addr: CPU.VirtAddress);

PROCEDURE IllegalInstruction(strand: Strand.T;
                             VAR ss: CPU.SavedState;
  			     addr: CPU.VirtAddress);

PROCEDURE FPDisabled(strand: Strand.T;
                     VAR ss: CPU.SavedState; 
                     addr: CPU.VirtAddress);


(*
 * System calls
 *)
PROCEDURE Syscall(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE UnhandledUserSpaceException(strand: Strand.T;
				      VAR ss: CPU.SavedState);
(* This procedure does nothing. This is called when the user space thread
  "strand" died. System call extensions may hook a handler on this event. *)

END Trap.

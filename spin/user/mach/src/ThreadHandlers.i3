(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 for CVS.
 *
 * 04-Aug-95  David Dion (ddion) at the University of Washington
 *       Created.
 *
 *)

(* "ThreadHandlers" interface -- dynamically linked extension *)

(* The "ThreadHandlers" interface exports the procedures which handle
   system calls related to threads.  Procedures in this module, with
   the exception of Rendezvous, are called from "Syscall", the Unix 
   server extension driver module. *)

INTERFACE ThreadHandlers;
IMPORT CPU, Strand, UserSpaceThread;

(* There are two parameters to each of these procedures.  The first is
   the current strand, and the second is the CPU state.  These are
   the same parameters passed to "TrapSyscall.Syscall", since all 
   argument marshalling is handled independently by each procedure.
   Similarly, return values will come through register v0 in the 
   machine state. *)

PROCEDURE thread_switch(strand: Strand.T; VAR ms: CPU.SavedState);
(* Yield control (currently ignoring scheduling hints). *)

PROCEDURE thread_create(strand: Strand.T; VAR ms: CPU.SavedState);
(* Create a thread to support a cthread. *)

PROCEDURE thread_set_state(strand: Strand.T; VAR ms: CPU.SavedState);
(* Set the thread state. *)

PROCEDURE thread_resume(strand: Strand.T; VAR ms: CPU.SavedState);
(* Resume a thread. *)

PROCEDURE thread_rendezvous(strand: Strand.T; VAR ms: CPU.SavedState);
(* Yield and handoff control -- syscall wrapper around Rendezvous. *)

PROCEDURE Rendezvous(stopSelf: BOOLEAN; 
                     otherUthread: UserSpaceThread.T; 
                     Uthreadself: UserSpaceThread.T);
(* Yield and handoff control.  This procedure appears in the interface
   in case another handler needs to invoke the Rendezvous thread
   utility. *)

END ThreadHandlers.



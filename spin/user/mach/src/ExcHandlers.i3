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
 * 22-Aug-95  David Dion (ddion) at the University of Washington
 *       Created.
 *
 *)

(* "ExcHandlers" interface -- dynamically linked extension *)

(* The "ExcHandlers" interface exports the procedures which handle
   system calls for the OSF/1 server.  

   The Unix syscall extensions currently deal with two fundamental
   types of system calls.  The first kind are system calls which
   originate in the OSF/1 server and are intended for Mach.  These
   calls, all of which have negative numbers, are caught by the
   "Syscall" module and are redirected to other syscall handling
   extensions.  User-level programs make these calls through libmach.

   The second kind of system call originates in application programs
   and is intended for Unix.  These syscalls, all of which have
   positive numbers, are also caught by the "Syscall" module.
   However, they are forwarded to this module, where they are packaged
   up and sent to the OSF/1 server. *)

INTERFACE ExcHandlers;
IMPORT Strand, CPU;

(* There are two parameters to each of these procedures.  The first is
   the current strand, and the second is the machine state.  These are
   the same parameters passed to "TrapSyscall.Syscall", since all
   argument marshalling is handled independently by each procedure.
   Similarly, return values will come through register v0 in the
   machine state. *)

PROCEDURE syscall_handler(strand: Strand.T; VAR ms: CPU.SavedState);
(* Target for libc exceptions to redirect them to server. *)

PROCEDURE exc_task_get_next_exc(strand: Strand.T; 
                                VAR ms: CPU.SavedState);
(* Called by server to retrieve libc exceptions. *)

END ExcHandlers.



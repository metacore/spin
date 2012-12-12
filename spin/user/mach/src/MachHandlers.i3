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
 * 19-Aug-95  David Dion (ddion) at the University of Washington
 *       Created.
 *
 *)

(* "MachHandlers" interface -- dynamically linked extension *)

(* The "MachHandlers" interface exports the procedures which handle
   system calls related to Mach services, such as ports.  Procedures 
   in this module are called from "Syscall", the Unix server extension 
   driver module. *)

INTERFACE MachHandlers;

IMPORT Strand, CPU, Space;

VAR ServerTask: Space.T; (* the server's Space *)

(* There are two parameters to each of these procedures.  The first is
   the current strand, and the second is the CPU state.  These are
   the same parameters passed to "TrapSyscall.Syscall", since all 
   argument marshalling is handled independently by each procedure.
   Similarly, return values will come through register v0 in the 
   machine state. *)

PROCEDURE mach_thread_self(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE mach_task_self(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE mach_host_self(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE host_kernel_version(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE task_info(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE task_create(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE task_terminate(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE task_suspend(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE mach_port_allocate(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE task_get_special_port(strand:Strand.T; VAR ms:CPU.SavedState);

PROCEDURE task_get_master_ports(strand:Strand.T; VAR ms:CPU.SavedState);

PROCEDURE processor_set_default(strand:Strand.T; VAR ms:CPU.SavedState);

PROCEDURE host_processor_set_priv(strand: Strand.T; 
                                  VAR ms: CPU.SavedState);

PROCEDURE host_info(strand: Strand.T; VAR ms: CPU.SavedState);

END MachHandlers.


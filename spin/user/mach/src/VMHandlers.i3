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
 * 27-Jul-95  David Dion (ddion) at the University of Washington
 *       Created.
 *
 *)

(* "VMHandlers" interface -- dynamically linked extension *)

(* The "VMHandlers" interface exports the procedures which handle
   system calls related to virtual memory.  Procedures in this module 
   are called from "Syscall", the Unix server extension driver module. 

   Much of the procedures in this interface may become obsolete once
   the Mach Task extension is implemented. *)

INTERFACE VMHandlers;
IMPORT Strand, CPU;

CONST
  VM_PAGE_SIZE = 8192; (* 8K *)
  VM_INHERIT_COPY = 16_1;
  VM_INHERIT_NONE = 16_0;

(* There are two parameters to each of these procedures.  The first is
   the current strand, and the second is the CPU state.  These are
   the same parameters passed to "TrapSyscall.Syscall", since all 
   argument marshalling is handled independently by each procedure.
   Similarly, return values will come through register v0 in the 
   CPU state. *)

PROCEDURE vm_write(strand: Strand.T; VAR ms: CPU.SavedState);
(* Write data to an address space. *)

PROCEDURE vm_read(strand: Strand.T; VAR ms: CPU.SavedState);
(* Read data from an address space. *)

PROCEDURE vm_inherit(strand: Strand.T; VAR ms: CPU.SavedState);
(* Set inheritance info for a region of an address space.  Not currently
   supported. *)

PROCEDURE vm_region(strand: Strand.T; VAR ms: CPU.SavedState);
(* Return information on a region of virtual memory. This is not 
   entirely implemented. *)

PROCEDURE vm_statistics(strand: Strand.T; VAR ms: CPU.SavedState);
(* Return statistics on the kernel's use of virtual memory.  This is 
   not entirely implemented. *)

PROCEDURE vm_map(strand: Strand.T; VAR ms: CPU.SavedState);
(* Map a memory object to a task's address space. This is not entirely
   implemented. *)

PROCEDURE vm_allocate(strand: Strand.T; VAR ms: CPU.SavedState);
(* Allocate a region of virtual memory. *)

PROCEDURE vm_deallocate(strand: Strand.T; VAR ms: CPU.SavedState);
(* De-allocate a region of virtual memory. *)

PROCEDURE vm_protect(strand: Strand.T; VAR ms: CPU.SavedState);
(* Set the protection for a region of virtual memory. *)

END VMHandlers.



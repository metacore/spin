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
 *)

(* "DeviceHandlers" interface -- dynamically linked extension *)

(* The "DeviceHandlers" interface exports the procedures which handle
   device related system calls.  Procedures in this module are called
   from "Syscall", the Unix server extension driver module. *)

INTERFACE DeviceHandlers;
IMPORT Strand, CPU;

(* There are two parameters to each of these procedures.  The first is
   the current strand, and the second is the machine state.  These are
   the same parameters passed to "TrapSyscall.Syscall", since all 
   argument marshalling is handled independently by each procedure.
   Similarly, return values will come through register v0 in the 
   machine state. *)

PROCEDURE device_read(    strand: Strand.T; 
                      VAR ms:     CPU.SavedState; 
                          inband: BOOLEAN);
(* Read from a device.  Has an extra inband parameter set by the syscall
   driver to indicate whether the original syscall was device_read or
   device_read_inband. *)

PROCEDURE device_write(strand: Strand.T; VAR ms: CPU.SavedState);
(* Write to a device. *)

PROCEDURE device_open(strand: Strand.T; VAR ms: CPU.SavedState);
(* Open a device. *)

PROCEDURE device_close(strand: Strand.T; VAR ms: CPU.SavedState);
(* Close a device. *)

PROCEDURE device_set_status(strand: Strand.T; VAR ms: CPU.SavedState);
(* Set device status. *)

PROCEDURE device_get_status(strand: Strand.T; VAR ms: CPU.SavedState);
(* Get device status. *)

PROCEDURE get_ether_packet(strand: Strand.T; VAR ms: CPU.SavedState);
(* Get an ethernet packet *)

END DeviceHandlers.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 * html
 *)

INTERFACE PhysAddrPrivate;
IMPORT PhysAddr;
IMPORT VMError;

PROCEDURE AllocatePinnedPage(): PhysAddr.T RAISES {VMError.E};
(* Allocate a page that will never be chosen as a victim. *)

PROCEDURE AllocateIOSpace(phys, physEnd : PhysAddr.Address): PhysAddr.T;
(* Allocate a frame that is tied to a particular phys addr region. *)

PROCEDURE ChangeStatePrivate(frame: PhysAddr.T; state: PhysAddr.State);
(* Pre: "frame" is "Lock"ed, and is "valid". *)

PROCEDURE Lock(page: PhysAddr.T);
PROCEDURE Unlock(page: PhysAddr.T);
(* Lock and unlock are usually used internally. *)

PROCEDURE SetParams(READONLY p : PhysAddr.Params);
(* Set the vm paging parameters. *)
  
PROCEDURE Init(verbose: BOOLEAN; tracedHeapStart: PhysAddr.Address;
	       tracedHeapSize: PhysAddr.Size);

END PhysAddrPrivate.

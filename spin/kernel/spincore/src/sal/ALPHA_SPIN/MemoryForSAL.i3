(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Removed unused procedures (with savage's permission).
 *
 * 12-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Changed to reflect virtual mapping.
 *
 * 08-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created.  
 *
 *)

(* MemoryForSAL implements a wrapper for upcalls from SAL into SPIN
   kernel.  Currently includes {Allocate,Free}PhysPage().  *)
   
INTERFACE MemoryForSAL;
IMPORT VirtAddr;

PROCEDURE AllocatePhysPage(VAR pp: VirtAddr.Address);
 (* "AllocatePhysPage" create a handle to a physical page and sets the
    virtual address of a physical page.  The physical page addressed
    by the "pp" virtual address cannot be reclaimed by the MM
    system. *)

PROCEDURE FreePhysPage(address: VirtAddr.Address): [0..1]; 
  (* "FreePhysPage" destroys the handle to the physical page addressed
     by the virtual "address" a handle to a physical page and sets the
     address of a physical page.  This page cannot be reclaimed by the
     MM system. *)

PROCEDURE Init(verbose:BOOLEAN);
END MemoryForSAL.

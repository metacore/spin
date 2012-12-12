(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	vm_pg_free and vm_pg_alloc are implemented by these functions.
 *	Removed other unused functionality (with savage's permission).
 *	
 *
 * 12-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Changed to reflect eventual virtual mapping and to keep track
 *	of physical pages used.
 *
 * 08-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created.  
 *
 *)

MODULE MemoryForSAL;

IMPORT PhysAddr, PhysAddrRep, VirtAddr, AddressPhysAddrTbl,
       PhysAddrPrivate;

VAR
  SALPageTable: AddressPhysAddrTbl.Default;
  SALPageTableLock: MUTEX;

PROCEDURE AllocatePhysPage(VAR pp: VirtAddr.Address) =
  VAR
    p: PhysAddr.T;
  BEGIN
    TRY
      LOCK SALPageTableLock DO
        p := PhysAddrPrivate.AllocatePinnedPage();
        pp := p.addr;
        EVAL SALPageTable.put(pp, p);
      END;
    EXCEPT
      (* XXX It is bad style to silently catch all exceptions. (mef) *)
    ELSE
      pp := 0;
    END;
  END AllocatePhysPage;

PROCEDURE FreePhysPage(address: VirtAddr.Address): [0..1] =
  VAR
    p: PhysAddr.T;
    ret: [0..1];
  BEGIN
    TRY
      LOCK SALPageTableLock DO
        IF SALPageTable.get(address, p) = TRUE THEN
          PhysAddr.Deallocate(p);
          ret := 1;
        ELSE
          PhysAddr.Deallocate(p);
          ret := 1; (* BUGBUG: Fix soon so other memory is included in table *)
        END;
      END;
    EXCEPT
      (* XXX It is bad style to silently catch all exceptions. (mef) *)
    ELSE
      ret := 0;
    END;
    RETURN ret;
  END FreePhysPage;

PROCEDURE Init(<*UNUSED*> verbose:BOOLEAN) =
  BEGIN
    (* internal initialization *)
    SALPageTable := NEW(AddressPhysAddrTbl.Default).init();
    SALPageTableLock := NEW(MUTEX);
  END Init;

BEGIN
END MemoryForSAL.

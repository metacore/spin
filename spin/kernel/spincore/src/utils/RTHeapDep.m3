(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Sal and CPU interfaces
 *
 * 31-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Changed force flag in Remap().
 *
 * 11-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed pmap interface.
 *
 * 20-Apr-97  Tian Fung Lim (tian) at the University of Washington
 *	Added Remap() for page remapping in Treadmill.
 *
 * 19-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added ProtectAddr.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Fix TimeUsed.
 *
 * 14-Oct-96  Stefan Savage (savage) at the University of Washington
 *	Fault() inlined into TrapPrivate (to deal with
 *	dependency issues between m3core and spincore)
 *
 * 09-Oct-96  Stefan Savage (savage) at the University of Washington
 *	Created (based on SRC template).
 *)
UNSAFE (* to import RTHeapRep *)
MODULE RTHeapDep;

IMPORT RTHeapRep, Protection, SalExtern, CPU, Word;
IMPORT PhysAddr;
(*
 * We do not have a reliable clock so use a cycle counter instead.
 * It can overflow but GC takes less time that it takes the counter
 * to overflow.  Even if it does we will only collect earlier or
 * later than if we got the correct time.  This is still safe because
 * full collection is triggered independently if too much is allocated.
 *
 * The first call to TimeUsed records current tick counter and returns
 * zero.  The second call returns number of microseconds since the
 * first call.  The calls are strictly ordered because the collector
 * is not reentrant
 *)

VAR
  first : BOOLEAN := FALSE;
  tick  : INTEGER := 0;

PROCEDURE TimeUsed (): INTEGER =
  BEGIN
    first := NOT first;
    IF first THEN
      tick := CPU.CycleCounter();
      RETURN 0;
    ELSE
      RETURN CPU.CycleToMicrosec(CPU.CycleMinus(CPU.CycleCounter(), tick));
    END;
  END TimeUsed;

PROCEDURE VMFaultTime (): INTEGER =
  BEGIN
    (* this is an over-estimate, good enough for now *)
    RETURN 1;
  END VMFaultTime;

PROCEDURE Protect (p                 : Page;
                   n                 : CARDINAL;
                   readable, writable: BOOLEAN   ) =
  VAR
    prot: Protection.T;
  BEGIN
    prot.read := readable;
    prot.write := writable;
    SalExtern.pmap_protect(SalExtern.kernel_pmap,
                        LOOPHOLE(RTHeapRep.PageToAddress(p), Word.T),
                        LOOPHOLE(RTHeapRep.PageToAddress(p), Word.T) +
                        n * CPU.PAGESIZE - ADRSIZE(ADDRESS), prot);
  END Protect;

PROCEDURE ProtectAddr (start, end : ADDRESS; readable, writable: BOOLEAN) =
  VAR
    prot: Protection.T;
  BEGIN
    prot.read := readable;
    prot.write := writable;
    SalExtern.pmap_protect(SalExtern.kernel_pmap, 
                        LOOPHOLE(start, Word.T), LOOPHOLE(end, Word.T), prot);
  END ProtectAddr; 


(* Remaps the page at address src to address dest. *)
PROCEDURE Remap(src : ADDRESS; dest : ADDRESS) =
  VAR
    pmap := SalExtern.kernel_pmap;
    frame : PhysAddr.Address;
    Prot := Protection.T{TRUE, TRUE, FALSE, 0};
  BEGIN
    frame := SalExtern.pmap_extract(pmap, LOOPHOLE(src, Word.T));
    (* unmap page from wherever it was *)
    SalExtern.pmap_remove_all(frame, FALSE);
    (* remap page to dest *)
    SalExtern.pmap_enter(pmap, LOOPHOLE(dest, Word.T), frame, Prot, 1, Prot);
  END Remap;


BEGIN
END RTHeapDep.

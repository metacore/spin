(*
 * Copyright 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 28-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made lock an 1 bit flag to save space.
 *	Made T an object and made IO memory be a subtype. Hope this will save
 *	memory.
 *	
 * 7-Apr-96 oystr at the University of Washington
 *	Added isIO space tag.
 *
 * 02-Jan-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

(*
  This interface exports the internal structure of the PhysAddr.T type
  which describes a page of physical memory.  Clients which use this
  interface must be trusted to correctly implement physical
  memory locking protocols and conventions.  Importers currently include
  the translation and DMA modules(really? yas).
 *)
INTERFACE PhysAddrRep;
IMPORT PhysAddr;
IMPORT Protection;
IMPORT Word;

REVEAL
  PhysAddr.T = BRANDED (* RECLAIMABLE *) OBJECT
    next, prev: PhysAddr.T; (* dequeue *)
    addr: Word.T; (* kernel phys address of the frame. *)
    tag: PhysAddr.Tag; (* User defined info. Not used by spincore. *)
    locked: INTEGER; (* mutex *)
    prot: BITS 32 FOR Protection.T;
      (* Max protection of the frame. The actual prot in particular pmap may be
	 weaker than this. *)
    state: BITS 3 FOR PhysAddr.State;
      
    isIO: BITS 1 FOR BOOLEAN;
      (* Always false for normal frames; TRUE for
         IO frames created by PhysAddrPrivate.AllocateIOSpace.*)
    valid: BITS 1 FOR BOOLEAN;
      (* Workaround until RECLAIMABLE works. If this bit is false, then all
       operations on the object fail. Thus, you can emulate the reclamation
       by setting this bit FALSE. *)
  END;
      

      
END PhysAddrRep.



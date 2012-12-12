(*
 * Copyright 1995,1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 14-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed pageIn and pageOut to take PhysAddr.T instead of VAR ARRAY
 *	OF CHAR.  This is more flexible.
 * 13-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added mapNotify.
 * 13-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 *)

INTERFACE PagerObject;
IMPORT VMTypes;
IMPORT PhysAddr;
IMPORT Protection;

TYPE
  ResultCode = {Success,
		AlreadyPagedIn,
		AlreadyPagedOut,
		PagerDestroyed,
		DeviceFull,
		NotImplemented};
  
  PageNumber = VMTypes.PageNumber;
  PageCount = VMTypes.PageCount;
		
  T <: Public;
  Public = OBJECT
    mObj: REFANY; (* XXX this is really a MemoryObject.T *)
  METHODS
    init(): T;
    (* Called then the object is initialized. You may wonder why this
       proc doesn't take the size argument. The reason is that, the pager
       object is created using a way other than this "init" in most cases.
       For example, in case of <a href="MemoryObject.html">bogopager or
       file pager</a>, pager object is created using
       "DefaultPager.Create".

       Thus, this procedure is a nop in most cases. *)
    
    destroy();
    
    pageIn(offset: PageNumber; type: INTEGER;
	   frame: PhysAddr.T; VAR prot: Protection.T): ResultCode;
    (* Bring in the page content. *)
    
    pageOut(offset: PageNumber; frame: PhysAddr.T; dirty: BOOLEAN): ResultCode;
    (* Write out the page content. *)
    
    pageMapNotify(offset: PageNumber; space: REFANY; virtAddr: PageNumber);
    (* Notification event from the vmcore telling that a frame at
       memobject offset "offset" is going to be
       mapped on the address "virtAddr" of the space "space".
       "space" is actually "AddressSpace.T", but it is declared
       "REFANY" to avoid import cycle.

       Implementation is free to ignore this event; in fact, the default
       implementation define this to be nop.

       XXX is this safe?*)
    
    mapNotify(from: PageNumber; len: PageCount;
	      space: REFANY; virtAddr: PageNumber);
    unmapNotify(from: PageNumber; len: PageCount;
		space: REFANY; virtAddr: PageNumber);
    
  END;
  
CONST Brand = "PagerObject-1.0";

END PagerObject.

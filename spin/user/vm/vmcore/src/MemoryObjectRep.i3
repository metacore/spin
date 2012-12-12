(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added Copy-on-write support.
 *
 * 20-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

INTERFACE MemoryObjectRep;

IMPORT MemoryObject;
IMPORT CacheObject;
IMPORT PagerObject;
IMPORT AddressMapEntry;
IMPORT VMTypes;
IMPORT Bitmap;

TYPE T = MemoryObject.Public OBJECT
  size: VMTypes.PageNumber;
  cache: CacheObject.T;
  pager: PagerObject.T;
  hasOwnContent: Bitmap.T;
  (* This field is non-NIL only on cow-mbjs, ie, mobjs whose "shadow" field
     is non-NIL.
     
     This maps page number to a boolean. When a bit is TRUE(=1), then
     this mobj has a local copy of the page in either cache or pager for
     that page. Thus, whenever the bit is 1, the page fault handler doesn't
     run up the cow chain.
  *)
  
  
  lock: MUTEX;
  
  shadow: T; (* Copy on write source *)
  copy: T; (* most recent child *)
  shadowOff: INTEGER; (* This obj holds pages starting "shadowOff"
		       in the "shadow" object(in page unit). This may be
		       negative, in case the cow chain is manipulated
		       internally like described below.*)
  mapEntries: AddressMapEntry.T;
  invalid: BOOLEAN;
  name: TEXT; (* for debugging. *)
METHODS
END;

(*
 "shadow" and "copy" are used to create a dequeue chain of c-o-w objects.

 "shadow" is the c-o-w source of the object.
 "copy" is the most recent c-o-w descendant of the memobject.
 
 Suppose there is a memory object A(also suppose no c-o-w has taken place
 on A). If B is created by c-o-w A, then

 | A.shadow = NIL, A.copy = B, 
 | B.shadow = A,  B.copy = NIL.

 The c-o-w chain becomes "A <- B".
 
 Later, if C is created by c-o-w A, then A.shadow = 
 
 | A.shadow = NIL, A.copy = C, 
 | C.shadow = A, C.copy = B.
 | B.shadow = C, B.copy = NIL.

 Therefore, c-o-w chain becomes "A <- C <- B".
 Later, if D is created from A, then the chain becomes "A <- D <- C <- B".
 
 This way, we can always linearize the c-o-w chains.

 (This technique is adopted from 4.4BSD)

 Yasushi
 *)
  
REVEAL MemoryObject.T <: T;


END MemoryObjectRep.

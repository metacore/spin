(*
 * Copyright 1994 - 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)
INTERFACE BaseMObjCache;

IMPORT CacheObject;
IMPORT VMTypes, MachineMem, VMError;
IMPORT Buffer, DoubleList;


TYPE
  T <: Public;
  Public = CacheObject.T OBJECT
    victimlist: DoubleList.T := NIL;  (* allows per-cache victim list *)
  METHODS
    (* initialize with numpages physical pages *)
    init(numpages: VMTypes.PageCount; name: TEXT) : T RAISES { VMError.E };

    (* get a buffer, which is just a container for a physical page.
       newowner is a ref to the new container for this buffer.
       return NIL if no buffer available *)
    getBuffer(newowner: REFANY := NIL; newindex: INTEGER := 0) : Buffer.T;

    (* return a buffer to the free pool. owner is the current container
       for this buffer. *)
    freeBuffer(buf: Buffer.T; owner: REFANY := NIL; index: INTEGER := 0);

    (* return a descriptive string - for debugging and error reporting *)
    print() : TEXT;

    (* return some cache statistics *)
    stat(VAR capacity: VMTypes.PageCount; VAR numfree: VMTypes.PageCount);
  END;

CONST 
  (* set to 4 megs.  ideally, this should be determined dynamically 
     according to the memory size of the machine, whether the
     cache pages will be pinned, how many caches there will be, etc. *)
  MaxPagesInCache = 4 * 1024 * 1024 DIV MachineMem.PAGESIZE;

END BaseMObjCache. 

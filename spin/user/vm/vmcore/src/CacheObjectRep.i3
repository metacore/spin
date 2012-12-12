(*
 * Copyright 1995,1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 *)

INTERFACE CacheObjectRep;

IMPORT CacheObject, OffsetCacheBlockTbl;
IMPORT RefSeq;

TYPE T = CacheObject. OBJECT
  size : CacheObject.Size;
  cowChain : RefSeq.T; (* list of copy on write memory objects *)
  cache: OffsetCacheBlockTbl.T;
  lock: MUTEX;
END;

REVEAL CacheObject.T <: T;

END CacheObjectRep.

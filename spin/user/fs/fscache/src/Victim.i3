(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)

(* Victim defines the cache victim policy.  The default policy is LRU
   global over all cache blocks.  The procedures in this interface can
   be dynamically overridden with event handlers. *)

(* XXX - should probably define authorizer *)

INTERFACE Victim;

IMPORT BaseMObjCache, FileMObjCache, Buffer;

TYPE RefType = { Read, Write };

(* called when a buffer is allocated and granted to a file mem obj *)
PROCEDURE AllocBuffer(bcache: BaseMObjCache.T; fcache: FileMObjCache.T;
  buf: Buffer.T);

(* called when a buffer is returned to the base mem obj and placed in
   the free buffer pool *)
PROCEDURE FreeBuffer(bcache: BaseMObjCache.T; fcache: FileMObjCache.T;
  buf: Buffer.T);

(* called when a buffer in a file mem obj is referenced *)
PROCEDURE Ref(bcache: BaseMObjCache.T; fcache: FileMObjCache.T;
  blockno: FileMObjCache.BlockNo; buf: Buffer.T; reftype: RefType);

(* called by a file mem obj when a buffer is needed and the free buffer
   pool is empty. returns NIL if no buffers can be stolen. *)
PROCEDURE StealBuffer(bcache: BaseMObjCache.T; fcache: FileMObjCache.T;
  blockno: FileMObjCache.BlockNo) : Buffer.T;

(* indicate which file contains the most likely victim block and least
   likely victim block *)
PROCEDURE Stat(bcache: BaseMObjCache.T; 
  VAR fcacheNextVictim: FileMObjCache.T; VAR nextIndex: INTEGER;
  VAR fcacheNotVictim: FileMObjCache.T; VAR notIndex: INTEGER);

END Victim.

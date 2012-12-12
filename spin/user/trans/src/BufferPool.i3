(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to per-site cache that manages buffers from multiple storages.
 * 18-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(*
   BufferPool is yet another cache.
   It manages pages in fast hash table.
   "find" & "insert" puts the page at the top of the queue.

   In this module, storage offsets are all MMU pagesize unit.
   All the operations are mutexed internally. 
 *)
INTERFACE BufferPool;
IMPORT Buffer, Storage;
IMPORT PhysAddr;
		
PROCEDURE Lock(st: Storage.T; page: Buffer.Offset): Buffer.T;
(* Find a buffer that caches the storage "st" at position "page" and
   pins & locks the buffer so that it won't be chosen as the
   LRU victim. *)
PROCEDURE LockWithoutPin(st: Storage.T; page: Buffer.Offset): Buffer.T;

PROCEDURE GetPhysAddr(st: Storage.T; page: Buffer.Offset): PhysAddr.T;
  
PROCEDURE Unpin(buf: Buffer.T);
(* Unpin the buffer without unlocking. *)
   
PROCEDURE Unlock(buf: Buffer.T);
(* Unpin & unlocks the buffer. *)

PROCEDURE UnlockWithoutUnpin(buf: Buffer.T);
  
PROCEDURE Allocate(st: Storage.T; page: Buffer.Offset;
		   frame: PhysAddr.T): Buffer.T;
(* Allocate an empty buffer. The buffer is returned locked. *)

PROCEDURE Delete(buf: Buffer.T);
(* Put the buffer in the free pool. Client has the responsibility to
   write back the contents beforehand if necessary.
   Pre: "buf" is locked and pinned *)

PROCEDURE Intern(buf: Buffer.T): BOOLEAN;
(* Insert a buffer into the pool. "buf" must be the one returned
   by "allocate".
   Returns TRUE if a buffer for the same storage, the same offset is
   already present in the buffer. *)
  
PROCEDURE Size(): INTEGER;
(* Returns the # of live buffers in the pool *)

PROCEDURE Iterate(st: Storage.T): Iterator;
(* Start iteration. "st" can de NIL, in which case all the buffers are
   retrieved.
   Post: buffer is locked *)
  
PROCEDURE EndIterate();
(* End iteration. Post: buffer is unlocked *)

TYPE
  Iterator = OBJECT
  METHODS
    next((*OUT*)VAR t: Buffer.T): BOOLEAN;
    (* Buffer is returned NOT pinned and NOT locked. *)
  END;
    
END BufferPool.

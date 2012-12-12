(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 08-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Buffer manages a physical page. A page is a fixed sized(MMU page)
   array of bytes. It's actual size and contents aren't not relevant 
   to the Buffer.
 *)  
INTERFACE Buffer;
IMPORT Storage;
IMPORT PhysAddr;
IMPORT CPU;
IMPORT MachineTrans;
IMPORT ActiveTrans;
IMPORT VMError;

TYPE
  Offset = CARDINAL; (* "Offset" represents a relative position of the
			buffer within the storage. It is MMU page size
			unit. *)
  T <: ROOT;

PROCEDURE IsLocked(t: T): BOOLEAN;

PROCEDURE Write(t: T);
(* Write out the contents into the storage "t.st" at bytepos "t.pos".
   XXX this should not be here. *)
  
PROCEDURE NormalDiff(t: T; VAR diff: Diff): CARDINAL;
(* Take a old-new-diff of "t.frame" and "t.shadow" and store it on "diff".
   Returns the number of bytes stored in "diff". *)
  
PROCEDURE ReverseDiff(t: T; VAR diff: Diff): CARDINAL;
(* Take a new-old-diff of "t.frame" and "t.shadow" and store it on "diff".
   Returns the number of bytes stored in "diff". *)
  
PROCEDURE Patch(t: T; READONLY diff: ARRAY OF CHAR);
(* Opposite of "Diff". This proc applies the diff to "t.frame", it
   also copies the resulting "t.frame" into "t.shadow". *)

PROCEDURE Shadow(t: T; at: ActiveTrans.T);
  
PROCEDURE Lock(st: Storage.T; page: Offset): T RAISES {VMError.E};
(* Find a buffer that caches the storage "st" at position "page" and
   pins & locks the buffer so that it won't be chosen as the
   LRU victim. *)
PROCEDURE LockWithoutPin(st: Storage.T; page: Offset): T;

PROCEDURE Unlock(buf: T);
(* Unpin & unlocks the  *)

PROCEDURE Allocate(st: Storage.T; page: Offset;
		   frame: PhysAddr.T): T;
(* Allocate an empty  The buffer is returned locked. *)

PROCEDURE Delete(buf: T);
(* Put the buffer in the free pool. Client has the responsibility to
   write back the contents beforehand if necessary.
   Pre: "buf" is locked and pinned *)

PROCEDURE Intern(buf: T): BOOLEAN;
(* Insert a buffer into the pool. "buf" must be the one returned
   by "allocate".
   Returns TRUE if a buffer for the same storage, the same offset is
   already present in the  *)
  
PROCEDURE Size(): INTEGER;
(* Returns the # of live buffers in the pool *)

PROCEDURE Iterate(st: Storage.T): Iterator;
(* Start iteration. "st" can de NIL, in which case all the buffers are
   retrieved.
   Post: buffer is locked *)
  
PROCEDURE EndIterate();
(* End iteration. Post: buffer is unlocked *)

PROCEDURE Nuke();
  
TYPE
  Iterator = OBJECT
  METHODS
    next((*OUT*)VAR t: T): BOOLEAN;
    (* Buffer is returned NOT pinned and NOT locked. *)
  END;

CONST
  DiffRunLength = BITSIZE(INTEGER) DIV 2;
  
TYPE
  DiffEntry = MachineTrans.DiffEntry;
  Diff = ARRAY [0 .. CPU.PAGESIZE + 64] OF CHAR;

PROCEDURE AllocDiff(): REF Diff;
PROCEDURE FreeDiff(buf: REF Diff);

CONST Brand = "Buffer";
PROCEDURE CheckBufferIsNotModified(buf: T): BOOLEAN;    
  
END Buffer.

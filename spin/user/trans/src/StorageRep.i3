(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 17-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *
 * html
 *)
INTERFACE StorageRep;
IMPORT Storage;
IMPORT Buffer;
IMPORT MemoryObject;
IMPORT PagerObject;
IMPORT PhysAddr;
IMPORT ActiveTrans, ActiveTransQ;
IMPORT LockQ, LockMode;
IMPORT Thread;
IMPORT TID;
IMPORT WAL;
IMPORT VMError;

REVEAL Storage.T <: TPublic;
TYPE TPublic = Storage.TPublic BRANDED OBJECT
  mu: MUTEX; (* used by "lock"&"unlock" *)
  memObj, shadowObj: MemoryObject.T;
  pager: PagerObject.T; (* pager for the memory object. *)
  
  activeTrans: ActiveTransQ.T; (* list of transactions carried on on
				  this storage *)
    
  
  (* Below are used by the lock manager *)
  lockMu: MUTEX;
  sync: Thread.Condition;

  (* Holds checksum of all the swapped out pages. used only when
     "TransUtils.Debug=TRUE". *)
  checkSum: REF ARRAY OF INTEGER;
  
  (* "lockMu" and "sync" are used to synchronize access to "locks" *)
  locks: LockQ.T; (* "locks" holds the queue of the LockRecord *)
  
  fail: BOOLEAN; (* used only in debug mode. *)
METHODS
  operateOnRegion (at: ActiveTrans.T; lock: LockMode.T; pos, len: INTEGER; 
		   callback: PROCEDURE(VAR c: ARRAY OF CHAR;
				       buf: Buffer.T; pos: INTEGER));
  (* For each page contained in the region "pos" .. "pos"+"len",
     this proc calls "callback". "c" is the subarray of the content of
     the buffer, and "pos" is the byte position of the "c" within
     the storage. If [LockMode:"lock"] is not "None", then the page(s) that
     contain the region are locked in the mode "lock" by the trans "tr".

     This is implemented in Storage.m3.

     Pre: "st" is locked.
  *)

  pinFrame(at: ActiveTrans.T; lock: LockMode.T;
	   pos: Buffer.Offset; VAR allocate: BOOLEAN;
	   frame: PhysAddr.T := NIL): Buffer.T RAISES {VMError.E};
  (* Pin the frame at "pos"(counted in MMU page unit), and return the buffer.
     "frame" is used when the page is not found and has to be
     allocated. If "frame" is NIL, then the proc allocates a new frame
     by itself. Otherwise, it uses "frame".

     "allocate" is an in-out varible. It if it TRUE on input, then
     a buffer is allocated(by either allocating new frame or using "frame")
     when it is not found in the pool. Otherwise, the method returns NIL
     in such case. On return, "allocate" is set to indicate whether
     new buffer is allocated or not.
     
     Note that this proc doesn't create a pin descriptor.
     If [LockMode:"lock"] is not "None", then the page(s) that
     contain the region are locked in the mode "lock" by the trans "tr".
     This is a virtual method.
     
     Pre: "st" is locked.
     Post: retval is locked.
  *)

  writeRedo(tid: TID.T; type: WAL.Type; prevLSN: WAL.LSN;
	    pos: Buffer.Offset; READONLY image: ARRAY OF CHAR): WAL.LSN;
END;

END StorageRep.

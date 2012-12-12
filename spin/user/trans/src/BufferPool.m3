(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE BufferPool;
IMPORT Storage, StorageRep, TransServiceRep;
IMPORT Buffer, BufferQ;
IMPORT Thread;
<*NOWARN*>IMPORT IO;
IMPORT PhysAddr;
IMPORT TransUtils;
IMPORT LightMutex;

CONST TableSize = 1024;

VAR
  mu: MUTEX; (* Guards both table and buffer "lock" field. *)
  nPages: CARDINAL; (* # of pages in the hash *)
  hashTable: ARRAY [0 .. TableSize-1] OF Buffer.T;
  queue: BufferQ.T;
  
TYPE IteratorWithStorage = Iterator OBJECT
  st: Storage.T;
  itr: BufferQ.Iterator;
OVERRIDES
  next := NextWithStorage;
END;
  
TYPE IteratorWithoutStorage = Iterator OBJECT
  itr: BufferQ.Iterator;
OVERRIDES
  next := NextWithoutStorage;
END;


<*INLINE*>
PROCEDURE Hash(buf: Buffer.T): INTEGER =
  BEGIN
    RETURN (buf.st.id.lid + buf.pos) MOD TableSize;
  END Hash;

PROCEDURE Allocate (st: Storage.T; page: Buffer.Offset;
		    frame: PhysAddr.T): Buffer.T =
  VAR
    tag := PhysAddr.Tag{st.memObj, page};
    (* XXX it's a little weird that we have to feed the correct tag
       here for vmcore to function correctly. *)

    buf := NEW(Buffer.T,
	       pinCount := 1,
	       lsn := 0,
	       recoveryLSN := 0,
	       st := st,
	       mu := LightMutex.Create(),
	       pos := page,
	       dirty := FALSE,
	       valid := TRUE);
  BEGIN
    IF frame = NIL THEN
      Thread.Release(st.mu); (* This is to avoid deadlock bewteen me and
				pageout handler. This is surely ugly...
				However, this works. Only access race
				is on ActiveTrans.modifications, and
				only guys that potentially modify them are
				StorageXXX.SetRange and
				StorageXXX.FlushModificationsForTrans and
				the pageout handler.
				And StorageXXX.FlushModificationsForTrans
				never calls this function. 
			     *)
      buf.frame := PhysAddr.Allocate(tag);
      Thread.Acquire(st.mu);
    ELSE
      buf.frame := frame;
    END;
    LightMutex.Lock(buf.mu);
    RETURN buf;
  END Allocate;

PROCEDURE Delete (buf: Buffer.T) =
  VAR
    hash := Hash(buf);
    e: Buffer.T;
  BEGIN
    LOCK mu DO
      <*ASSERT buf.pinCount = 0*>
      <*ASSERT Buffer.IsLocked(buf)*>
      (* The buffer has to be locked by the caller, but no one else
	 should be using this. *)
       
      (* Remove "buf" from the hash table *)
      e := hashTable[hash];
      IF e = buf THEN
	hashTable[hash] := e.hashLink;
      ELSE
	WHILE e.hashLink # buf DO
	  e := e.hashLink;
	END;
	e.hashLink := e.hashLink.hashLink;
      END;
      DEC(nPages);
      BufferQ.Remove(buf);
      <*ASSERT Sane()*>
    END;
  END Delete;
  
PROCEDURE Intern (buf: Buffer.T): BOOLEAN =
  VAR
    e: Buffer.T;
    hash: INTEGER;
  BEGIN
    LOCK mu DO
      <*ASSERT Sane()*>
      <*ASSERT buf.pinCount = 1*>
      <*ASSERT TransUtils.ValueIsNotPageAligned(buf.pos)*>
      hash := Hash(buf);

      (* Check the duplicate *)
      e := hashTable[hash];
      WHILE e # NIL DO
	IF e.st = buf.st AND e.pos = buf.pos THEN
	  RETURN TRUE;
	END;
	e := e.hashLink;
      END;

      (* Duplicate not found, insert "buf" into the table. *)
      buf.hashLink := hashTable[hash];
      hashTable[hash] := buf;

      <*ASSERT buf.pinCount = 1*>
      BufferQ.InsertHead(queue, buf);

      INC(nPages);
      <*ASSERT Sane()*>
    END;
    RETURN FALSE;
  END Intern;

<*INLINE*>
PROCEDURE FindInternal (st: Storage.T; pos: INTEGER): Buffer.T =
  VAR
    hash := (st.id.lid + pos) MOD TableSize;
    e: Buffer.T;
  BEGIN
    e := hashTable[hash];
    
    WHILE e # NIL DO
      IF e.st = st AND e.pos = pos THEN
	RETURN e;
      END;
      e := e.hashLink;
    END;
    
    RETURN NIL;
  END FindInternal;
  
PROCEDURE Lock (st: Storage.T; pos: Buffer.Offset): Buffer.T =
  VAR
    buf: Buffer.T;
  BEGIN
    <*ASSERT TransUtils.ValueIsNotPageAligned(pos)*>
    LOCK mu DO
      buf := FindInternal(st, pos);
      IF buf = NIL THEN RETURN NIL; END;

      IF buf.pinCount = 0 THEN
	(* Pin down the buffer *)
	buf.pinCount := 1;
	PhysAddr.ChangeState(buf.frame, PhysAddr.State.WeaklyPinned);
      ELSE
	INC(buf.pinCount);
      END;
    END;
    LightMutex.Lock(buf.mu);
    IF NOT buf.valid THEN RETURN NIL; END;
    <*ASSERT Sane()*> 
    RETURN buf;
  END Lock;

PROCEDURE LockWithoutPin (st: Storage.T; pos: Buffer.Offset): Buffer.T =
  VAR buf: Buffer.T;
  BEGIN
    <*ASSERT TransUtils.ValueIsNotPageAligned(pos)*>
    LOCK mu DO
      buf := FindInternal(st, pos);
    END;
    IF buf = NIL THEN RETURN NIL; END;
    LightMutex.Lock(buf.mu);
    IF NOT buf.valid THEN RETURN NIL; END;
    <*ASSERT Sane()*> 
    RETURN buf;
  END LockWithoutPin;
  
PROCEDURE GetPhysAddr (st: Storage.T; pos: Buffer.Offset): PhysAddr.T =
  VAR
    buf: Buffer.T;
  BEGIN
    <*ASSERT TransUtils.ValueIsNotPageAligned(pos)*>
    LOCK mu DO
      buf := FindInternal(st, pos);
      IF buf = NIL OR NOT buf.valid THEN RETURN NIL; END;
      RETURN buf.frame;
    END;
  END GetPhysAddr;

PROCEDURE Size (): INTEGER =
  BEGIN
    RETURN nPages;
  END Size;

PROCEDURE Unlock (buf: Buffer.T) =
  BEGIN
    <*ASSERT Buffer.IsLocked(buf)*>
    LOCK mu DO
      DEC(buf.pinCount);
      IF buf.pinCount = 0 THEN
	PhysAddr.ChangeState(buf.frame, PhysAddr.State.Active);
      END;
      
      LightMutex.Unlock(buf.mu);
      <*ASSERT Sane()*>
    END;
  END Unlock;

PROCEDURE UnlockWithoutUnpin (buf: Buffer.T) =
  BEGIN
    <*ASSERT Buffer.IsLocked(buf)*>
    LightMutex.Unlock(buf.mu);
  END UnlockWithoutUnpin;
  
PROCEDURE Unpin (buf: Buffer.T) =
  BEGIN
    (* XXX lock here. *)
    DEC(buf.pinCount);
    IF buf.pinCount = 0 THEN
      PhysAddr.ChangeState(buf.frame, PhysAddr.State.Active);
    END;
    <*ASSERT Sane()*>
  END Unpin;
  
(* See if contents in "p" are consistent.
 Pre: p is locked. *)
PROCEDURE Sane (): BOOLEAN =
  VAR
    e, blah: BufferQ.T;
    n: CARDINAL := 0;
    itr: BufferQ.Iterator;
  BEGIN
    RETURN TRUE; (* XXX this checking is too slow.. *)
    (* Check if # of items in the hash table is equal to nPages *)
    FOR i := 0 TO TableSize-1 DO
      e := hashTable[i];
      WHILE e # NIL DO
	e := e.hashLink;
	INC(n);
      END;
    END;
    <*ASSERT nPages = n*>

    (* Check if all the pages in queue are found in hash *)
    n := 0;
    itr := BufferQ.Iterate(queue);
    WHILE BufferQ.NextItr(itr, e) DO
      INC(n);
      blah := FindInternal(e.st, e.pos);
      <*ASSERT blah = e*>
    END;
    <*ASSERT nPages = n*>
    
    RETURN TRUE;
  END Sane;


  
PROCEDURE Iterate (st: Storage.T): Iterator =
  VAR itr: Iterator;
  BEGIN
    Thread.Acquire(mu);
    IF st = NIL THEN
      itr := NEW(IteratorWithoutStorage, itr := BufferQ.Iterate(queue));
    ELSE
      itr := NEW(IteratorWithStorage, st := st, itr := BufferQ.Iterate(queue));
    END;
    RETURN itr;
  END Iterate;

PROCEDURE EndIterate() =
  BEGIN
    Thread.Release(mu);
  END EndIterate;

PROCEDURE PinAndLock (buf: Buffer.T) =
  BEGIN
    IF buf.pinCount = 0 THEN
      (* Pin down the buffer *)
      buf.pinCount := 1;
      PhysAddr.ChangeState(buf.frame, PhysAddr.State.WeaklyPinned);
    ELSE
      INC(buf.pinCount);
    END;
    LightMutex.Lock(buf.mu);
  END PinAndLock;
  
PROCEDURE NextWithStorage (itr: IteratorWithStorage;
			   VAR t: Buffer.T): BOOLEAN =
  BEGIN
    LOOP
      IF NOT BufferQ.NextItr(itr.itr, t) THEN
	RETURN FALSE;
      END;
      IF t.st = itr.st THEN
	(* XXX deadlock!! 
	PinAndLock(t); *)
	RETURN TRUE;
      END;
    END;
  END NextWithStorage;
  
PROCEDURE NextWithoutStorage (itr: IteratorWithoutStorage;
			      VAR t: Buffer.T): BOOLEAN =
  BEGIN
    IF BufferQ.NextItr(itr.itr, t) THEN
      (* XXX deadlock!! 
	 PinAndLock(t); *)
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END NextWithoutStorage;
  

BEGIN
  mu := NEW(MUTEX);
  nPages := 0;
  queue := BufferQ.NewHeader();
END BufferPool.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE Buffer;
(* unsafe because it calls diffextern *)
IMPORT IO, Fmt;
IMPORT BufferQ, BufferRep;
IMPORT PhysAddr;
IMPORT RawIO;
IMPORT Storage, StorageRep, StorageLocal;
IMPORT CPU;
IMPORT LightMutex, Mutex, Thread, Strand;
IMPORT TransUtils;
IMPORT TransServiceRep;
IMPORT DiffExtern;
IMPORT ActiveTrans, ActiveTransRep;
IMPORT Debugger;<*NOWARN*>
IMPORT ArrayUtils;
IMPORT VMError;
FROM TransUtils IMPORT Debug, DebugMsg, Msg;
CONST TableSize = 1024;

VAR
  mu: MUTEX; (* Guards "hashTable" *)
  nPages: CARDINAL; (* # of pages in the hash *)
  hashTable: ARRAY [0 .. TableSize-1] OF T;
  
PROCEDURE IsLocked (t: T): BOOLEAN =
  BEGIN
    IF LightMutex.TryLock(t.mu) THEN
      LightMutex.Unlock(t.mu);
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END IsLocked;
  
VAR
  checkMu := NEW(MUTEX);
  checkBuf: REF ARRAY OF CHAR;
(* Make sure that frame contents are same as checkBuf.
   Pre, post: checkMu is locked. *)
PROCEDURE CheckFrameContents (frame: PhysAddr.T) =
  VAR
    x1: ARRAY [0 .. 127] OF CHAR;
  BEGIN
    TRY
      FOR i := 0 TO CPU.PAGESIZE-1 BY 128 DO
	PhysAddr.Read(frame, i, x1);
	IF x1 # SUBARRAY(checkBuf^, i, 128) THEN
	  IO.Put("content changed?\n");
	  Debugger.Enter();
	END;
      END;
    EXCEPT
    | VMError.E(e) =>
      Msg("Buffer.CheckFrameContents: ", VMError.Message(e));
    END;
  END CheckFrameContents;

PROCEDURE Write (buf: T) =
  PROCEDURE Callback(VAR x: PhysAddr.Content) =
    BEGIN
      RawIO.Write(NARROW(buf.st, StorageLocal.T).fh, x,
		  buf.pos*CPU.PAGESIZE);
    END Callback;
  BEGIN
    IF NOT Debug THEN
      IF NOT buf.dirty THEN
	Debugger.Enter();
      END;
      PhysAddr.Access(buf.frame, Callback);
    ELSE    
      (* Read from the sectors we've just written on, and make sure
	 the contents match. *)
      LOCK checkMu DO
	IF checkBuf = NIL THEN
	  checkBuf := NEW(REF ARRAY OF CHAR, CPU.PAGESIZE);
	END;
	PhysAddr.Read(buf.frame, 0, checkBuf^);
	<*ASSERT ISTYPE(buf.st, StorageLocal.T)*>
	PhysAddr.Access(buf.frame, Callback);
	CheckFrameContents(buf.frame);
	ArrayUtils.Set(checkBuf^, VAL(16_5a, CHAR));
	EVAL RawIO.Read(NARROW(buf.st, StorageLocal.T).fh, checkBuf^,
			buf.pos*CPU.PAGESIZE);
	CheckFrameContents(buf.frame);
      END;
    END;
  END Write;

PROCEDURE CheckBufferIsNotModified (buf: T): BOOLEAN =
  BEGIN
    LOCK checkMu DO
      IF checkBuf = NIL THEN
	checkBuf := NEW(REF ARRAY OF CHAR, CPU.PAGESIZE);
      END;
      EVAL RawIO.Read(NARROW(buf.st, StorageLocal.T).fh, checkBuf^,
		      buf.pos*CPU.PAGESIZE);
      CheckFrameContents(buf.frame);
    END;
    RETURN TRUE;
  END CheckBufferIsNotModified;
  
PROCEDURE ReverseDiff (t: T; VAR diff: Diff): CARDINAL =
  VAR n: CARDINAL;
  PROCEDURE Callback1 (VAR frame: PhysAddr.Content) =
    PROCEDURE Callback2 (VAR shadow: PhysAddr.Content) =
      BEGIN
	n := DiffExtern.diff(ADR(frame[0]), ADR(shadow[0]), ADR(diff[0]), FALSE);
      END Callback2;
    BEGIN
      PhysAddr.Access(t.shadow, Callback2);
    END Callback1;
  BEGIN
    PhysAddr.Access(t.frame, Callback1);
    RETURN n;
  END ReverseDiff;
  
PROCEDURE NormalDiff (t: T; VAR diff: Diff): CARDINAL =
  VAR n: CARDINAL;
  PROCEDURE Callback1 (VAR frame: PhysAddr.Content) =
    PROCEDURE Callback2 (VAR shadow: PhysAddr.Content) =
      BEGIN
	n := DiffExtern.diff(ADR(shadow[0]), ADR(frame[0]), ADR(diff[0]), TRUE);
      END Callback2;
    BEGIN
      PhysAddr.Access(t.shadow, Callback2);
    END Callback1;
  BEGIN
    PhysAddr.Access(t.frame, Callback1);
    RETURN n;
  END NormalDiff;

PROCEDURE Patch (t: T; READONLY cdiff: ARRAY OF CHAR) =
  PROCEDURE Callback (VAR cimage: PhysAddr.Content) =
    VAR i := 0;
    BEGIN
      WITH diff = VIEW(cdiff, ARRAY OF INTEGER),
	   image = VIEW(cimage, ARRAY OF INTEGER) DO 
	WHILE i < NUMBER(diff) DO
	  WITH hdr = VIEW(diff[i], DiffEntry) DO
	    SUBARRAY(image, hdr.from, hdr.len) := SUBARRAY(diff, i+1, hdr.len);
	    INC(i, hdr.len+1);
	  END;
	END;
      END;
    END Callback;
  BEGIN
    TRY
      PhysAddr.Access(t.frame, Callback);
    EXCEPT
    | VMError.E(e) =>
      Msg("Buffer.Patch", VMError.Message(e));
    END;
  END Patch;

PROCEDURE Shadow (buf: T; at: ActiveTrans.T) =
  VAR
    page := buf.pos;
    st := buf.st;
  BEGIN
    <*ASSERT IsLocked(buf)*>
    IF buf.shadow = NIL THEN
      IF DebugMsg THEN
	IO.Put("shadow " & st.name & " off = " & Fmt.Int(page) & ".\n");
      END;
      VAR tag := PhysAddr.Tag{st.shadowObj, page};
      BEGIN
	TRY
	  Thread.Release(st.mu); (* See also the comment in "Allocate". *)
	  buf.shadow := PhysAddr.Allocate(tag);
	  Thread.Acquire(st.mu);
	  PhysAddr.Copy(buf.shadow, buf.frame);
	EXCEPT
	| VMError.E(e) =>
	  Msg("Buffer.Shadow1: ", VMError.Message(e));
	END;
      END;
    ELSE
      IF Debug AND buf.at = NIL THEN
	(* Make sure "buf.frame" and "buf.shadow" are same.
	   If buf.at # NIL, then the current transaction may be
	   modifying this buffer, so this check may fail. However, this
	   isn't a problem *)
	VAR x1, x2: ARRAY [0..127] OF CHAR;
	BEGIN
	  TRY
	    FOR i := 0 TO CPU.PAGESIZE-1 BY 128 DO
	      PhysAddr.Read(buf.frame, i, x1);
	      PhysAddr.Read(buf.shadow, i, x2);
	      IF x1 # x2 THEN
		IO.Put("shadow page screwed.\n");
		Debugger.Enter();
	      END;
	    END;
	  EXCEPT
	  | VMError.E(e) =>
	    Msg("Buffer.Shadow2: ", VMError.Message(e));
	  END;
	END;
      END;
    END;

    buf.dirty := TRUE;
    IF buf.at = NIL THEN
      INC(at.nPagesModified);
      buf.at := at;
      BufferQ.InsertTail(at.pages, buf);
    ELSE
      <*ASSERT buf.at = at AND BufferQ.Member(at.pages, buf)*>
    END;
  END Shadow;
    
<*INLINE*>
PROCEDURE Hash (buf: T): INTEGER =
  BEGIN
    RETURN (buf.st.id.lid + buf.pos) MOD TableSize;
  END Hash;

PROCEDURE Allocate (st: Storage.T; page: Offset; frame: PhysAddr.T): T =
  VAR
    tag := PhysAddr.Tag{st.memObj, page};
    (* XXX it's a little weird that we have to feed the correct tag
       here for vmcore to function correctly. *)
    buf :=  BufferQ.Allocate();
  BEGIN
    buf.at := NIL;
    buf.st := st;
    buf.pos := page;
    buf.dirty := FALSE;
    buf.valid := TRUE;
    buf.lsn := 0;
    buf.recoveryLSN := 0;
    buf.mu := LightMutex.Create();
    
    IF DebugMsg THEN
      IO.Put("alloc " & st.name & " off = " & Fmt.Int(page) & ".\n");
    END;
    IF frame = NIL THEN
      TRY
	Thread.Release(st.mu); (* This is to avoid deadlock bewteen me and
				  pageout handler. This is surely ugly...
				  However, this works. Only access race
				  is on ActiveTrans.modifications, and
				  only guys that potentially modify them are
				  StorageXXX.SetRange and
				  StorageXXX.FlushModificationsForTrans and
				  the pageout handler.
				  And StorageXXX.FlushModificationsForTrans
				  never calls this function. *)
	buf.frame := PhysAddr.Allocate(tag);
	Thread.Acquire(st.mu);
      EXCEPT
      | VMError.E(e) =>
	Msg("Buffer.Allocate: ", VMError.Message(e));
      END;
    ELSE
      buf.frame := frame;
    END;
    LightMutex.Lock(buf.mu);
    RETURN buf;
  END Allocate;

PROCEDURE DeleteUnlocked (buf: T) =
  VAR
    hash := Hash(buf);
    e: T;
  BEGIN
    <*ASSERT IsLocked(buf)*>
    <*ASSERT buf.valid*>
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
    <*ASSERT buf.next = NIL AND buf.prev = NIL*>
    PhysAddr.Deallocate(buf.frame);
    buf.frame := NIL;
    IF buf.shadow # NIL THEN
      PhysAddr.Deallocate(buf.shadow);
      buf.shadow := NIL;
    END;
    buf.valid := FALSE;
    BufferQ.Free(buf);
    LightMutex.Unlock(buf.mu);
  END DeleteUnlocked;
  
PROCEDURE Delete (buf: T) =
  BEGIN
    LOCK mu DO
      DeleteUnlocked(buf);
    END;
  END Delete;
  
PROCEDURE Intern (buf: T): BOOLEAN =
  VAR
    e: T;
    hash: INTEGER;
  BEGIN
    LOCK mu DO
      <*ASSERT Sane()*>
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

      INC(nPages);
      <*ASSERT Sane()*>
    END;
    RETURN FALSE;
  END Intern;

<*INLINE*>
PROCEDURE FindInternal (st: Storage.T; pos: INTEGER): T =
  VAR
    hash := (st.id.lid + pos) MOD TableSize;
    e: T;
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

PROCEDURE Lock (st: Storage.T; pos: Offset): T RAISES {VMError.E} =
  VAR
    buf: T;
  BEGIN
    LOCK mu DO
      buf := FindInternal(st, pos);
    END;
    IF buf = NIL THEN RETURN NIL; END;
    (* Note: we shouldn't lock buf.mu at this moment because
       1) exception may be raised, and we don't want to
       return with the buf locked.
       2) ChangeState is just a hint
    *)
    IF buf.shadow # NIL THEN
      PhysAddr.ChangeState(buf.shadow, PhysAddr.State.Active);
    END;
    PhysAddr.ChangeState(buf.frame, PhysAddr.State.Active);
    LightMutex.Lock(buf.mu);
    IF NOT buf.valid THEN 
      Msg("Buffer.LockW/OPin:buf invalid.\n");
      RETURN NIL;
    END;
    <*ASSERT Sane()*> 
    RETURN buf;
  END Lock;

PROCEDURE LockWithoutPin (st: Storage.T; pos: Offset): T =
  VAR buf: T;
  BEGIN
    LOCK mu DO
      buf := FindInternal(st, pos);
    END;
    IF buf = NIL THEN RETURN NIL; END;
    LightMutex.Lock(buf.mu);
    IF NOT buf.valid THEN
      Msg("Buffer.LockW/OPin:buf invalid.\n");
      RETURN NIL;
    END;
    <*ASSERT Sane()*> 
    RETURN buf;
  END LockWithoutPin;
  
PROCEDURE Size (): INTEGER =
  BEGIN
    RETURN nPages;
  END Size;

PROCEDURE Unlock (buf: T) =
  BEGIN
    <*ASSERT IsLocked(buf) AND buf.valid*>
    LightMutex.Unlock(buf.mu);
    <*ASSERT Sane()*>
  END Unlock;
  
(* See if contents in "p" are consistent.
 Pre: p is locked. *)
PROCEDURE Sane (): BOOLEAN =
  VAR
    e: BufferQ.T;
    n: CARDINAL := 0;
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
    RETURN TRUE;
  END Sane;

PROCEDURE Nuke () =
  VAR
    itr: Iterator;
    buf: T;
    inUse := 0;
    nuked := 0;
    done: BOOLEAN;
  BEGIN
    LOOP
      done := TRUE;
      itr := Iterate(NIL);
      WHILE itr.next(buf) DO
	IF buf.at # NIL OR IsLocked(buf) THEN 
	  (* skip buffers in use. *)
	  INC(inUse);
	ELSE
	  IF LightMutex.TryLock(buf.mu) THEN
	    IF buf.dirty THEN
	      Write(buf);
	    END;
	    DeleteUnlocked(buf);
	    INC(nuked);
	  ELSE
	    (* In other places, the lock order is "mu" -> "buf.mu".
	       We are locking in the opposite order here. So, to
	       avoid deadlocks, we need to trylock-spin. *)
	    done := FALSE;
	    EXIT;
	  END;   
	END;
      END;
      EndIterate();
      
      IF done THEN
	EXIT;
      ELSE
	IO.Put("whoops! possible deadlock in Buffer.Nuke; retrying.\n");
	Strand.Yield();
      END;
    END;
    IF inUse > 0 THEN
      IO.Put("Buffer.Nuke : " & Fmt.Int(nuked) & " pages freed, "
	   & Fmt.Int(inUse) & " pages in use.\n");
    END;
  END Nuke;
  
TYPE
  XIterator = Iterator OBJECT
    hashIdx: [0 .. TableSize]; (* last elem is a sentinel. *)
    buf: T;
    st: Storage.T;
  OVERRIDES
    next := Next;
  END;
  
PROCEDURE Iterate (st: Storage.T): Iterator =
  BEGIN
    Thread.Acquire(mu);
    RETURN NEW(XIterator, hashIdx := 0, buf := NIL, st := st);
  END Iterate;

PROCEDURE NextSub (itr: XIterator; VAR t: T): BOOLEAN =
  BEGIN
    IF itr.buf = NIL THEN
      FOR i := itr.hashIdx TO TableSize-1 DO
	IF hashTable[i] # NIL THEN
	  itr.hashIdx := i+1;
	  itr.buf := hashTable[i].hashLink;
	  t := hashTable[i];
	  RETURN TRUE;
	END;
      END;
      RETURN FALSE;
    ELSE
      t := itr.buf;
      itr.buf := itr.buf.hashLink;
      RETURN TRUE;
    END;
  END NextSub;

PROCEDURE Next (itr: XIterator; VAR t: T): BOOLEAN =
  BEGIN
    IF itr.st = NIL THEN
      RETURN NextSub(itr, t);
    ELSE
      WHILE NextSub(itr, t) DO
	IF t.st = itr.st THEN
	  RETURN TRUE;
	END;
      END;
      RETURN FALSE;
    END;
  END Next;
  
PROCEDURE EndIterate () =
  BEGIN
    <*ASSERT NOT Mutex.TryLock(mu)*>
    Thread.Release(mu);
  END EndIterate;

VAR
  diffMu := NEW(MUTEX);
  diffFreePtr: [-1 .. 63] := -1;
  diffBufs: ARRAY [0 .. 63] OF REF Diff;
  
PROCEDURE AllocDiff (): REF Diff =
  VAR buf: REF Diff;
  BEGIN
    LOCK diffMu DO 
      IF diffFreePtr = -1 THEN
	RETURN NEW(REF Diff);
      ELSE
	buf := diffBufs[diffFreePtr];
	diffBufs[diffFreePtr] := NIL;
	<*ASSERT buf # NIL*>
	diffFreePtr := VIEW(buf^, INTEGER);
	RETURN buf;
      END;
    END;
  END AllocDiff;

PROCEDURE FreeDiff (buf: REF Diff) =
  BEGIN
    LOCK diffMu DO
      IF diffFreePtr = -1 THEN
	diffBufs[LAST(diffBufs)] := buf;
	VIEW(buf^, INTEGER) := -1;
	diffFreePtr := LAST(diffBufs);
      ELSE
	FOR i := 0 TO LAST(diffBufs) DO
	  IF diffBufs[i] = NIL THEN
	    diffBufs[i] := buf;
	    VIEW(buf^, INTEGER) := diffFreePtr;
	    diffFreePtr := i;
	    RETURN;
	  END;
	END;
	Debugger.Enter();
      END;
    END;
  END FreeDiff;
  
BEGIN
  mu := NEW(MUTEX);
  nPages := 0;
END Buffer.



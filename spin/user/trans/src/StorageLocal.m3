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
MODULE StorageLocal;
IMPORT SID;
IMPORT TID;
IMPORT RawIO;
IMPORT WAL;
IMPORT Buffer, BufferQ, BufferRep;
IMPORT StorageProtected, StorageRep;
IMPORT SIDRefTbl;
IMPORT Error;
IMPORT LockMan;
IMPORT LockMode;
IMPORT LockRecord;
IMPORT ActiveTrans, ActiveTransRep, ActiveTransQ;
IMPORT Text;
IMPORT IO;
IMPORT Fmt;
IMPORT CheckPoint;
IMPORT TransOS;
IMPORT HostID;
IMPORT Transaction;
IMPORT TransServiceRep;
IMPORT TransT, TransPrivate, TransRep;
IMPORT Mutex, Thread;
IMPORT TransMode;
IMPORT TransGroup, TransGroupPrivate;
IMPORT VMError, VMDebug;
IMPORT MemoryObject;
IMPORT TransPager;
IMPORT ShadowPager;
IMPORT TransCache;
IMPORT PhysAddr;
IMPORT CPU;
IMPORT Debugger;

FROM TransUtils IMPORT Msg, Debug, GatherStats;
  
CONST PageSize = CPU.PAGESIZE;

REVEAL T = TPublic BRANDED OBJECT
OVERRIDES
  close := Close;
  prepare := Prepare;
  commit := Commit;
  onePhaseCommit := OnePhaseCommit;
  abort := Abort;
  flushLog := FlushLog;
  writeRedo := WriteRedo;
  pinFrame := PinFrame;
END;

(* Get the log device for the "fileName" on the host "hid".
 
   We really don't need "hid", because we are surely running on
   my own machine.

   In the future, this procedure does some kind of config file lookup, but
   now, we just return a fixed log device per site.
 
   XXX This crap has to be renovated. 
 *)
PROCEDURE GetLogDevice (<*UNUSED*>fileName: TEXT; hid: HostID.T): WAL.T =
  BEGIN
    IF hid = HostID.myID THEN
      RETURN Transaction.GetDefaultLogDevice();
    ELSE
      VAR logName := "log." & Fmt.Int(hid, 16);
      BEGIN
	RETURN WAL.Open(logName);
      END;
    END;
  END GetLogDevice;

  
PROCEDURE Open (fileName: TEXT; log: WAL.T): T RAISES {Error.E} =
  VAR
    st: T;
    r: REFANY;
    id := TransOS.FileNameToSID(fileName);
    byteSize: CARDINAL;
  BEGIN
    IF log = NIL THEN 
      log := GetLogDevice(fileName, HostID.myID);
    END;
    
    LOCK openFilesMu DO
      IF openFiles.get(id, r) THEN
	st := NARROW(r, T);
	INC(st.refCnt);
      ELSE
	st := NEW(T,
		  fh := RawIO.Open(fileName, 16_200000),
		  log := log,
		  refCnt := 1);
	<*ASSERT st.fh # NIL*>
	

	st.pager := TransPager.Create(st);
	byteSize := RawIO.Stat(st.fh).size;
	EVAL st.init(id, fileName);
	
	TRY
	  st.memObj := NEW(MemoryObject.T).init(byteSize DIV PageSize,
						st.pager,
						TransCache.Create(st));
	  st.shadowObj := NEW(MemoryObject.T).init(byteSize DIV PageSize,
						   ShadowPager.Create(st),
						   TransCache.Create(st));
	EXCEPT
	| VMError.E(e) =>
	  Msg("StorageLocal.Open", VMError.Message(e));
	END;
	(* If the filename begins with "F", this storage fails in
	   the prepare phase. This crap is here only for debugging purpose *)
	IF Text.Equal(Text.Sub(fileName, 0, 1), "F") THEN
	  st.fail := TRUE;
	END;
      
	EVAL openFiles.put(id, st);
      END;
    END;
    
    RETURN st;
  END Open;

PROCEDURE Abort (st: T; tr: TransT.T) =
  VAR
    notused: REFANY;
    at: ActiveTrans.T;
    buf: Buffer.T;
  BEGIN
    <*ASSERT openFiles.get(st.id, notused) AND notused = st*>

    st.lock();

    at := StorageProtected.FindTrans(st, tr);
    <*ASSERT at # NIL*>

    (* First, roll back the dirty pages in core.
       XXX below code is not thread-safe.*)
    LOOP
      buf := BufferQ.RemoveHeadSafe(at.pages);
      IF buf = NIL THEN EXIT; END;
      <*ASSERT buf.at = at*>
      buf.at := NIL;
      <*ASSERT buf.shadow # NIL*>
      PhysAddr.Copy(buf.frame, buf.shadow);
    END;

    (* Then, roll back the pages that were already flushed out to disk.
       Here, we traverse the log records for this transaction from the
       tail to the head order, and if UNDO record is found,
       apply it to the storage contents. *)
    IF at.needUndoLogScan THEN
      VAR
	log:= NEW(REF ARRAY OF CHAR, WAL.BlockSize);
	logSize: INTEGER;
	hdr: WAL.CommonHeader;
	lsn: WAL.LSN;
        type: WAL.Type;
	logIdx: INTEGER;
	start: INTEGER;
      BEGIN
	lsn := at.lastLSN;
      
	WHILE lsn # -1 DO 
	  logSize := WAL.Read(st.log, lsn, log);
	  <*ASSERT logSize <= NUMBER(log^)*>
	  EVAL WAL.ViewCommonHeader(log^, hdr, 0);
	  type := VAL(hdr.type, WAL.Type);
      
	  <*ASSERT type = WAL.Type.SPrepare
	        OR type = WAL.Type.Undo
	        OR type = WAL.Type.Redo
	        OR type = WAL.Type.EOB *>
      
	  IF type = WAL.Type.Undo THEN
	    (* roll back *)
	    logIdx := BYTESIZE(WAL.CommonHeader);
	    start := WAL.ViewWord64(log^, logIdx);
	    (* Now, log[logIdx .. logIdx+hdr.size-24] holds the
	       undo content.
	    *)
	    at.lastLSN := WAL.WriteRedo(st.log, st.id, tr.tid,
					WAL.Type.Redo, at.lastLSN, start,
					SUBARRAY(log^, logIdx, hdr.size-24));
	    StorageProtected.ModifyPage(st, start, SUBARRAY(log^, logIdx, hdr.size-24));
	  ELSE
	    (* ignore other records *)
	  END;
	  lsn := hdr.prevLSN;
	END;
      END;
    END;
    
    EVAL WAL.WriteCommit(st.log, st.id, tr.tid, WAL.Type.SAbort, at.lastLSN,
			 NullArray);
    
    LockMan.UnlockAll(at, st);
    ActiveTrans.Destroy(at);
    TransPager.InvalidateMappingsForSpace(st.pager, tr.group);
    TransPrivate.Unjoin(tr, st);
    st.unlock();
  END Abort;

PROCEDURE Prepare (st: T; tr: TransT.T; VAR lsn: WAL.LSN): BOOLEAN =
  CONST
    LockSize = BYTESIZE(WAL.PrepareLockEntry);
    LockMax = 64;
  VAR
    notused: REFANY;
    at: ActiveTrans.T;
    ent: ARRAY [0 .. LockMax*LockSize + BYTESIZE(INTEGER)-1] OF CHAR;
    longEnt: REF ARRAY OF CHAR;
    
  PROCEDURE GetNumberOfLockRecords (): CARDINAL =
    (* XXX this is very dumb. We should keep the # of locks in ActiveTrans. *)
    VAR
      itr := LockMan.Iterate(at, st);
      lock: LockRecord.T;
      n: CARDINAL := 0;
    BEGIN
      WHILE LockMan.NextItr(itr, lock) DO
	INC(n);
      END;
      RETURN n;
    END GetNumberOfLockRecords;
    
  PROCEDURE FillInLockEntries (n: CARDINAL; VAR buf: ARRAY OF CHAR) =
    VAR
      itr := LockMan.Iterate(at, st);
      lock: LockRecord.T;
      idx: CARDINAL;
    BEGIN
      VIEW(buf, INTEGER) := n;
      idx := BYTESIZE(INTEGER);
      WHILE LockMan.NextItr(itr, lock) DO
	WITH rec = VIEW(SUBARRAY(buf, idx, LockSize), WAL.PrepareLockEntry) DO
	  rec.from := lock.from;
	  rec.end := lock.end;
	  rec.mode := ORD(lock.mode);
	END;
	INC(idx, LockSize);
      END;
    END FillInLockEntries;
  BEGIN
    <*ASSERT openFiles.get(st.id, notused) AND notused = st*>
    IF Debug AND st.fail THEN RETURN FALSE; END;

    st.lock();

    at := StorageProtected.FindTrans(st, tr);
    
    EVAL StorageProtected.FlushModificationsForTrans(st, at);

    (* Get the list of all the locks held by the transaction into
       either "ent" or "longEnt", depending on the # of the locks.
       Then output them to log also, because we need to reestablish the
       locks in case transaction needs to be resolved at recovery time. *)
    WITH nLocks = GetNumberOfLockRecords() DO
      IF nLocks <= LockMax THEN
	FillInLockEntries(nLocks, ent);
	at.lastLSN := WAL.WriteCommit(st.log, st.id, tr.tid,
				      WAL.Type.SPrepare, at.lastLSN,
				      SUBARRAY(ent, 0, nLocks*LockSize+BYTESIZE(INTEGER)));
      ELSE
	longEnt := NEW(REF ARRAY OF CHAR, nLocks*LockSize + BYTESIZE(INTEGER));
	FillInLockEntries(nLocks, longEnt^);
	at.lastLSN := WAL.WriteCommit(st.log, st.id, tr.tid,
				      WAL.Type.SPrepare, at.lastLSN,
				      longEnt^);
      END;
    END;
    lsn := at.lastLSN;
    at.state := CheckPoint.RState.Prepared;
    st.unlock();
    RETURN TRUE;
  END Prepare;

PROCEDURE PrintStats (st: T; at: ActiveTrans.T) =
  BEGIN
    Msg(st.name, ": " & Fmt.Int(at.nPagesMapped)&" pages mapped.");
    Msg(st.name, ": " & Fmt.Int(at.nPagesModified)&" pages modified.");
    Msg(st.name, ": " & Fmt.Int(at.nRedoLogBytes)&" redo bytes.");
    Msg(st.name, ": " & Fmt.Int(at.nUndoLogBytes)&" undo bytes.");
    Msg(st.name, ": " & Fmt.Int(at.nRedoLogBytes + at.nUndoLogBytes),
	" total mod bytes.");
    Msg(st.name, ": " & Fmt.Int(at.nPagesPagedIn) &" pages paged in.");
    Msg(st.name, ": " & Fmt.Int(at.nPagesPurged) &" pages purged.");
  END PrintStats;
  
PROCEDURE Commit (st: T; tr: TransT.T; VAR lsn: WAL.LSN) =
  VAR
    notused: REFANY;
    at: ActiveTrans.T;
  BEGIN
    <*ASSERT openFiles.get(st.id, notused) AND notused = st*>
    LOCK st.mu DO
      at := StorageProtected.FindTrans(st, tr);
      <*ASSERT at.state = CheckPoint.RState.Prepared *>
      lsn := WAL.WriteCommit(st.log, st.id, tr.tid, WAL.Type.SCommit,
			     at.lastLSN, NullArray);
      LockMan.UnlockAll(at, st);
      ActiveTrans.Destroy(at);
      TransPager.InvalidateMappingsForSpace(st.pager, tr.group);
      TransPrivate.Unjoin(tr, st);
      
      IF GatherStats THEN PrintStats(st, at); END;
    END;
  END Commit;

PROCEDURE OnePhaseCommit (st: T; tr: TransT.T; VAR lsn: WAL.LSN): BOOLEAN =
  VAR
    notused: REFANY;
    at: ActiveTrans.T;
  BEGIN
    <*ASSERT openFiles.get(st.id, notused) AND notused = st*>
    IF Debug AND st.fail THEN RETURN FALSE; END;
    
    LOCK st.mu DO
      at := StorageProtected.FindTrans(st, tr);

      (* We write the commit record only when the storage is
	 modified. *)
      lsn := 0;
      IF StorageProtected.FlushModificationsForTrans(st, at) THEN
	lsn := WAL.WriteCommit(st.log, st.id, tr.tid,
			       WAL.Type.S1PCommit, at.lastLSN,
			       NullArray);
      END;
      TransPager.InvalidateMappingsForSpace(st.pager, tr.group);
      LockMan.UnlockAll(at, st);
      TransPrivate.Unjoin(tr, st);
      ActiveTrans.Destroy(at);
      IF GatherStats THEN PrintStats(st, at); END;
    END;
    RETURN TRUE;
  END OnePhaseCommit;

PROCEDURE FlushLog (st: T; lsn: INTEGER) =
  BEGIN
    WAL.Flush(st.log, lsn);
  END FlushLog;
  
CONST NullArray = ARRAY [0 ..-1] OF CHAR {};
    
PROCEDURE Close (st: T; group: TransGroup.T) =
  VAR
    itr: Buffer.Iterator;
    buf: Buffer.T;
    notused: REFANY;
  BEGIN
    <*ASSERT openFiles.get(st.id, notused) AND notused = st*>
    
    LOCK st.mu DO
      DEC(st.refCnt);
      IF st.refCnt = 0 THEN
	IO.Put("Shutting down " & Fmt.Int(st.id.lid) & ".\n");
	
	(* Scan through every page of this storage and write
	   it back to the disk if it's modified *)
	itr := Buffer.Iterate(st);
	WHILE itr.next(buf) DO
	  (* The below assertion is not a safe one, in a sense that a 
	     rogue client can violate it only by using public interfaces.
	     Therefore, we really should raise exception instead of just
	     dieing here. *)
	  <*ASSERT NOT Buffer.IsLocked(buf)*>
	  IF Debug THEN
	    (* Check the checksum *)
	    IF st.checkSum = NIL THEN
	      st.checkSum := NEW(REF ARRAY OF INTEGER,
				 st.memObj.stat().virtualSize);
	    END;
	    WITH sum = VMDebug.CalculateChecksum(buf.frame) DO 
	      IF buf.dirty THEN
		st.checkSum[buf.pos] := sum;
	      ELSE
		IF st.checkSum[buf.pos] # 0
		  AND st.checkSum[buf.pos] # sum THEN
		  Msg("close: checksum altered.\n");
		  Debugger.Enter();
		END;
	      END;
	    END;
	  END;
	  
	  IF buf.dirty THEN
	    (* since where should be no transaction going on this
	     storage, we don't have to flush the log here *)
	    Buffer.Write(buf);
	    buf.dirty := FALSE;
	  END;
	END;
	Buffer.EndIterate();
	
	(* Write the ShutDown log record *)
	EVAL WAL.WriteOther(st.log, st.id, WAL.Type.ShutDown, NullArray);
	WAL.Flush(st.log);

	(* Re-open the file to flush the contents. This is needed only
	   on OSF where RawiO.Write is not synchronous. But I do this
	   on SPIN also because it doesn't hurt performance anyway. *)
	RawIO.Close(st.fh);
	st.fh := RawIO.Open(st.name, 0);
	
	(* Finished closing. Remove the entry from openFiles. *)
	TransGroupPrivate.DeleteResource(group, st);
	
	(* Note: the storage is not deleted from the openfiles.
	   It is deferred until all the buffers belonging to the storage
	   is purged out(but this is not implemented, so storage stays
	   there forever once opened.) *)
      END;
    END;
  END Close;
  
PROCEDURE PinFrame (st: T; at: ActiveTrans.T; lock: LockMode.T;
		    page: Buffer.Offset; VAR allocate: BOOLEAN;
		    frame: PhysAddr.T): Buffer.T RAISES {VMError.E} =
  VAR
    buf: Buffer.T;
    status: INTEGER;
  PROCEDURE WritePageToLog (VAR x: PhysAddr.Content) =
    BEGIN
      at.lastLSN := st.writeRedo(at.tr.tid, WAL.Type.Undo, at.lastLSN,
				 buf.pos, x);
    END WritePageToLog;
  PROCEDURE Callback (VAR x: PhysAddr.Content) =
    BEGIN
      EVAL RawIO.Read(st.fh, x, buf.pos*CPU.PAGESIZE);
    END Callback;
  BEGIN
    <*ASSERT st.isLocked()*>

    LOOP
      buf := Buffer.Lock(st, page);
      IF buf # NIL THEN
	allocate := FALSE;
	EXIT;
      END;
      IF NOT allocate THEN
	RETURN NIL;
      END;
      allocate := TRUE;
      buf := Buffer.Allocate(st, page, frame);
      (* read the disk contents into the frame. *)
      PhysAddr.Access(buf.frame, Callback);

      IF Debug THEN
	(* Check the checksum *)
	IF st.checkSum = NIL THEN
	  st.checkSum := NEW(REF ARRAY OF INTEGER,
			     st.memObj.stat().virtualSize);
	END;
	IF st.checkSum[buf.pos] # 0
	  AND st.checkSum[buf.pos] # VMDebug.CalculateChecksum(buf.frame) THEN
	  Msg("pagein: checksum altered.\n");
	  Debugger.Enter();
	END;
      END;
	  
      IF Buffer.Intern(buf) THEN
	(* Conflict! someone inserted the page while I'm working on it.
	   Discard the page and start over again. *)
	Msg("StorageLocal.PinFrame: intern failed.\n");
	IF frame = NIL THEN
	  (* Free the frame only if I allocated it. *)
	  PhysAddr.Deallocate(buf.frame);
	END;
	(* "buf" itself will be GCed. *)
      ELSE
	EXIT;
      END;
    END;
    
    IF lock # LockMode.T.None THEN
      IF NOT (TransMode.T.NoLogging IN at.tr.mode) THEN
	status := LockMan.Lock(at, st, lock, page, page+1, LAST(INTEGER));
	IF status # 0 THEN 
	  Msg("storagelocal.pinframe:lock", Fmt.Int(status));
	  RAISE VMError.E(0);
	END;
	IF lock = LockMode.T.Write THEN
	  IF TransMode.T.PageGrainLogging IN at.tr.mode THEN
	    buf.dirty := TRUE;
	    PhysAddr.Access(buf.frame, WritePageToLog);
	  ELSE
	    Buffer.Shadow(buf, at);
	  END;
	END;
      ELSE
	buf.dirty := TRUE;
      END;
    END;

    RETURN buf;
  END PinFrame;

PROCEDURE WriteRedo (st: T; tid: TID.T; type: WAL.Type;
		     lsn: WAL.LSN; pos: CARDINAL;
		     READONLY image: ARRAY OF CHAR): WAL.LSN =
  BEGIN
    RETURN WAL.WriteRedo(st.log, st.id, tid, type, lsn, pos, image);
  END WriteRedo;

PROCEDURE FreeupLogSpace (): WAL.LSN =
  VAR
    itr := Buffer.Iterate(NIL);
    buf: Buffer.T;
    minLSN := LAST(WAL.LSN);
    (* "minLSN" is LSN up from which is (potentially) used. *)
    r: REFANY;
    st: T;
    stItr: SIDRefTbl.Iterator;
    sid: SID.T;
    at: ActiveTrans.T;
    atItr: ActiveTransQ.Iterator;
    locked: BOOLEAN;
  BEGIN
    WHILE itr.next(buf) DO
      IF buf.at = NIL THEN
	(*IF buf.dirty THEN
	  Buffer.Write(buf);
	  buf.dirty := FALSE;
	END;*)
      ELSE
	(* Has to wait..
	   the first log record that made this buffer dirty
	   (ie, "buf.recoveryLSN") cannot be discarded because
	   it might be used in case transaction is aborted. *)
	IF buf.recoveryLSN # 0 THEN
	  minLSN := MIN(minLSN, buf.recoveryLSN);
	END;
      END;
    END;
    Buffer.EndIterate();

    (* Look at all the outstanding transactions, and get the minimum
       firstLSN of all ActiveTrans'. *)
    LOCK openFilesMu DO
      stItr := openFiles.iterate();
      WHILE stItr.next(sid, r) DO
	st := r;
	locked := Mutex.TryLock(st.mu);
	(* st.lock(); *) (* XXX this might lead to deadlock.
			    We need to compute the value offline somehow. *)
	atItr := StorageProtected.IterateTrans(st);
	WHILE ActiveTransQ.NextItr(atItr, at) DO
	  IF at.firstLSN # 0 THEN
	    minLSN := MIN(minLSN, at.firstLSN);
	  END;
	END;
	IF locked THEN Thread.Release(st.mu); END;
      END;
    END;
    RETURN minLSN;
  END FreeupLogSpace;
  
PROCEDURE Poll(<*UNUSED*>sid: SID.T; <*UNUSED*>tid: TID.T) =
  BEGIN
    <*ASSERT FALSE*>
  END Poll;

BEGIN
  HostID.Init();
  openFiles := NEW(SIDRefTbl.Default).init();
  openFilesMu := NEW(MUTEX);
END StorageLocal.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE Storage EXPORTS Storage, StorageProtected;
IMPORT Debugger;
IMPORT StorageRep;
IMPORT SID;
IMPORT TransService, TransServiceRep;
IMPORT Thread;
IMPORT Mutex;
IMPORT Text;
IMPORT StorageProxy;
IMPORT StorageLocal;
IMPORT Error;
IMPORT TransGroup, TransGroupPrivate;
IMPORT TransT, TransRep, TransPrivate;
IMPORT PhysAddr;
IMPORT Buffer, BufferQ, BufferRep;
IMPORT Lex, Scan;
IMPORT LockMode, LockQ;
IMPORT ActiveTrans, ActiveTransRep, ActiveTransQ;
IMPORT Spy;
IMPORT VMError;
IMPORT TransMode;<*NOWARN*>
<*NOWARN*>IMPORT IO, Fmt;
IMPORT WAL;
FROM TransUtils IMPORT Debug, Msg, SpyTime;

REVEAL T = StorageRep.TPublic BRANDED OBJECT
OVERRIDES
  init := Init;
  lock := Lock;
  unlock := Unlock;
  isLocked := IsLocked;
  access := Access;
  accessRO := AccessRO;
  operateOnRegion := OperateOnRegion;
END;

VAR
  accessTimer: Spy.T;
  
PROCEDURE Open (fileName: TEXT; host: TransGroup.T): T RAISES {Error.E}=
  VAR
    colonPos := Text.FindChar(fileName, ':');
    hostName: TEXT;
    port: CARDINAL := 0;
    st: T;
  BEGIN
    IF colonPos >= 0 THEN
      hostName := Text.Sub(fileName, 0, colonPos);
      fileName := Text.Sub(fileName, colonPos+1);
      colonPos := Text.FindChar(fileName, ':');
      IF colonPos >= 0 THEN
	(* port name specified. *)
	TRY
	  port := Scan.Int(Text.Sub(fileName, 0, colonPos));
	EXCEPT
	| Lex.Error =>
	  Msg("storage.open: \"", Text.Sub(fileName, 0, colonPos), 
	      "\" is not a number.");
	  port := 0;
	END;
	fileName := Text.Sub(fileName, colonPos+1);
      END;
      st := StorageProxy.Open(hostName, port, fileName);
    ELSE
      st := StorageLocal.Open(fileName);
    END;
    TransGroupPrivate.RegisterResource(host, st);
    RETURN st;
  END Open;
  
PROCEDURE Init (st: T; sid: SID.T; fileName: TEXT): TransService.T =
  BEGIN
    st.mu := NEW(MUTEX);
    st.activeTrans := ActiveTransQ.NewHeader();
    st.lockMu := NEW(MUTEX);
    st.sync := NEW(Thread.Condition);
    st.locks := LockQ.NewHeader();
    st.name := fileName;
    st.id := sid;
    RETURN st;
  END Init;

PROCEDURE IsLocked (st: T): BOOLEAN =
  BEGIN
    RETURN NOT Mutex.TryLock(st.mu);
  END IsLocked;
  
PROCEDURE Lock (st: T) =
  BEGIN
    Thread.Acquire(st.mu);
  END Lock;

PROCEDURE Unlock(st: T) =
  BEGIN
    Thread.Release(st.mu);
  END Unlock;
    
  
PROCEDURE OperateOnRegion (st:T; at: ActiveTrans.T;
			   lock: LockMode.T; from, end: INTEGER; 
			   callback: PROCEDURE(VAR c: ARRAY OF CHAR;
					       buf: Buffer.T; pos: INTEGER)) =
  VAR
    pos, off, len: INTEGER;
    buf: Buffer.T;
    allocate: BOOLEAN;
    PROCEDURE Trampolin (VAR c: PhysAddr.Content) =
      BEGIN
	IF SpyTime THEN Spy.Exit(accessTimer); END;
	callback(SUBARRAY(c, off, len), buf, pos);
	IF SpyTime THEN Spy.Enter(accessTimer); END;
      END Trampolin;
  BEGIN
    <*ASSERT st.isLocked()*>
    LOOP
      TRY
	pos := from;
	WHILE pos < end DO
	  off := pos MOD PageSize;
	  len := MIN(PageSize-off, end-pos);
	  
	  allocate := TRUE;
	  buf := st.pinFrame(at, lock, pos DIV PageSize, allocate, NIL);
	  IF SpyTime THEN Spy.Enter(accessTimer); END;
	  TRY
	    PhysAddr.Access(buf.frame, Trampolin);
	  FINALLY 
	    Buffer.Unlock(buf);
	  END;
	  IF SpyTime THEN Spy.Exit(accessTimer); END;
	  INC(pos, len);
	END;
	EXIT; (* LOOP *)
      EXCEPT
      | VMError.E(e) =>
	Msg("Storage.OperateOnRegion: ", VMError.Message(e));
      END;
    END;
  END OperateOnRegion;

PROCEDURE Access (st: T; tr: TransT.T; from, len: CARDINAL;
		  callback: PROCEDURE (VAR x: ARRAY OF CHAR; pos: CARDINAL)) =
  VAR
    pos, end: CARDINAL;
    at: ActiveTrans.T;
  PROCEDURE Trampolin(VAR c: ARRAY OF CHAR;
		      <*UNUSED*>buf: Buffer.T; pos: INTEGER) =
    BEGIN
      callback(c, pos);
    END Trampolin;
  BEGIN
    LOCK st.mu DO
      at := InternTrans(st, tr);
    END;
    pos := from DIV PageSize;
    end := (from+len-1) DIV PageSize + 1;
    
    LOCK st.mu DO 
      st.operateOnRegion(at, LockMode.T.Write, from, from+len, Trampolin);
    END;
  END Access;
    
PROCEDURE AccessRO (st: T; tr: TransT.T; from, len: CARDINAL;
		    callback: PROCEDURE (READONLY x: ARRAY OF CHAR; pos: CARDINAL)) =
  VAR at: ActiveTrans.T;
  PROCEDURE Trampolin(VAR c: ARRAY OF CHAR;
		      <*UNUSED*>buf: Buffer.T; pos: INTEGER) =
    BEGIN
      callback(c, pos);
    END Trampolin;
  BEGIN
    LOCK st.mu DO
      at := InternTrans(st, tr);
      st.operateOnRegion(at, LockMode.T.Read, from, from+len, Trampolin);
    END;
  END AccessRO;
  
PROCEDURE FindTrans (st: T; tr: TransT.T): ActiveTrans.T =
  VAR
    at : ActiveTrans.T := st.activeTrans.next;
  BEGIN
    WHILE at # st.activeTrans DO
      IF at.tr = tr THEN RETURN at; END;
      at := at.next;
    END;
    RETURN NIL;
  END FindTrans;

(* Pre: st is locked *)
PROCEDURE InternTrans (st: T; tr: TransT.T): ActiveTrans.T =
  VAR
    at : ActiveTrans.T := st.activeTrans.next;
  BEGIN
    WHILE at # st.activeTrans DO
      IF at.tr = tr THEN RETURN at; END;
      at := at.next;
    END;
    at := ActiveTransQ.Allocate();
    ActiveTrans.Init(at, tr);
    TransPrivate.Join(tr, st);
    ActiveTransQ.InsertTail(st.activeTrans, at);
    RETURN at;
  END InternTrans;

PROCEDURE IterateTrans (st: T): Iterator =
  BEGIN
    IF Debug AND NOT IsLocked(st) THEN
      Debugger.Enter();
    END;
    RETURN ActiveTransQ.Iterate(st.activeTrans);
  END IterateTrans;

PROCEDURE FlushModificationsForTrans (st: T; at: ActiveTrans.T): BOOLEAN =
  VAR
    buf: Buffer.T;
    diff := Buffer.AllocDiff();
    diffSize: CARDINAL;
    rc := FALSE;
  PROCEDURE WritePageToLog (VAR x: PhysAddr.Content) =
    BEGIN
      at.lastLSN := st.writeRedo(at.tr.tid, WAL.Type.Redo, at.lastLSN,
				 buf.pos, x);
    END WritePageToLog;
  BEGIN
    LOOP
      buf := BufferQ.RemoveHeadSafe(at.pages);
      IF buf = NIL THEN EXIT; END;
      <*ASSERT buf.at = at*>
      buf.at := NIL;
      rc := TRUE;
      IF buf.dirty THEN
	IF buf.shadow # NIL THEN
	  <*ASSERT NOT(TransMode.T.PageGrainLogging IN at.tr.mode)*>	  
	  diffSize := Buffer.NormalDiff(buf, diff^);
	  IF diffSize > 0 THEN 
	    INC(at.nRedoLogBytes, diffSize);
	    at.lastLSN := st.writeRedo(at.tr.tid, WAL.Type.Redo, at.lastLSN,
				       buf.pos, SUBARRAY(diff^, 0, diffSize));
	  END;
	ELSE
	  <*ASSERT TransMode.T.PageGrainLogging IN at.tr.mode*>
	  INC(at.nRedoLogBytes, PageSize);
	  PhysAddr.Access(buf.frame, WritePageToLog);
	END;	  
	buf.lsn := at.lastLSN;
	IF buf.recoveryLSN = 0 THEN
	  buf.recoveryLSN := buf.lsn;
	END;
      END;
    END;
    Buffer.FreeDiff(diff);
    RETURN rc;
  END FlushModificationsForTrans;
  
(* Modify the contents of the region starting from POS directly by
   copying "data" on that region. *)
PROCEDURE ModifyPage (st: T; pos: Buffer.Offset;
		      READONLY data: ARRAY OF CHAR; lsn: WAL.LSN) =
  VAR
    buf: Buffer.T;
    allocate := TRUE;
  BEGIN
    buf := st.pinFrame(NIL, LockMode.T.None, pos, allocate, NIL);
    Buffer.Patch(buf, data);
    buf.dirty := TRUE;
    buf.lsn := lsn;
    IF buf.recoveryLSN = 0 THEN buf.recoveryLSN := lsn; END;
    Buffer.Unlock(buf);
  END ModifyPage;


			   
BEGIN
  IF SpyTime THEN
    accessTimer := Spy.Create("trans:physaccess", FALSE);
  END;
END Storage.
















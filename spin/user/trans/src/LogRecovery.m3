(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(*
   The recovery algorithm is based on ARIES, but no compensation record
   is output on undo. I hope this is ok because CLR is only used to speed up
   recovery from failure during previous recovery,
   which may not occur so often.
*)

MODULE LogRecovery EXPORTS WAL, WALPrivate;
IMPORT SID;
IMPORT CheckPoint;
IMPORT StorageLocal;
IMPORT TID;
IMPORT IO, Fmt, Round;
IMPORT TransPrivate;
IMPORT SIDRefTbl;
IMPORT IntRefTbl;
<*NOWARN*>FROM TransUtils IMPORT Msg, Msg1, Debug, DebugMsg;

(* Locate the last check point record that appears before LSN. *)
PROCEDURE LocateCheckPointRecord (log: T; lsn: LSN;
				  (*OUT*)VAR hdr: RawHeader): BOOLEAN =
  BEGIN
    WHILE ReadPrevLogWRTStorage(log, SID.Void, lsn, hdr) DO
      IF hdr.type = ORD(Type.CheckPoint) THEN
	RETURN TRUE;
      END;
      lsn := hdr.lsn;
    END;
    RETURN FALSE;
  END LocateCheckPointRecord;

PROCEDURE Recover (log: T) =
  VAR
    (* The name is confusing, but lastLSN is the last possible record
       we have to look at *)
    lastLSN := log.startLSN;
    hdr: RawHeader;

    info: CheckPoint.T;
    (* Note: We don't need firstUndoLSN, since undo record is scanned
       from last-to-first order, and we can detect the beginning of
       the log record of particular transaction by checking if prevLSN = -1
     *)
  BEGIN
    (* log records do not exist *)
    IF lastLSN = -1 THEN RETURN; END;
    IF DebugMsg THEN Msg1("Locating the checkpoint record.."); END;
    
    IF LocateCheckPointRecord(log, log.startLSN, hdr) THEN
      IF DebugMsg THEN IO.Put("found at " & Fmt.Int(hdr.lsn) & ".\n"); END;
      CheckPoint.Undump(log, hdr.lsn, info);
    ELSE
      (* When we can't find checkpoint record, we see everything from the
       beginning.
       *)
      IF DebugMsg THEN IO.Put("not found.\n"); END;
      info.trans := NEW(IntRefTbl.Default).init();
      info.storage := NEW(SIDRefTbl.Default).init();
      info.firstAnalysisLSN := log.firstLSN;
      info.firstRedoLSN := log.firstLSN;
    END;

    IF DebugMsg THEN 
      Msg("1. Starting analysis from ", Fmt.Int(info.firstAnalysisLSN), "\n");
    END;
    Analyze(log, info.firstAnalysisLSN, log.startLSN, info);

    StorageLocal.RecoveryStart(log, info);
    
    IF DebugMsg THEN
      Msg("2. Redoing committed transactions from ",
	  Fmt.Int(info.firstRedoLSN), "\n");
    END;
    
    Redo(log, info.firstRedoLSN, log.startLSN, info);

    (* At this point, we spawn off threads that
       query the outcome of in-doubt transactions *)
    VAR
      stItr := info.storage.iterate();
      r: REFANY;
      sid: SID.T;
      st: CheckPoint.Storage;
      trItr: IntRefTbl.Iterator;
      tid: INTEGER;
      tr: REF CheckPoint.RemoteTrans;
    BEGIN
      WHILE stItr.next(sid, r) DO
	st := r;
	trItr := st.trans.iterate();
	WHILE trItr.next(tid, r) DO
	  tr := r;
	  IF tr.state = CheckPoint.RState.Prepared THEN
	    StorageLocal.ResolveUncommitedTrans(log, sid, tid, tr);
	  END;
	END;
      END;
    END;

    IF DebugMsg THEN Msg("3. Undoing uncommitted transactions"); END;
    Undo(log, log.startLSN, info);
    
    IF DebugMsg THEN Msg("4. Finished."); END;
    StorageLocal.RecoveryFinished(log, info);
    TransPrivate.RecoveryFinished(log, info);
  END Recover;

PROCEDURE Redo (log: T; lsn, lastLSN: LSN; READONLY info: CheckPoint.T) =
  VAR
    buf := NEW(REF ARRAY OF CHAR, BlockSize);
    recordSize: INTEGER;
    rawHdr: RawHeader;
    commonHdr: CommonHeader;
    pos: INTEGER;
    bufIdx: INTEGER;
    r: REFANY;
    st: CheckPoint.Storage;
    dp: REF CheckPoint.DirtyPage;
    result: RecoveryResult;
  BEGIN
    WHILE lsn # lastLSN DO
      <*ASSERT lsn < lastLSN*>
      recordSize := Read(log, lsn, buf);
      EVAL ViewRawHeader(buf^, rawHdr, 0);
      
      IF info.storage.get(rawHdr.sid, r) THEN
	st := NARROW(r, CheckPoint.Storage);
	<*ASSERT st.sid = rawHdr.sid*>
	
	CASE VAL(rawHdr.type, Type) OF
	| Type.Redo =>
	  EVAL ViewCommonHeader(buf^, commonHdr, 0);
	  bufIdx := BYTESIZE(commonHdr);
	  pos := ViewWord64(buf^, bufIdx);
	  
	  IF NOT st.dirtyPages.get(Round.DownToPage(pos), r) THEN
	    (* we don't have to do anything *)
	    IO.Put("redo : " & Fmt.Int(lsn) & " the page " & Fmt.Int(pos)
		   & " wasn't dirty\n");
	  ELSE
	    dp := NARROW(r, REF CheckPoint.DirtyPage);
	    IF dp.recoveryLSN > lsn THEN
	      IF DebugMsg THEN
		Msg("redo:", Fmt.Int(lsn), " recoverylsn>lsn");
	      END;
	    ELSE
	      IF DebugMsg THEN
		Msg("redo:", Fmt.Int(lsn), TypeName(VAL(rawHdr.type, Type)));
	      END;
	      result := StorageLocal.Redo(log, st,
					  SUBARRAY(buf^, bufIdx, commonHdr.size-24), pos);
	      IF result = RecoveryResult.NoSuchStorage THEN
		EVAL info.storage.delete(st.sid, r);
	      END;
	    END;
	  END;
	ELSE
	  (* Simply ignore other types of log records *)
	END;
      ELSE
	IF DebugMsg THEN
	  Msg("redo: ignoring sid=", Fmt.Int(rawHdr.sid.lid));
	END;
      END;
      lsn := GetNextLSN(rawHdr);
    END;
    buf := NIL;
  END Redo;

(* calculate the total number of outstanding remote transactions *)
PROCEDURE NOutstandingTransactions (READONLY info: CheckPoint.T): CARDINAL =
  VAR
    sid: SID.T;
    itr := info.storage.iterate();
    nTrans: CARDINAL := 0;
    r: REFANY;
  BEGIN
    WHILE itr.next(sid, r) DO
      WITH st = NARROW(r, CheckPoint.Storage) DO 
	INC(nTrans, st.trans.size());
      END;
    END;
    RETURN nTrans;
  END NOutstandingTransactions;
  

PROCEDURE Undo (log: T; lastLSN: LSN; READONLY info: CheckPoint.T) =
  VAR
    lsn := lastLSN;
    rawHdr: RawHeader;
    commonHdr: CommonHeader;
    buf := NEW(REF ARRAY OF CHAR, BlockSize);
    bufIdx: INTEGER;
    r: REFANY;
    pos: INTEGER;
    recordSize: INTEGER;
    st: CheckPoint.Storage;
    tr: REF CheckPoint.RemoteTrans;
    result: RecoveryResult;
  BEGIN
    (* Back up one *)
    IF NOT ReadPrevLog(log, lastLSN, rawHdr) THEN
      RETURN;
    END;
    lsn := rawHdr.lsn;
    
    WHILE NOutstandingTransactions(info) > 0 DO 
      recordSize := Read(log, lsn, buf);
      EVAL ViewRawHeader(buf^, rawHdr, 0);
    
      IF info.storage.get(rawHdr.sid, r) THEN
	st := NARROW(r, CheckPoint.Storage);
	<*ASSERT st.sid = rawHdr.sid*>
	
	CASE VAL(rawHdr.type, Type) OF
	| Type.Undo =>
	  EVAL ViewCommonHeader(buf^, commonHdr, 0);
	  bufIdx := BYTESIZE(commonHdr);
	  IF st.trans.get(commonHdr.tid, r) THEN
	    (* this transaction was active at the moment of the crash *)
	    tr := r;
	    
	    <*ASSERT tr.state = CheckPoint.RState.Active
	          OR tr.state = CheckPoint.RState.Prepared *>
	    
	    IF tr.state = CheckPoint.RState.Active THEN 
	      pos := ViewWord64(buf^, bufIdx);
	      result := StorageLocal.Undo(log, st, 
				  SUBARRAY(buf^, bufIdx, commonHdr.size-24),
					  pos);
	      IF result = RecoveryResult.NoSuchStorage THEN
		EVAL info.storage.delete(st.sid, r);
	      END;
	    END;
	    
	    (* A prepared transaction is undo'ed separately if it is
	       to be aborted, and it's the storage manager's responsibility. *)
	  END;
	  IF commonHdr.prevLSN = -1 THEN
	    IF DebugMsg THEN
	      Msg("undo:", Fmt.Int(lsn), ": this trans is complete");
	    END;
	    (* this is the first record of the transaction *)
	    EVAL st.trans.delete(commonHdr.tid, r);
	  END;
	| Type.Redo, Type.SCommit, Type.SAbort =>
	  IF DebugMsg THEN 
	    Msg("undo:", Fmt.Int(lsn), " ", TypeName(VAL(rawHdr.type, Type)));
	  END;
	  EVAL ViewCommonHeader(buf^, commonHdr, 0);
	  IF commonHdr.prevLSN = -1 THEN
	    (* this is the first record of the transaction *)
	    EVAL st.trans.delete(commonHdr.tid, r);
	  END;
	ELSE
	  IF DebugMsg THEN
	    Msg("undo:", Fmt.Int(lsn), " ignore ",
		TypeName(VAL(rawHdr.type, Type)));
	  END;
	END;
      ELSE
	(* Couldn't find the storage corresponding to "rawHdr.sid".
	   May be this storage is found to be deleted by some user.
	   (See storagelocal.undo/redo also). *)
	IF DebugMsg THEN
	  Msg("undo: ignoring sid=", Fmt.Int(rawHdr.sid.lid));
	END;
      END;	
      
      IF NOT ReadPrevLog(log, lsn, rawHdr) THEN
	EXIT;
      END;
      lsn := rawHdr.lsn;
    END;
  END Undo;
  
PROCEDURE InternTrans (st: CheckPoint.Storage; tid: TID.T; lsn: LSN)
	 : REF CheckPoint.RemoteTrans =
  VAR
    r: REFANY;
    t: REF CheckPoint.RemoteTrans;
  BEGIN
    (* If the tid is not registered in activeTrans, put it in. *)
    IF st.trans.get(tid, r) THEN
      t := NARROW(r, REF CheckPoint.RemoteTrans);
      <*ASSERT t.lastLSN < lsn *>
    ELSE
      t := NEW(REF CheckPoint.RemoteTrans, firstLSN := lsn);
      EVAL st.trans.put(tid, t);
    END;
    t.lastLSN := lsn;
    RETURN t;
  END InternTrans;

PROCEDURE Analyze (log: T; lsn, lastLSN: LSN; VAR info: CheckPoint.T) =
  VAR
    buf := NEW(REF ARRAY OF CHAR, BlockSize);
    bufIdx : INTEGER;
    recordSize: INTEGER;
    rawHdr: RawHeader;
    commonHdr: CommonHeader;
    r: REFANY;
    st: CheckPoint.Storage;
    page: INTEGER;
  BEGIN
    WHILE lsn # lastLSN DO
      <*ASSERT lsn < lastLSN*>
      recordSize := Read(log, lsn, buf);
      EVAL ViewRawHeader(buf^, rawHdr, 0);

      IF rawHdr.sid = SID.Void THEN
	st := NIL;
      ELSIF info.storage.get(rawHdr.sid, r) THEN
	st := r;
      ELSE
	st := NEW(CheckPoint.Storage).init(rawHdr.sid);
	EVAL info.storage.put(rawHdr.sid, st);
      END;
      
      (* IO.Put("anal: " & Fmt.Int(lsn) & " " & TypeName(VAL(rawHdr.type, Type)) & "\n"); *)
    
      CASE VAL(rawHdr.type, Type) OF
      | Type.Undo, Type.Redo =>
	VAR
	  rt: REF CheckPoint.RemoteTrans;
	  dpEntry: REF CheckPoint.DirtyPage;
	BEGIN
	  EVAL ViewCommonHeader(buf^, commonHdr, 0);
	  bufIdx := BYTESIZE(commonHdr);
	  rt := InternTrans(st, commonHdr.tid, commonHdr.lsn);
	  rt.state := CheckPoint.RState.Active;
	  
	  (* If the page is not in the dirty page list, put it in. *)
	  page := Round.DownToPage(ViewWord64(buf^, bufIdx));
	  IF NOT st.dirtyPages.get(page, r) THEN
	    dpEntry := NEW(REF CheckPoint.DirtyPage);
	    dpEntry.recoveryLSN := commonHdr.lsn;
	    EVAL st.dirtyPages.put(page, dpEntry);
	  END;
	END;
      | Type.SPrepare =>
	VAR
	  rt: REF CheckPoint.RemoteTrans;
	  nLocks: CARDINAL;
	BEGIN
	  EVAL ViewCommonHeader(buf^, commonHdr, 0);
	  bufIdx := BYTESIZE(commonHdr);
	  nLocks := VIEW(SUBARRAY(buf^, bufIdx, BYTESIZE(INTEGER)), INTEGER);
	  INC(bufIdx, BYTESIZE(INTEGER));
	  
	  (* In most cases, trans is already interned. But when
	     storage is never modified during the transaction, we might
	     have commit/1pcommit record without any preceding records
	     for that transaction. *)
	  rt := InternTrans(st, commonHdr.tid, commonHdr.lsn);
	  rt.state := CheckPoint.RState.Prepared;

	  (* "locks" becomes non-nil only when prepare after record is
	     analyzed. So "locks" should be nil at this point. *)
	  <*ASSERT rt.locks = NIL*> 
	  
	  (* Parse lock records *)
	  rt.locks := NEW(REF ARRAY OF PrepareLockEntry, nLocks);
	  VIEW(rt.locks^, ARRAY OF CHAR) :=
 	    SUBARRAY(buf^, bufIdx, nLocks*BYTESIZE(PrepareLockEntry));
	END;
      | Type.S1PCommit =>
	EVAL ViewCommonHeader(buf^, commonHdr, 0);
	(* The transaction may or may not be in the "st.trans" table.
	   It is not in the table when the transaction never modified
	   any page. XXX in such case, the storage man should not emit
	   S1PCommit. *)
	IF st.trans.get(commonHdr.tid, r) THEN
	  EVAL st.trans.delete(commonHdr.tid, r);
	END;
      | Type.SCommit =>	
	EVAL ViewCommonHeader(buf^, commonHdr, 0);
	<*ASSERT st.trans.get(commonHdr.tid, r)*>
	EVAL st.trans.delete(commonHdr.tid, r);
      | Type.SAbort =>
	EVAL ViewCommonHeader(buf^, commonHdr, 0);
	(* In a rare case, abort record may occur without preceding
	   log records for that transaction.

	   Let R be the remote client, and S be this host.
	   1. A READONLY (transaction info recorded on S)
	   2. A setrange (not flushed)
	   3. A crash
	   4. S detects the crash of A, and aborts S.
	*)
      
	IF st.trans.get(commonHdr.tid, r) THEN
	  EVAL st.trans.delete(commonHdr.tid, r);
	END;
      | Type.CheckPoint =>
	(* This should not happen since analysis always begin from
	   the last checkpoint(or the beginning of the log when no
	   checkpoint record exists. *)
	IO.Put("Hey, I'm not supposed to see checkpoint here.\n");
	<*ASSERT FALSE*>
      | Type.ShutDown =>
	(* Remove the storage from the activeTrans *)
	<*ASSERT info.storage.get(rawHdr.sid, r)*>
	EVAL info.storage.delete(rawHdr.sid, r);
      | Type.TPrepared, Type.TAbort, Type.TCommitted =>
	VAR tr: REF CheckPoint.LocalTrans;
	BEGIN
	  EVAL ViewCommonHeader(buf^, commonHdr, 0);
	  IF info.trans.get(commonHdr.tid, r) THEN
	    tr := NARROW(r, REF CheckPoint.LocalTrans);
	  ELSE
	    tr := NEW(REF CheckPoint.LocalTrans);
	    EVAL info.trans.put(commonHdr.tid, tr);
	  END;
	  CASE VAL(rawHdr.type, Type) OF
	  | Type.TPrepared =>
	    tr.state := CheckPoint.LState.Prepared;
	  | Type.TAbort, Type.TCommitted =>
	    EVAL info.trans.delete(commonHdr.tid, r);
	  ELSE
	  END;
	END;
      | Type.EOB =>
      END;
      lsn := GetNextLSN(rawHdr);
    END;
    buf := NIL;
  END Analyze;

BEGIN
END LogRecovery.

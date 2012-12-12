(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE CheckPoint;
IMPORT WAL;
IMPORT SIDRefTbl;
IMPORT IntRefTbl;
IMPORT TID, SID;

PROCEDURE ExpandArray (VAR log: REF ARRAY OF CHAR) =
  VAR old: REF ARRAY OF CHAR;
  BEGIN
    old := log;
    log := NEW(REF ARRAY OF CHAR, BYTESIZE(log^)*2);
    SUBARRAY(log^, 0, BYTESIZE(old^)) := old^;
  END ExpandArray;
  
PROCEDURE DynAppendWord64 (VAR log: REF ARRAY OF CHAR;
			   VAR idx: INTEGER;
			   val: INTEGER) =
  BEGIN
    IF idx + 8 > BYTESIZE(log^) THEN ExpandArray(log); END;
    WAL.AppendWord64(log^, idx, val);
  END DynAppendWord64;
  
PROCEDURE DynAppendWord32(VAR log: REF ARRAY OF CHAR;
			  VAR idx: INTEGER;
			  val: INTEGER) =
  BEGIN
    IF idx + 4 > BYTESIZE(log^) THEN ExpandArray(log); END;
    WAL.AppendWord32(log^, idx, val);
  END DynAppendWord32;
  
PROCEDURE Dump (log: WAL.T; READONLY info: T) =
  VAR
    buf := NEW(REF ARRAY OF CHAR, 8192);
    logIdx := 0;
    trItr := info.trans.iterate();
    stItr := info.storage.iterate();
    sid: SID.T;
    tid: TID.T;
    r: REFANY;
  BEGIN
    DynAppendWord64(buf, logIdx, info.trans.size());
    
    (* 1. Dump local transactions *)
    WHILE trItr.next(tid, r) DO
      VAR
	lt := NARROW(r, REF LocalTrans);
      BEGIN
	DynAppendWord64(buf, logIdx, tid);
	DynAppendWord64(buf, logIdx, ORD(lt.state));
      END;
    END;
    
    (* 2. Dump all the storages *)
    
    WHILE stItr.next(sid, r) DO
      VAR
	st := NARROW(r, Storage);
	rtItr := st.trans.iterate();
	rt: REF RemoteTrans;
	tid: TID.T;
	
	dpItr := st.dirtyPages.iterate();
	dp: REF DirtyPage;
	pos: INTEGER;
      BEGIN
	DynAppendWord32(buf, logIdx, sid.hid);
	DynAppendWord32(buf, logIdx, sid.lid);
	
	(* Dump the ongoing transactions on this storage *)
	DynAppendWord64(buf, logIdx, st.trans.size());
	WHILE rtItr.next(tid, r) DO
	  rt := NARROW(r, REF RemoteTrans);
	  DynAppendWord64(buf, logIdx, tid);
	  DynAppendWord64(buf, logIdx, rt.lastLSN);
	  DynAppendWord64(buf, logIdx, ORD(rt.state));
	END;

	(* Dump the dirty pages *)
	DynAppendWord64(buf, logIdx, st.dirtyPages.size());
	WHILE dpItr.next(pos, r) DO
	  dp := NARROW(r, REF DirtyPage);
	  DynAppendWord64(buf, logIdx, pos);
	  DynAppendWord64(buf, logIdx, dp.recoveryLSN);
	END;
      END;
    END;
    
    EVAL WAL.WriteOther(log, SID.Void, WAL.Type.CheckPoint,
			SUBARRAY(buf^, 0, logIdx)); 
  END Dump;

PROCEDURE Undump (log: WAL.T; lsn: WAL.LSN;
		  (*OUT*)VAR info: T) =
  VAR
    hdr: WAL.RawHeader;
    buf := NEW(REF ARRAY OF CHAR, WAL.BlockSize);
    logIdx: INTEGER := BYTESIZE(hdr);
    nTrans: INTEGER; (* # of local transactions *)
  BEGIN
    EVAL WAL.Read(log, lsn, buf);
    EVAL WAL.ViewRawHeader(buf^, hdr, 0);
    <*ASSERT hdr.lsn = lsn*>

    info.trans := NEW(IntRefTbl.Default).init();
    info.storage := NEW(SIDRefTbl.Default).init();
    info.firstAnalysisLSN := WAL.GetNextLSN(hdr);
    info.firstRedoLSN := info.firstAnalysisLSN;
  
    nTrans := WAL.ViewWord64(buf^, logIdx);
  
    FOR i := 1 TO nTrans DO
      VAR
	tid: TID.T;
	lt := NEW(REF LocalTrans);
      BEGIN
	tid := WAL.ViewWord64(buf^, logIdx);
	lt.state := VAL(WAL.ViewWord64(buf^, logIdx), LState);
	EVAL info.trans.put(tid, lt);
      END;
    END;
    
    WHILE logIdx < hdr.size DO
      VAR
	sid: SID.T;
	tid: TID.T;
	nTrans, nDirtyPages: INTEGER;
	pos: INTEGER;
	dp: REF DirtyPage;
	rt: REF RemoteTrans;
	st: Storage;
      BEGIN
	sid.hid := WAL.ViewWord32(buf^, logIdx);
	sid.lid := WAL.ViewWord32(buf^, logIdx);
	nTrans := WAL.ViewWord64(buf^, logIdx);
	
	st := NEW(Storage).init(sid);
	
	(* Undump the remote transactions *)
	FOR i := 1 TO nTrans DO
	  rt := NEW(REF RemoteTrans);
	  tid := WAL.ViewWord64(buf^, logIdx);
	  rt.lastLSN := WAL.ViewWord64(buf^, logIdx);
	  rt.state := VAL(WAL.ViewWord64(buf^, logIdx), RState);
	  EVAL st.trans.put(tid, rt);
	END;
      
	nDirtyPages := WAL.ViewWord64(buf^, logIdx);
	FOR i := 1 TO nDirtyPages DO
	  dp := NEW(REF DirtyPage);
	  pos := WAL.ViewWord64(buf^, logIdx);
	  dp.recoveryLSN := WAL.ViewWord64(buf^, logIdx);
	  info.firstRedoLSN := MIN(dp.recoveryLSN, info.firstRedoLSN);
	  EVAL st.dirtyPages.put(pos, dp);
	END;
	EVAL info.storage.put(sid, st);
      END;
    END;
  END Undump;


BEGIN
END CheckPoint.

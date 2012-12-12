(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made all procs private, and changed to do table jump.
 * 24-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE StorageRemote;
IMPORT TransError;
IMPORT TransPrivate;
IMPORT StorageLocal;
IMPORT Storage, StorageProtected, StorageRep;
IMPORT WAL;
IMPORT SID;
IMPORT TID;
IMPORT ActiveTrans, ActiveTransRep;
IMPORT TransRPC;
IMPORT Buffer, BufferRep;
IMPORT Error;
IMPORT LockMan, LockMode;
IMPORT CPU;
IMPORT HostID;
IMPORT IO, Fmt;
IMPORT PhysAddr;
IMPORT TransMode;
IMPORT TransServiceRep;
IMPORT TransGroup, TransGroupPrivate;

IMPORT TransT, TransRep;

<*NOWARN*>FROM TransUtils IMPORT Debug, DebugMsg, ValueIsNotPageAligned;

CONST PageSize = CPU.PAGESIZE;

PROCEDURE Hello (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
		 <*UNUSED*>group: TransGroup.T) =
  VAR msg := in.unpackText();
  BEGIN
    IO.Put("MSG: Hello : " &  msg);
    out.packInt(TransRPC.OK);
  END Hello;

PROCEDURE Open (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
		group: TransGroup.T) =
  VAR
    fileName := in.unpackText();
    st: StorageLocal.T;
  BEGIN
    IF DebugMsg THEN IO.Put("MSG:open " & fileName & ".\n"); END;
    TRY
      st := Storage.Open(fileName, group);
      IF st # NIL THEN
	out.packHeader3(TransRPC.OK, st.id, st.memObj.stat().virtualSize);
      ELSE
	out.packInt(9999); (* XXX *)
      END;
    EXCEPT
    | Error.E(e) =>
      IO.Put("open: " & e.message() & ".\n");
    END;
  END Open;

PROCEDURE InternTrans (group: TransGroup.T; tid: TID.T): TransT.T =
  VAR tr: TransT.T;
  BEGIN
    tr := TransGroupPrivate.InternTrans(group, tid);
    tr.mode := TransMode.Default;
    RETURN tr;
  END InternTrans;

PROCEDURE Msg (a: TEXT; in: TransRPC.RecvBuf;
	       t1: TEXT; v1: INTEGER;
	       t2: TEXT := NIL; v2: INTEGER := 0;
	       t3: TEXT := NIL; v3: INTEGER := 0;
	       t4: TEXT := NIL; v4: INTEGER := 0) =
  BEGIN
    IO.Put("MSG:"); IO.Put(a);
    IO.Put(" idx=" & Fmt.Int(in.currentIdx()));
    IO.Put(" " & t1 & "=" & Fmt.Int(v1));
    IF t2 # NIL THEN IO.Put(" " & t2 & "=" & Fmt.Int(v2)); END;
    IF t3 # NIL THEN IO.Put(" " & t3 & "=" & Fmt.Int(v3)); END;
    IF t4 # NIL THEN IO.Put(" " & t4 & "=" & Fmt.Int(v4)); END;
    IO.Put(".\n");
  END Msg;
  
PROCEDURE Close (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
		 group: TransGroup.T) =
  VAR
    sid: SID.T;
    st: Storage.T;
  BEGIN
    in.unpackHeader(sid);
    st := TransGroupPrivate.FindResource(group, sid);
    IF DebugMsg THEN Msg("close ", in, "sid", sid.lid); END;
    st.close(group);
    out.packInt(TransRPC.OK);
  END Close;

PROCEDURE LogUndo (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
		   group: TransGroup.T) =
  VAR
    sid: SID.T;
    tid: TID.T;
    from: INTEGER;
    oldImage: ARRAY [0 .. 8191] OF CHAR;
    oldImageSize: CARDINAL;

    tr: TransT.T;
    st: StorageLocal.T;
    notused: REFANY;
    at: ActiveTrans.T;

  BEGIN
    in.unpackHeader3(sid, tid, from);
    oldImageSize := in.unpackArray(oldImage);

    IF DebugMsg THEN 
      Msg("logundo", in, "sid", sid.lid, "from", from, "len", oldImageSize);
    END;

    tr := InternTrans(group, tid);
    st := TransGroupPrivate.FindResource(group, sid);
    
    <*ASSERT StorageLocal.openFiles.get(st.id, notused) AND notused = st*>
    st.lock();
    at := StorageProtected.InternTrans(st, tr);
    at.lastLSN := WAL.WriteRedo(st.log, st.id, tid, WAL.Type.Undo,
				at.lastLSN,
				from, SUBARRAY(oldImage, 0, oldImageSize));
    out.packInt(TransRPC.OK);
    st.unlock();
  END LogUndo;

PROCEDURE LogRedo (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
		   group: TransGroup.T) =
  VAR
    sid: SID.T;
    tid: TID.T;
    from: INTEGER;
    newImage: ARRAY [0 .. 8191] OF CHAR;
    newImageSize: CARDINAL;
    tr: TransT.T;
    st: StorageLocal.T;
    notused: REFANY;
    at: ActiveTrans.T;

  BEGIN
    in.unpackHeader3(sid, tid, from);
    newImageSize := in.unpackArray(newImage);
    IF DebugMsg THEN 
      Msg("logredo", in, "sid", sid.lid, "from", from, "len", newImageSize);
    END;

    tr := InternTrans(group, tid);
    st := TransGroupPrivate.FindResource(group, sid);
    
    <*ASSERT StorageLocal.openFiles.get(st.id, notused) AND notused = st*>
    LOCK st.mu DO 
      at := StorageProtected.InternTrans(st, tr);
      at.lastLSN := WAL.WriteRedo(st.log, st.id, tid, WAL.Type.Redo,
				  at.lastLSN,
				  from, SUBARRAY(newImage, 0, newImageSize));
      StorageProtected.ModifyPage(st, from,
				  SUBARRAY(newImage, 0, newImageSize), at.lastLSN);
      out.packInt(TransRPC.OK);
    END;
  END LogRedo;

(* Lock the pages "from" to "from*NUMBER(op)*pagesize" either
   exclusively or unexclusively depending on "exclusive", then
   read in the pages for those marked "LockAndRead". This procedure
   directly writes into the buffer for performance reasons. *)
PROCEDURE Read (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
		group: TransGroup.T) =
  VAR
    hid: HostID.T;
    sid: SID.T;
    tid: TID.T;
    from, modeVal: INTEGER;
    nOp: CARDINAL;
    op: ARRAY [0 .. 255] OF CHAR;
    mode: LockMode.T;
    status: INTEGER;
    len: INTEGER;
    buf: Buffer.T;
    notused: REFANY;
    st: StorageLocal.T;
    tr: TransT.T;
    at: ActiveTrans.T;
    allocated := TRUE;
  PROCEDURE Callback(VAR page: PhysAddr.Content) =
    BEGIN
      out.packArray(page);
    END Callback;
    
  BEGIN
    in.unpackHeader4(sid, tid, from, modeVal);
    st := TransGroupPrivate.FindResource(group, sid);
    <*ASSERT ISTYPE(st, StorageLocal.T)*>
    tr := InternTrans(group, tid);
    mode := VAL(modeVal, LockMode.T);

    hid := TID.GetHostID(tr.tid);
    nOp := in.unpackArray(op);
    IF DebugMsg THEN
      Msg("read", in, "sid", sid.lid, "tid", tid,
	  "from", from, "mode", modeVal);
      FOR i := 0 TO nOp-1 DO
	IF op[i] = LockAndRead THEN IO.Put("r");
	ELSE IO.Put("-");
	END;
      END;
      IO.Put("\n");
    END;
    len := nOp * PageSize;
    
    <*ASSERT StorageLocal.openFiles.get(st.id, notused) AND notused = st*>
    
    st.lock();
    at := StorageProtected.InternTrans(st, tr);
    TRY
      <*ASSERT nOp=1*>
      status := LockMan.Lock(at, st, mode, from, from+nOp);
      IF status # 0 THEN
	TransError.Raise(status);
      END;
      out.packInt(TransRPC.OK);
      
      FOR i := 0 TO nOp-1 DO 
	IF op[i] = LockAndRead THEN
	  buf := st.pinFrame(NIL, LockMode.T.None, from, allocated);
	  IF buf = NIL THEN
	    IO.Put("storageremote: buf=nil");
	    EXIT;
	  END;
	  PhysAddr.Access(buf.frame, Callback);
	  Buffer.Unlock(buf);
	END;
	INC(from);
      END;

    EXCEPT
    | Error.E(e) =>
      (* XXX need to reset "out". *)
      out.packInt(e.resultCode());
      st.unlock();
      RETURN;
    END;

    st.unlock();
  END Read;

PROCEDURE Prepare (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
		   group: TransGroup.T) =
  VAR
    sid: SID.T;
    tid: TID.T;
    st: Storage.T;
    tr: TransT.T;
    ok: BOOLEAN;
    lsn: WAL.LSN;
  BEGIN
    in.unpackHeader2(sid, tid);
    st := TransGroupPrivate.FindResource(group, sid);
    tr := InternTrans(group, tid);
    
    IF DebugMsg THEN
      Msg("prepare ", in, "tid", tr.tid, "st", st.id.lid);
    END;
    ok := st.prepare(tr, lsn);
    TransPrivate.FlushDefaultLog(lsn);
    out.packInt(TransRPC.OK);
    out.packBool(ok);
  END Prepare;

PROCEDURE OnePhaseCommit (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
			  group: TransGroup.T) =
  VAR
    sid: SID.T;
    tid: TID.T;
    st: Storage.T;
    tr: TransT.T;
    ok: BOOLEAN;
    lsn: WAL.LSN;
  BEGIN
    in.unpackHeader2(sid, tid);
    st := TransGroupPrivate.FindResource(group, sid);
    tr := InternTrans(group, tid);
					 
    IF DebugMsg THEN
      Msg("1pcommit", in, "tid", tr.tid, "sid", st.id.lid);
    END;
    ok := st.onePhaseCommit(tr, lsn);
    TransPrivate.FlushDefaultLog(lsn);
    TransGroupPrivate.DeleteTrans(tr);
    out.packInt(TransRPC.OK);
    out.packBool(ok);
  END OnePhaseCommit;

PROCEDURE Commit (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
		  group: TransGroup.T) =
  VAR
    sid: SID.T;
    tid: TID.T;
    st: Storage.T;
    tr: TransT.T;
    lsn: WAL.LSN;
  BEGIN
    in.unpackHeader2(sid, tid);
    st := TransGroupPrivate.FindResource(group, sid);
    tr := InternTrans(group, tid);
    IF DebugMsg THEN
      Msg("commit", in, "tid", tr.tid, "sid", st.id.lid);
    END;
    st.commit(tr, lsn);
    TransPrivate.FlushDefaultLog(lsn);
    TransGroupPrivate.DeleteTrans(tr);
    out.packInt(TransRPC.OK);
    <*ASSERT tr.nService = 0*>
  END Commit;

PROCEDURE Abort (in: TransRPC.RecvBuf; out: TransRPC.SendBuf;
		 group: TransGroup.T) =
  VAR
    sid: SID.T;
    tid: TID.T;
    st: Storage.T;
    tr: TransT.T;
  BEGIN
    in.unpackHeader2(sid, tid);
    st := TransGroupPrivate.FindResource(group, sid);
    tr := InternTrans(group, tid);
    IF DebugMsg THEN
      Msg("abort", in, "tid", tr.tid, "sid", st.id.lid);
    END;
    st.abort(tr);
    TransGroupPrivate.DeleteTrans(tr);
    out.packInt(TransRPC.OK);
  END Abort;

BEGIN
  Callbacks := ARRAY OF CallbackProc
  {
   Hello,
   Open,
   Read,
   Close,
   Prepare,
   Commit,
   Abort,
   OnePhaseCommit,
   LogUndo,
   LogRedo,
   TransPrivate.ResolveTransInDoubt
   };
END StorageRemote.



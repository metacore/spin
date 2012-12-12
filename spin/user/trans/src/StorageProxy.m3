(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to use batch rpc.
 * 31-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	whisted.
 *	
 *)

UNSAFE MODULE StorageProxy;
IMPORT HostID;
IMPORT TransServiceRep;
IMPORT Storage, StorageProtected, StorageRep;
IMPORT TransRPC;
IMPORT IO;
IMPORT Debugger;
IMPORT StorageRemote;
IMPORT TransT, TransPrivate, TransRep;
IMPORT TransGroup, TransGroupPrivate;

IMPORT Buffer, BufferRep;

IMPORT MemoryObject;
IMPORT TransPager;
IMPORT TransCache;

IMPORT PhysAddr;
IMPORT LockMan, LockMode;
IMPORT ActiveTrans, ActiveTransRep;
IMPORT SID;
IMPORT TID;
IMPORT WAL;
IMPORT Error;
FROM TransUtils IMPORT Msg;

TYPE T = Storage.T BRANDED OBJECT
  in: TransRPC.RecvBuf;
  out: TransRPC.SendBuf;
  server: TransRPC.Server;
OVERRIDES
  pinFrame := PinFrame;
  close := Close;
  abort := Abort;
  prepare := Prepare;
  commit := Commit;
  onePhaseCommit := OnePhaseCommit;
  writeRedo := WriteRedo;
  flushLog := FlushLog;
END;

  
PROCEDURE Open (host: TEXT; port: CARDINAL; fileName: TEXT): Storage.T
  RAISES {Error.E} =
  VAR 
    server: TransRPC.Server;
    st: T;
    size: INTEGER;
    in := TransRPC.CreateRecvBuf();
    out := TransRPC.CreateSendBuf();
    hid, lid: INTEGER;
  BEGIN
    server := TransRPC.OpenServer(HostID.HostNameToT(host), port);
    
    out.startNewCommand();
    out.packInt(TransRPC.Open);
    out.packText(fileName);
    
    EVAL TransRPC.DoRPC(server, out, in);
    in.unpackInt3(hid, lid, size);
    in.endUnpack();
    
    st := NEW(T,
	      server := server,
	      in := in, out := out);
    EVAL st.init(SID.T{hid := hid, lid := lid}, fileName);
    st.pager := TransPager.Create(st);
    st.memObj := NEW(MemoryObject.T).init(size,
					  st.pager,
					  TransCache.Create(st));
    RETURN st;
  END Open;

PROCEDURE PinFrame (st: T; at: ActiveTrans.T; mode: LockMode.T;
		    pos: Buffer.Offset; VAR allocate: BOOLEAN;
		    frame: PhysAddr.T): Buffer.T =
  VAR
    op: ARRAY [0 .. 0] OF CHAR;
    buf: Buffer.T;
  PROCEDURE Callback(VAR page: PhysAddr.Content) =
    BEGIN
      EVAL st.in.unpackArray(page);
    END Callback;
  BEGIN
    <*ASSERT st.isLocked()*>
    
    buf := Buffer.Lock(st, pos);
    
    IF buf # NIL THEN
      allocate := FALSE;
      IF mode = LockMode.T.None THEN
	RETURN buf;
      END;
      op[0] := StorageRemote.Lock;
    ELSE
      IF NOT allocate THEN RETURN NIL; END;
      allocate := TRUE;
      op[0] := StorageRemote.LockAndRead;
    END;
    
    (* If either buffer is not found, or lock is needed, we have to
       do RPC. *)
    <*ASSERT mode # LockMode.T.None*>
    st.out.startNewCommand();
    st.out.packHeader5(TransRPC.Read, st.id, at.tr.tid, pos, ORD(mode));
    st.out.packArray(op);
    
    IF TransRPC.DoRPC(st.server, st.out, st.in) THEN
      IF op[0] = StorageRemote.LockAndRead THEN
	<*ASSERT buf = NIL*>
	buf := Buffer.Allocate(st, pos, frame);
	PhysAddr.Access(buf.frame, Callback);
	IF Buffer.Intern(buf) THEN
	  Debugger.Enter();
	END;
      END;
      st.in.endUnpack();
      IF mode = LockMode.T.Write THEN
	Buffer.Shadow(buf, at);
      END;
      RETURN buf;
    ELSE
      IF buf # NIL THEN Buffer.Unlock(buf); END;
      RETURN NIL;
    END;
  END PinFrame;
    
PROCEDURE Abort (st: T; tr: TransT.T) =
  VAR
    at: ActiveTrans.T;
  BEGIN
    st.lock();
    at := StorageProtected.FindTrans(st, tr);
    st.out.startNewCommand();
    st.out.packHeader3(TransRPC.Abort, st.id, tr.tid);
    EVAL TransRPC.DoRPC(st.server, st.out, st.in); (* doesn't matter whether
						      it succeeded or not. *)
    st.in.endUnpack();
    (* XXX *)
    
    LockMan.UnlockAll(at, st);
    ActiveTrans.Destroy(at);
    TransPager.InvalidateMappingsForSpace(st.pager, tr.group);
    TransPrivate.Unjoin(tr, st);    
    st.unlock();
  END Abort;
  
PROCEDURE Prepare (st: T; tr: TransT.T; VAR lsn: WAL.LSN): BOOLEAN =
  VAR ok: BOOLEAN;
  BEGIN
    lsn := 0;
    st.lock();
    
    st.out.startNewCommand();
    st.out.packHeader3(TransRPC.Prepare, st.id, tr.tid);
      
    IF TransRPC.DoRPC(st.server, st.out, st.in) THEN
      ok := st.in.unpackBool();
      st.in.endUnpack();
      st.unlock();
      IF ok  THEN
	RETURN TRUE;
      ELSE
	Msg("proxy:remote prepare failed\n");
	RETURN FALSE;
      END;
    ELSE
      Msg("proxy:comm failure.\n");
      RETURN FALSE;
    END;
  END Prepare;
  
PROCEDURE Commit (st: T; tr: TransT.T; VAR lsn: WAL.LSN) =
  VAR at: ActiveTrans.T;
  BEGIN
    lsn := 0;
    st.lock();
    at := StorageProtected.FindTrans(st, tr);
    
    EVAL StorageProtected.FlushModificationsForTrans(st, at);

    st.out.startNewCommand();
    st.out.packHeader3(TransRPC.Commit, st.id, tr.tid);

    EVAL TransRPC.DoRPC(st.server, st.out, st.in);
    st.in.endUnpack();
    
    LockMan.UnlockAll(at, st);
    ActiveTrans.Destroy(at);
    TransPager.InvalidateMappingsForSpace(st.pager, tr.group);
    TransPrivate.Unjoin(tr, st);
    st.unlock();
  END Commit;
  
PROCEDURE OnePhaseCommit (st: T; tr: TransT.T; VAR lsn: WAL.LSN): BOOLEAN =
  VAR ok := FALSE;
    at: ActiveTrans.T;
  BEGIN
    lsn := 0; (* log is stored on the remote machine. *)
    st.lock();
    at := StorageProtected.FindTrans(st, tr);
	
    EVAL StorageProtected.FlushModificationsForTrans(st, at);

    st.out.startNewCommand();
    st.out.packHeader3(TransRPC.OnePhaseCommit, st.id, tr.tid);

    IF TransRPC.DoRPC(st.server, st.out, st.in) THEN 
      ok := st.in.unpackBool();
      st.in.endUnpack();
    END;
    
    LockMan.UnlockAll(at, st);
    ActiveTrans.Destroy(at);
    TransPager.InvalidateMappingsForSpace(st.pager, tr.group);
    TransPrivate.Unjoin(tr, st);    
    st.unlock();
    
    IF ok THEN
      RETURN TRUE;
    ELSE
      IO.Put("proxy:1pcommit failed\n");
      RETURN FALSE;
    END;
  END OnePhaseCommit;

PROCEDURE Close (st: T; group: TransGroup.T) =
  BEGIN
    st.lock();
    st.out.startNewCommand();
    st.out.packHeader2(TransRPC.Close, st.id);
    EVAL TransRPC.DoRPC(st.server, st.out, st.in);
    st.in.endUnpack();
    TransGroupPrivate.DeleteResource(group, st);
    st.unlock();
  END Close;

PROCEDURE WriteRedo (st: T; tid: TID.T; type: WAL.Type;
		     <*UNUSED*>lsn: WAL.LSN; pos: CARDINAL;
		     READONLY image: ARRAY OF CHAR): WAL.LSN =
  BEGIN
    st.out.startNewCommand();
    (* XXX *)
    st.out.packHeader4(TransRPC.LogRedo, st.id, tid, pos);
    st.out.packArray(image);
    TransRPC.QueueRPC(st.server, st.out);
    RETURN LAST(WAL.LSN);
  END WriteRedo;
  
PROCEDURE FlushLog (st: T; <*UNUSED*>lsn: INTEGER) =
  BEGIN
    IF st.out.idx > BYTESIZE(INTEGER) * 2 THEN
      (* We have things to send. *)
      EVAL TransRPC.DoRPC(st.server, st.out, st.in);
    END;
  END FlushLog;
  
BEGIN
END StorageProxy.


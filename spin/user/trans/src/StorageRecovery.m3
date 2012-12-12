(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE StorageRecovery EXPORTS StorageLocal;
IMPORT ActiveTrans, ActiveTransRep, ActiveTransQ;
IMPORT TransT, TransRep;
IMPORT Error;
IMPORT TransError;
IMPORT TransServiceRep;
IMPORT StorageProtected, StorageRep;
IMPORT TransPrivate;
IMPORT SIDRefTbl;
IMPORT IntRefTbl;
IMPORT CheckPoint;
IMPORT Thread;
IMPORT TID;
IMPORT IO, Fmt;
IMPORT WAL;
IMPORT SID;
IMPORT Buffer, BufferRep;
IMPORT TransOS;
IMPORT LockMan;
IMPORT LockMode;

IMPORT TransGroup, TransGroupPrivate;
FROM TransUtils IMPORT Msg;

REVEAL CheckPoint.Storage = CheckPoint.StoragePublic BRANDED OBJECT
  nOpened: INTEGER;
OVERRIDES
  init := StorageInit;
END;

PROCEDURE Dump (<*UNUSED*>log: WAL.T): SIDRefTbl.T =
  VAR
    t := NEW(SIDRefTbl.Default).init();
    stItr := openFiles.iterate();
    sid: SID.T;
    r: REFANY;
    st: T;
  BEGIN
    WHILE stItr.next(sid, r) DO
      st := NARROW(r, T);
      
      (* XXX: we have to do do trylock on storage *)
      VAR 
	stRecord: CheckPoint.Storage;
	trItr := ActiveTransQ.Iterate(st.activeTrans);
	at: ActiveTrans.T;
	trRecord: REF CheckPoint.RemoteTrans;
	dpItr := Buffer.Iterate(st);
	buf: Buffer.T;
	dpRecord: REF CheckPoint.DirtyPage;
      BEGIN
	stRecord := NEW(CheckPoint.Storage).init(st.id);
	EVAL t.put(st.id, stRecord);
	
	WHILE ActiveTransQ.NextItr(trItr, at) DO
	  trRecord := NEW(REF CheckPoint.RemoteTrans,
			  state := at.state,
			  lastLSN := at.lastLSN);
	  EVAL stRecord.trans.put(at.tr.tid, trRecord);
	END;


	(* Then, dump the dirty pages *)
	WHILE dpItr.next(buf) DO
	  IF buf.dirty THEN
	    dpRecord := NEW(REF CheckPoint.DirtyPage,
			    recoveryLSN := buf.recoveryLSN);
	    EVAL stRecord.dirtyPages.put(buf.pos, dpRecord);
	  END;
	END;
	Buffer.EndIterate();
      END;
    END;
    RETURN t;
  END Dump;

PROCEDURE Redo (log: WAL.T;
		cs: CheckPoint.Storage;
		READONLY content: ARRAY OF CHAR;
		pos: INTEGER): WAL.RecoveryResult =
  VAR
    st: T;
  BEGIN
    TRY
      st := OpenFromID(cs.sid, log);
    EXCEPT
    | Error.E(e) =>
      IF e.resultCode() = TransError.NO_SUCH_STORAGE THEN
	Msg("storage redo: the file ", Fmt.Int(cs.sid.lid)," is removed.");
	RETURN WAL.RecoveryResult.NoSuchStorage;
      ELSE
	Msg("storage redo: error ", e.message());
	RETURN WAL.RecoveryResult.OtherError;
      END;
    END;
    INC(cs.nOpened);
    LOCK st.mu DO
      StorageProtected.ModifyPage(st, pos, content);
    END;
    RETURN WAL.RecoveryResult.OK;
  END Redo;

PROCEDURE Undo (log: WAL.T;
		cs: CheckPoint.Storage;
		READONLY content: ARRAY OF CHAR;
		pos: INTEGER): WAL.RecoveryResult =
  BEGIN
    (* Undo and redo are actually same process. Only the
     data is different *)
    RETURN Redo(log, cs, content, pos);
  END Undo;

PROCEDURE RecoveryStart (<*UNUSED*>log: WAL.T;
			 <*UNUSED*>READONLY info: CheckPoint.T) =
  BEGIN
  END RecoveryStart;

PROCEDURE RecoveryFinished (<*UNUSED*>log: WAL.T; READONLY info: CheckPoint.T) =
  VAR itr := info.storage.iterate();
    r: REFANY;
    cs: CheckPoint.Storage;
    st: T;
    sid: SID.T;
  BEGIN
    WHILE itr.next(sid, r) DO
      cs := NARROW(r, CheckPoint.Storage);
      WHILE cs.nOpened > 0 DO
	LOCK openFilesMu DO
	  EVAL openFiles.get(sid, r);
	  st := NARROW(r, T);
	END;
	st.close(TransGroup.default);
	DEC(cs.nOpened);
      END;
    END;
  END RecoveryFinished;

TYPE
  Resolver = Thread.Closure OBJECT
    log: WAL.T;
    st: T;
    at: ActiveTrans.T;
    tid: TID.T;
  OVERRIDES
    apply := ResolveInternal;
  END;
  
PROCEDURE ResolveInternal (c: Resolver): REFANY =
  BEGIN
    IF TransPrivate.Resolve(c.tid) THEN
      (* DO COMMIT *)
      (* c.st.commit(c.tid);*)
    ELSE
      (* DO ABORT. *)
      (* here, we have to back out manually, since log manager recovery is
       already finished at this point. *)

      IO.Put("abortabort");
    END;
    LockMan.UnlockAll(c.at, c.st);
    ActiveTrans.Destroy(c.at);
    c.st.close(TransGroup.default);
    RETURN NIL;
  END ResolveInternal;

(* This stuff is called after recovery redo is finished, but before
 undo starts. This means if we are to commit "tid", nothing is needed
 other writing the commit record, but if we are to abort "tid", we
 have to back out the changes made by the transaction manually. *)
 
PROCEDURE ResolveUncommitedTrans (log: WAL.T; sid: SID.T; tid: TID.T;
				  rt: REF CheckPoint.RemoteTrans) =
  VAR
    status: INTEGER;
    st: T;
    tr: TransT.T;
    at: ActiveTrans.T;
  BEGIN
    (* Fake open to ensure the storage is not closed while
     this proc is executing. *)
    TRY
      st := OpenFromID(sid, log);
      st.lock();
      tr := TransGroupPrivate.InternTrans(TransGroup.default, tid);
      at := StorageProtected.InternTrans(st, tr);
      FOR i := 0 TO LAST(rt.locks^) DO
	status := LockMan.Lock(at, st, VAL(rt.locks[i].mode, LockMode.T),
			       rt.locks[i].from, rt.locks[i].end);
	<*ASSERT status = 0*>
      END;

      st.unlock();
      EVAL Thread.Fork(NEW(Resolver,
			   st := st,
			   at := at,
			   tid := tid,
			   log := log));
    EXCEPT
    | Error.E(e) =>
      Msg("resolveuncommitedtrans: ", e.message());
    END;
  END ResolveUncommitedTrans;
  
PROCEDURE OpenFromID (sid: SID.T; log: WAL.T): T RAISES {Error.E} =
  VAR
    fileName := TransOS.SIDToFileName(sid);
    st: T;
  BEGIN
    IF fileName = NIL THEN
      RAISE Error.E(NEW(TransError.T).init(TransError.NO_SUCH_STORAGE));
    END;
    st := Open(fileName, log);
    TransGroupPrivate.RegisterResource(TransGroup.default, st);
    RETURN st;
  END OpenFromID;

  
PROCEDURE StorageInit (s: CheckPoint.Storage; sid: SID.T): CheckPoint.Storage=
  BEGIN
    s.nOpened := 0;
    s.sid := sid;
    s.trans := NEW(IntRefTbl.Default).init();
    s.dirtyPages := NEW(IntRefTbl.Default).init();
    RETURN s;
  END StorageInit;

BEGIN
END StorageRecovery.

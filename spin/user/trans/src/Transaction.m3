(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 * 
 *
 * HISTORY
 * 15-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added group commit.
 * 17-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *	
 *)
MODULE Transaction EXPORTS Transaction, TransPrivate;
IMPORT TID;
IMPORT WAL;
IMPORT HostID;
IMPORT TransOS;
IMPORT IO;
IMPORT TransMode;
IMPORT TransRep;
IMPORT TransGroup, TransGroupPrivate;
IMPORT TransService, TransServiceRep;
IMPORT Thread;
IMPORT RefRefTbl;

VAR
  tidCounter: TID.T;
  mu := NEW(MUTEX); (* guards tidCounter *)


(* Open the log device *)
PROCEDURE Init () =
  BEGIN
    IF log = NIL THEN 
      log := WAL.Open(TransOS.GetDefaultLogFileName());
    END;
  END Init;

PROCEDURE Begin (group: TransGroup.T; mode: TransMode.Set): T =
  VAR
    tr: T;
    tid: TID.T;
  BEGIN
    Init();
    LOCK mu DO
      tid := TID.CreateT(HostID.myID, tidCounter);
      INC(tidCounter);
      IF TransMode.T.NoLogging IN mode THEN
	INC(nNonAtomicTrans);
      END;
    END;
    tr := TransGroupPrivate.InternTrans(group, tid);    
    tr.mode := mode;
    RETURN tr;
  END Begin;

PROCEDURE GetTID (t: T): TID.T =
  BEGIN
    IF t.aborted THEN RETURN TID.Void; END;
    RETURN t.tid;
  END GetTID;

(*

 Two phase commit stuff

 *)

PROCEDURE TwoPhaseCommit (t: T): BOOLEAN =
  VAR
    nService := t.nService;
    lsn: WAL.LSN;
    maxLSN: WAL.LSN := FIRST(WAL.LSN);
  BEGIN
    (* Poll each storage participating in the transaction "t" if
     it is ready for the commit. Returns TRUE if all the service are
     ready.  If at least one of the service aren't ready, the whole
     transaction fails.
     *)
    FOR i := 0 TO nService-1 DO 
      IF NOT t.service[i].prepare(t, lsn) THEN
	FOR i := 0 TO nService-1 DO 
	  t.service[i].abort(t);
	END;
	RETURN FALSE;
      END;
      maxLSN := MAX(lsn, maxLSN);
    END;
    FlushDefaultLog(maxLSN);
    
    (* Ok, everyone is prepared. *)
    (* XXX log commit here *)
    (* Let everyone commit *)

    maxLSN := FIRST(WAL.LSN);
    FOR i := 0 TO nService-1 DO 
      t.service[i].commit(t, maxLSN);
      maxLSN := MAX(lsn, maxLSN);
    END;
    FlushDefaultLog(maxLSN);
    
    IF TransMode.T.NoLogging IN t.mode THEN
      LOCK mu DO 
	DEC(nNonAtomicTrans);
      END;
    END;

    (* XXX Then, emit completion record *)

    RETURN TRUE;
  END TwoPhaseCommit;

PROCEDURE Commit (t: T): BOOLEAN =
  VAR
    status: BOOLEAN;
    lsn: WAL.LSN;
  BEGIN
    IF t.aborted THEN RETURN FALSE; END;

    IF t.nService = 1 THEN
      status := t.service[0].onePhaseCommit(t, lsn);
      FlushDefaultLog(lsn);
    ELSIF t.nService > 1 THEN
      status := TwoPhaseCommit(t);
    ELSE
      (* The transaction never accessed the storage. *)
      status := TRUE;
    END;
    IF TransMode.T.NoLogging IN t.mode THEN
      LOCK mu DO
	DEC(nNonAtomicTrans);
      END;
    END;

    TransGroupPrivate.DeleteTrans(t);
    RETURN status;
  END Commit;

PROCEDURE Abort (t: T) =
  VAR nService := t.nService;
  BEGIN
    FOR i := 0 TO nService-1 DO 
      t.service[i].abort(t);
    END;
    IF TransMode.T.NoLogging IN t.mode THEN
      LOCK mu DO
	DEC(nNonAtomicTrans);
      END;
    END;
    TransGroupPrivate.DeleteTrans(t);
  END Abort;


  
PROCEDURE CheckPoint() =
  BEGIN
    IO.Put("checkpoint not supported\n");
  END CheckPoint;

PROCEDURE GetDefaultLogDevice (): WAL.T =
BEGIN
  Init();
  RETURN log;
END GetDefaultLogDevice;

PROCEDURE FlushLogs () =
  BEGIN
    WAL.FlushLog();
  END FlushLogs;

PROCEDURE FreeupLogSpace (): WAL.LSN =
  BEGIN
    (* XXX *)
    RETURN LAST(WAL.LSN);
  END FreeupLogSpace;

PROCEDURE Join (tr: T; st: TransService.T) =
  BEGIN
    FOR i := FIRST(tr.service) TO LAST(tr.service) DO
      IF tr.service[i] = st THEN RETURN; END;
      IF tr.service[i] = NIL THEN
	(* Storage is never deleted from the table until the transaction
	 commits or aborts, so the access to the table is two-phased;
	 first phase interns all the service, and next phase removes all the
	 service. Therefore, we don't have to worry about a hole in the
	 table that causes already interned storage to be interned again. *)
	INC(tr.nService);
	tr.service[i] := st;
	RETURN;
      END;
    END;
  END Join;
  
PROCEDURE Unjoin (tr: T; st: TransService.T) =
  BEGIN
    FOR i := FIRST(tr.service) TO LAST(tr.service) DO
      IF tr.service[i] = st THEN
	tr.service[i] := NIL;
	DEC(tr.nService);
	RETURN;
      END;
    END;
    <*ASSERT FALSE*>
  END Unjoin;

VAR
  threadTransTbl: RefRefTbl.T;
  
PROCEDURE AttachTransactionToCurrentThread(t: T) =
  BEGIN
    EVAL threadTransTbl.put(Thread.Self(), t);
  END AttachTransactionToCurrentThread;

PROCEDURE GetCurrent(): T =
  VAR r: REFANY;
  BEGIN
    IF threadTransTbl.get(Thread.Self(), r) THEN
      RETURN r;
    ELSE
      RETURN NIL;
    END;
  END GetCurrent;
  
BEGIN
  threadTransTbl := NEW(RefRefTbl.Default).init();
END Transaction.

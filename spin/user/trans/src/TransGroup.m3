(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransGroup EXPORTS TransGroup, TransGroupPrivate, TransPrivate;
IMPORT TransGroupRep;
IMPORT FastIntRefTbl;
IMPORT Storage, StorageRep;
IMPORT TransServiceRep;
IMPORT HostSet;
IMPORT HostID;
IMPORT IO, Fmt;
IMPORT TID;
IMPORT SID;
IMPORT TransQ;
IMPORT TransT, TransRep;
IMPORT Thread;
IMPORT Clock;
IMPORT WAL;
IMPORT LightMutex;
IMPORT Strand;

FROM TransUtils IMPORT SpyTime;

VAR 
  idTable: FastIntRefTbl.T;
  map: HostSet.T;
  
  nextID: ID;
  mu: MUTEX;
  nOutstandingTrans: CARDINAL;
  walMu: MUTEX;

REVEAL T = TransGroupRep.TPublic BRANDED OBJECT
OVERRIDES
  init := Init;
  shutDown := ShutDown;
END;
  
PROCEDURE Init (group: T): T =
  VAR
    found := FALSE;
    id: ID;
  BEGIN
    LOCK mu DO
      (* look for a free local id *)
      FOR i := nextID TO LAST(ID) DO
	IF map[i] = FALSE THEN
	  found := TRUE;
	  id := i;
	  EXIT;
	END;
      END;

      IF NOT found AND nextID > 0 THEN
	(* Wrap round and find the first free ID *)
	FOR i := 0 TO nextID-1 DO
	  IF map[i] = FALSE THEN
	    found := TRUE;
	    id := i;
	    EXIT;
	  END;
	END;
      END;

      IF NOT found THEN
	IO.Put("Too many trans groups.\n");
	<*ASSERT FALSE*>
	RETURN NIL;
      END;
    
      group.mu := NEW(MUTEX);
      group.trans := TransQ.NewHeader();
      group.gid := id;
      
      map[id] := TRUE;
      IF idTable.put(id, group) THEN
	IO.Put("registering same group twice2!!");
      END;
    END;
    (* XXX do weakref *)
    RETURN group;
  END Init;

PROCEDURE FindFromGid (id: ID): T =
  VAR r: REFANY;
  BEGIN
    LOCK mu DO 
      IF NOT idTable.get(id, r) THEN
	RETURN NIL;
      END;
    END;
    RETURN r;
  END FindFromGid;
  
PROCEDURE InternTrans (group: T; tid: TID.T) : TransT.T =
  VAR tr := group.trans.next;
  BEGIN
    LOCK group.mu DO
      WHILE tr # group.trans DO 
	IF tr.tid = tid THEN RETURN tr; END;
	tr := tr.next;
      END;
      tr := NEW(TransT.T,
		group := group,
		aborted := FALSE,
		tid := tid,
		mu := LightMutex.Create());
      LOCK mu DO
	INC(nOutstandingTrans);
	TransQ.InsertTail(group.trans, tr);
      END;
    END;
    RETURN tr;
  END InternTrans;

PROCEDURE DeleteTrans (tr: TransT.T) =
  BEGIN
    <*ASSERT tr.nService = 0*>
    LOCK mu DO 
      DEC(nOutstandingTrans);
      TransQ.Remove(tr);
    END;
  END DeleteTrans;

PROCEDURE RegisterResource (t: T; st: Storage.T) =
  BEGIN
    (* If the storage is already registered, just increase the
       reference count. *)
    FOR i := FIRST(t.service) TO  LAST(t.service) DO
      IF t.service[i] = st THEN
	INC(t.refCount[i]);
	RETURN;
      END;
    END;

    (* Otherwise, add it to the table. *)
    FOR i := FIRST(t.service) TO  LAST(t.service) DO
      IF t.service[i] = NIL THEN
	t.service[i] := st;
	t.refCount[i] := 1;
	RETURN;
      END;
    END;
  END RegisterResource;

PROCEDURE FindResource (t: T; id: SID.T): Storage.T =
  BEGIN
    FOR i := FIRST(t.service) TO  LAST(t.service) DO
      IF t.service[i] # NIL AND t.service[i].id = id THEN
	RETURN t.service[i];
      END;
    END;
    RETURN NIL;
  END FindResource;
  
PROCEDURE DeleteResource (t: T; re: Storage.T) =
  BEGIN
    FOR i := FIRST(t.service) TO  LAST(t.service) DO
      IF t.service[i] = re THEN
	DEC(t.refCount[i]);
	IF t.refCount[i] = 0 THEN
	  t.service[i] := NIL;
	END;
	RETURN;
      END;
    END;
    <*ASSERT FALSE*>
  END DeleteResource;
  
PROCEDURE ShutDown (group: T) =
  VAR
    tr: TransT.T;
    status: BOOLEAN;
    r: REFANY;
  BEGIN
    (* XXX clear the buffer *)
    
    IO.Put("shutting down a group\n");

    LOOP
      LOCK group.mu DO
	IF TransQ.Empty(group.trans) THEN
	  EXIT;
	END;
	(* For each transaction originated at the host HID *)
	tr := group.trans.next;
      END;
      
      IO.Put("aborting " & Fmt.Int(tr.tid));
      IF tr.nService > 0 THEN
	FOR i := 0 TO tr.nService-1 DO 
	  tr.service[i].abort(tr);
	END;
      ELSE
	IO.Put(" : null transaction ");
      END;
      DeleteTrans(tr);
      <*ASSERT tr.nService = 0*>
      <*ASSERT tr.next = NIL*>
    END;

    (* XXX group isn't locked. Is this ok? *)
    FOR i := FIRST(group.service) TO LAST(group.service) DO
      IF group.service[i] # NIL THEN
	VAR
	  st := NARROW(group.service[i], Storage.T);
	  refCount := group.refCount[i];
	BEGIN
	  <*ASSERT refCount >= 1*>
	  IO.Put("closing the storage \"" & st.name & "\".\n");
	  FOR j := 1 TO refCount DO
	    st.close(group);
	  END;
	  <*ASSERT group.service[i] = NIL*>
	  <*ASSERT group.refCount[i] = 0*>
	END;
      END;
    END;
    
    LOCK mu DO 
      status := idTable.delete(group.gid, r);
      <*ASSERT group = r*>
      <*ASSERT map[group.gid] = TRUE*>
      map[group.gid] := FALSE;
    END;
  END ShutDown;

VAR
  nWaiting: CARDINAL;
  nTimeouts: CARDINAL;
  nGroupCommits: CARDINAL;
  nBroadcasts: CARDINAL;
  nSoloCommits: CARDINAL;
  groupCommitLSN: WAL.LSN;
  groupCommitCond := NEW(Thread.Condition);
  timeout: BOOLEAN;
  
PROCEDURE Timeout (<*UNUSED*>r: REFANY) =
  BEGIN
    LOCK walMu DO 
      IF SpyTime THEN INC(nTimeouts); END;
      timeout := TRUE;
      Thread.Broadcast(groupCommitCond);
    END;
  END Timeout;
  
PROCEDURE FlushDefaultLog (lsn: WAL.LSN) =
  BEGIN
    (* This yield is here to avoid weird starvation that I have experienced. 
       A locks the SCSI lock.
       B comes, and waits for the SCSI lock
       LOOP 
         A does I/O, and waits in biowait
         A is waken up, and its quanta is reset.
         A unlocks the SCSI lock, but thread is not switched because
           A and B have same priority, and A's time is not up.
         A does short computation without using up the quanta
         A locks the SCSI lock.
       END;
      *)
    Strand.Yield();
    LOCK walMu DO
      IF nOutstandingTrans = 1 THEN
	IF SpyTime THEN INC(nSoloCommits); END;
	WAL.Flush(log, lsn);
	RETURN;
      END;

      (* Simplified group commit. If there are more than 1 active
	 transactions, wait for 5ms or other transaction arrives, and
	 try batched flush. *)
      INC(nWaiting);
      groupCommitLSN := MAX(groupCommitLSN, lsn);
      lsn := groupCommitLSN;
      IF nWaiting >= nOutstandingTrans THEN
	IF SpyTime THEN INC(nBroadcasts); END;
	Thread.Broadcast(groupCommitCond);
      ELSE
	IF SpyTime THEN INC(nGroupCommits); END;
	Clock.SetAlarm(5, Timeout, NIL); (* 5ms wait. *)
	Thread.Wait(walMu, groupCommitCond);
      END;
      DEC(nWaiting);
      (* Note: groupCommitLSN doesn't have to be reset because
	 LSN increases monotonically. *)
    END;
    WAL.Flush(log, lsn);
  END FlushDefaultLog;
  

PROCEDURE PrintStat () =
  BEGIN
    IO.Put("transaction status:\n");
    IO.Put("nGroupCommits: " & Fmt.Int(nGroupCommits) & ".\n");
    IO.Put("nTimeouts: " & Fmt.Int(nTimeouts) & ".\n");
    IO.Put("nSoloCommits: " & Fmt.Int(nSoloCommits) & ".\n");
    IO.Put("nBroadcasts: " & Fmt.Int(nBroadcasts) & ".\n");
  END PrintStat;


BEGIN
  mu := NEW(MUTEX);
  walMu := NEW(MUTEX);
  idTable := NEW(FastIntRefTbl.Default).init();

  HostID.Init();
  default := NEW(T);
  EVAL Init(default);
END TransGroup.

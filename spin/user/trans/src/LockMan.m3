(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Merged lock table and activetrans table.
 * 26-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE LockMan;
IMPORT RefList, RefSeq, LockQ;
IMPORT Storage, StorageRep;
IMPORT LockMode, LockRecord;
IMPORT TransError;
IMPORT IO, Thread;
IMPORT ActiveTrans, ActiveTransRep;
FROM TransUtils IMPORT DetectDeadlock;

  
(* See if there is a wait path starting from "s" and ending at "tr".
   If there is, return TRUE.
 *)
PROCEDURE DetectWaitCycle (at, src: ActiveTrans.T): BOOLEAN =
  VAR cur := src.waitingFor;
  BEGIN
    WHILE cur # NIL DO
      <*ASSERT cur.head # at *> (* you usually don't wait for yourself *)
      
      IF cur.head  = at THEN RETURN TRUE; END;
      IF DetectWaitCycle(at, cur.head) THEN RETURN TRUE; END;
      cur := cur.tail;
    END;
    RETURN FALSE;
  END DetectWaitCycle;

  
PROCEDURE Init (at: ActiveTrans.T) =
  BEGIN
    IF at.locks = NIL THEN 
      at.locks := NEW(RefSeq.T);
    END;
    at.waitingFor := NIL;
    EVAL at.locks.init();
  END Init;
  
PROCEDURE Lock (at:ActiveTrans.T; st: Storage.T;
		mode: LockMode.T; from, end: INTEGER;
		<*UNUSED*>timeout: INTEGER): INTEGER =
  VAR
    itr: LockQ.Iterator;
    c, prevLock: LockRecord.T;
    gotLock: BOOLEAN;
  BEGIN
    IF mode = LockMode.T.None THEN RETURN 0; END;
    
    (* XXX should set timer handler here *)
    LOCK st.lockMu DO
      LOOP
	(* Check if the lock can be acquired. *)
	(* XXX this has to be rewritten using interval tree *)
	
	at.waitingFor := NIL;
	gotLock := TRUE;
	
	itr := LockQ.Iterate(st.locks);
	prevLock := st.locks;

	(* Scan all the existing locks on the storage and check the
	   conflict. *)
	WHILE LockQ.NextItr(itr, c) DO
	  IF c.at = at AND c.mode = mode THEN
	    IF from >= c.from AND end <= c.end THEN
	      (* Already holding lock on the region. *)
	      RETURN 0;
	    ELSIF c.end = from THEN
	      (* XXX this doesn't work.. just extend the lock.. *)
	      c.end := end;
	      RETURN 0;
	    ELSIF end = c.from THEN
	      c.from := from;
	      RETURN 0;
	    END;
	  END;
	  
	  IF c.from < from AND prevLock = NIL THEN
	    prevLock := c;
	  END;
	  
	  (* LockRecords are sorted in "from" order. so
	     we can exit if "c" is larger than "from" and it doesn't
	     overlap with [from, end]). *)
	  (* XXX doesn't work.... *)
	  (* IF c.end <= from THEN EXIT; END; *)
	  
	  IF from < c.end AND end > c.from THEN
	    (* overlapping lock found *)
	    IF c.at = at THEN
	      IF mode = LockMode.T.Read OR c.mode = LockMode.T.Write THEN
		(* We already have a lock that's no weaker than the
		   requested one. Just ignore the current lock. *)
		RETURN 0;
	      END;
	    ELSE
	      IF c.mode = LockMode.T.Write OR mode = LockMode.T.Write THEN
		(* conflict! you can't have two write locks on a same
		   region *)
		IF DetectDeadlock THEN
		  VAR curHolder := at;
		  BEGIN
		    IF DetectWaitCycle(at, curHolder) THEN
		      RETURN TransError.DEADLOCK;
		    END;
		    
		    IF NOT RefList.Member(at.waitingFor, curHolder) THEN
		      at.waitingFor := RefList.Cons(curHolder, at.waitingFor);
		    END;
		  END;
		END;
		IO.Put("waiting for release.\n");
		TRY 
		  Thread.AlertWait(st.lockMu, st.sync);
		  (* XXX This actually doesn't work because AlertWait is not
		     implemented. *)
		EXCEPT
		| Thread.Alerted => 
		  Thread.Release(st.lockMu);
		  RETURN TransError.TIMEOUT;
		END;
		gotLock := FALSE;
		EXIT; (* from while loop and start over again. *)
	      END;
	    END;
	  END;
	END;
	
	IF gotLock THEN
	  c := LockQ.Allocate();
	  c.at := at;
	  c.from := from;
	  c.end := end;
	  c.mode := mode;
	  c.st := st;
	  LockQ.InsertHead(prevLock, c);
	  at.locks.addhi(c);
	  RETURN 0;
	END;
      END; (* loop *)
    END; (* lock *)
  END Lock;
  
PROCEDURE IsLocked (at: ActiveTrans.T; st: Storage.T;
		    mode: LockMode.T; from, end: INTEGER): BOOLEAN =
  VAR
    itr: LockQ.Iterator;
    curLock: LockRecord.T;
  BEGIN
    LOCK st.lockMu DO 
      itr := LockQ.Iterate(st.locks);
	
      WHILE LockQ.NextItr(itr, curLock) DO
	
	(* LockRecords are sorted in "from" order. so
	   we can exit if "c" is larger than "from" and it doesn't
	   overlap with [from, end). *)
	IF curLock.end <= from THEN
	  RETURN FALSE;
	END;
	
	IF curLock.at = at
	  AND curLock.from <= from
	  AND curLock.end >= end THEN
	  (* "curLock" includes the region [from .. end] *)
	  IF mode = LockMode.T.Read THEN
	    (* read or write lock is ok. *)
	    RETURN TRUE;
	  ELSIF curLock.mode = LockMode.T.Write THEN
	    RETURN TRUE;
	  END;
	END;
      END;
    END;
    RETURN FALSE;
  END IsLocked;

PROCEDURE UnlockAll (at: ActiveTrans.T; st: Storage.T) =
  VAR
    lock: LockRecord.T;
  BEGIN
    LOCK st.lockMu DO 
      FOR i := 0 TO at.locks.size()-1 DO 
	lock := NARROW(at.locks.get(i), LockQ.T);
	<*ASSERT st = lock.st*>

	<*ASSERT LockQ.Member(st.locks, lock) *>
	LockQ.Remove(lock);
	LockQ.Free(lock);
      END;
    END;
    
    Thread.Broadcast(st.sync);
  END UnlockAll;

PROCEDURE Iterate (at: ActiveTrans.T; st: Storage.T): Iterator =
  BEGIN
    RETURN Iterator{at := at, st := st, lock := st.locks.next};
  END Iterate;

PROCEDURE NextItr (VAR itr: Iterator; VAR lock: LockRecord.T): BOOLEAN =
  BEGIN
    LOOP
      IF itr.lock = itr.st.locks THEN
	(* wrapped round, ie, visited all the items. *)
	RETURN FALSE;
      END;
      
      lock := itr.lock;
      itr.lock := itr.lock.next;
      IF lock.at = itr.at THEN
	RETURN TRUE;
      END;
    END;
  END NextItr;
  
BEGIN  
END LockMan.

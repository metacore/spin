(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	2PC recovery.
 * 27-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransRecovery EXPORTS TransPrivate;
IMPORT CheckPoint;
IMPORT WAL;
IMPORT IntRefTbl;
IMPORT Thread;
IMPORT TID, TransProxy;
IMPORT TransRPC;
IMPORT TransGroup;
FROM TransUtils IMPORT Msg;
IMPORT Fmt;

VAR lastActiveTrans: IntRefTbl.T := NIL;
  (*List of transactions active just before the last crash *)
  cond := NEW(Thread.Condition);
  mu := NEW(MUTEX);
  
PROCEDURE Dump (<*UNUSED*>log: WAL.T): IntRefTbl.T =
BEGIN
  (* XXX *)
  RETURN NEW(IntRefTbl.Default).init();
END Dump;

PROCEDURE RecoveryStart(<*UNUSED*>log: WAL.T; <*UNUSED*>READONLY info: CheckPoint.T) =
BEGIN
END RecoveryStart;

PROCEDURE RecoveryFinished(<*UNUSED*>log: WAL.T; READONLY info: CheckPoint.T) =
BEGIN
  LOCK mu DO 
    lastActiveTrans := info.trans;
    Thread.Signal(cond);
  END;
END RecoveryFinished;

(* Resolve local or cached transaction. It returns TRUE if
   it was able to find the entry in the table, and in that case,
   "commit" indicates whether the transaction is to be committed or
   aborted. If the proc returns FALSE, the value of "commit" is undefined. *)
   
PROCEDURE ResolveLocal (tid: TID.T; VAR commit: BOOLEAN): BOOLEAN =
  VAR
    r: REFANY;
    tr: REF CheckPoint.LocalTrans;
  BEGIN
    LOCK mu DO 

      (* Wait till recovery finishes *)
      WHILE lastActiveTrans = NIL DO
	Thread.Wait(mu, cond);
      END;

      IF lastActiveTrans.get(tid, r) THEN
	(* Transaction is either local, or already has resolved before. *)
	tr := r;
	IF tr.state = CheckPoint.LState.Prepared THEN
	  Msg("transrecovery.resolve:trans ", Fmt.Int(tid), " is prepared.\n");
	  commit := TRUE;
	ELSE
	  Msg("transrecovery.resolve:trans ", Fmt.Int(tid), " is at ",
	      Fmt.Int(ORD(tr.state)));
	  commit := FALSE;
	END;
	RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END ResolveLocal;
  
PROCEDURE Resolve (tid: TID.T): BOOLEAN =
  VAR
    tr: REF CheckPoint.LocalTrans;
    commit: BOOLEAN;
  BEGIN
    IF ResolveLocal(tid, commit) THEN
      RETURN commit;
    END;

    (* Query the remote trans manager *)
    commit := TransProxy.Resolve(tid);
    
    LOCK mu DO
      tr := NEW(REF CheckPoint.LocalTrans);
      IF commit THEN
	tr.state := CheckPoint.LState.Prepared;
      ELSE
	tr.state := CheckPoint.LState.Active;
      END;
      (* Cache the result *)
      EVAL lastActiveTrans.put(tid, tr);
    END;
  
    RETURN commit;
  END Resolve;

PROCEDURE ResolveTransInDoubt (in: TransRPC.RecvBuf;
			       out: TransRPC.SendBuf;
			       group: TransGroup.T) =
  VAR
    tid := in.unpackInt();
    state, commit: BOOLEAN;
  BEGIN
    state := ResolveLocal(tid, commit);
    <*ASSERT state*>
    out.packInt(TransRPC.OK);
    out.packBool(commit);
  END ResolveTransInDoubt;
  
  
BEGIN
END TransRecovery.


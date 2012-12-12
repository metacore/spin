(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)


INTERFACE CheckPoint;
IMPORT IntRefTbl;
IMPORT SIDRefTbl;
IMPORT WAL;
IMPORT SID;

TYPE
  (* List of the transactions initiated on this site*) 
  LocalTrans = RECORD
    state: LState;
  END;

  (* "LState" is the state of transactions initiated on the local host. *)
  LState =
    {
     Active,  (* not commited nor aborted *)
     Prepared, (* commit message sent, but not all reply collected *)
     Committed, (* finished : not used , because Committed entry is
		 removed from the table. *)
     Aborted (* aborted : not used *)
     };
  
  (* List of transactions in which a storage manager is participating. *)
  RemoteTrans = RECORD
    state: RState;
    firstLSN: WAL.LSN; (* the smallest LSN belonging to the transaction. *)
    lastLSN: WAL.LSN; (* the largest LSN belonging to the transaction. *)
    locks: REF ARRAY OF WAL.PrepareLockEntry;
  END;
  
  DirtyPage = RECORD
    recoveryLSN: INTEGER;
  END;

  (* "RState" is the state of a transaction in which a local storage is
     participating. *)
  RState = {
	    Active,  (* not commited nor aborted *)
	    Prepared, (* prepared. no new request accepted for this
		       transaction *)
	    Commited,(* commited *)
	    Abort (* aborted *)
	    };
  
  Storage <: StoragePublic;
  StoragePublic = OBJECT
    sid: SID.T;
    trans: IntRefTbl.T; (* tid -> RemoteTrans *)
    dirtyPages: IntRefTbl.T; (* pagePos -> DirtyPage *)
  METHODS
    init(sid: SID.T): Storage;
  END;
  
  T = RECORD
    trans: IntRefTbl.T; (* tid -> LocalTrans *)
    (* List of active transaction at the moment of the crash.
     If someone queries about the transaction that is not in this
     table, we simply reply "it's aborted". *)
     
    storage: SIDRefTbl.T; (* sid -> Storage *)
    firstAnalysisLSN: INTEGER;
    (* the lsn from which we have to start analysis pass *)
    firstRedoLSN: INTEGER;
    (* the lsn from which we have to start recovery pass *)
  END;

PROCEDURE Undump (log: WAL.T; lsn: WAL.LSN; (*OUT*)VAR info: T);
PROCEDURE Dump (log: WAL.T; READONLY info: T);

END CheckPoint.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 17-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 * html
 *)

(* This module implements local raw file management.

 *)

INTERFACE StorageLocal;
IMPORT Storage;
IMPORT TID;
IMPORT SID;
IMPORT RawIO;
IMPORT WAL;
IMPORT SIDRefTbl;
IMPORT CheckPoint;
IMPORT Error;
CONST
  CheckPointThreshold = 2;
  
TYPE
  T <: TPublic;
  TPublic = Storage.T BRANDED OBJECT
    refCnt: INTEGER; (* now many times opened? *)
    fh: RawIO.T; (* file device *)
    log: WAL.T; (* log device *)
  END;

VAR
  openFiles: SIDRefTbl.T; (* list of open storages. SID -> Storage.T*)
  openFilesMu: MUTEX; (* guards openFiles *)
  
(* Below are recovery procedures. *)

    
PROCEDURE Redo(log: WAL.T; cs: CheckPoint.Storage;
	       READONLY content: ARRAY OF CHAR;
	       pos: INTEGER): WAL.RecoveryResult;
PROCEDURE Undo(log: WAL.T; cs: CheckPoint.Storage;
	       READONLY content: ARRAY OF CHAR;
	       pos: INTEGER): WAL.RecoveryResult;

PROCEDURE RecoveryStart(log: WAL.T; READONLY info: CheckPoint.T);
PROCEDURE RecoveryFinished(log: WAL.T; READONLY info: CheckPoint.T);
PROCEDURE FreeupLogSpace(): WAL.LSN;
(* This is an upcall from WAL, and is called when log space becomes scarce.
   Storage manager looks over all the buffer it has, and see up till what
   LSN the log records can be discarded. Returns the maximum LSN up to which
   the records can be discarded. *)

PROCEDURE ResolveUncommitedTrans(log: WAL.T; sid: SID.T; tid: TID.T;
				 rt: REF CheckPoint.RemoteTrans);
(* This is called for each <st, tid> pair in which the transaction "tid"
   is in prepare state in "st"'s log record. This procedure spawns off
   a thread that poll the transaction manager about the outcome of
   the transaction "tid", and if committed, it commits everything, and
   else it aborts manually. *)
  
PROCEDURE Poll(sid: SID.T; tid: TID.T);
(* This is a rpc handler. Called by remote transaction manager to
   commit the transaction "tid" on the storage "sid". *)
  
PROCEDURE Dump(log: WAL.T): SIDRefTbl.T;
(* Take checkpoint dump and return the information on WordRefTbl.T.
   See "CheckPoint" also.
 *)

PROCEDURE Open(fileName: TEXT; log: WAL.T := NIL): T RAISES {Error.E};
PROCEDURE OpenFromID(sid: SID.T; log: WAL.T := NIL)
	: T RAISES {Error.E};


  

END StorageLocal.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

INTERFACE StorageProtected;
IMPORT Storage;
IMPORT ActiveTrans, ActiveTransQ;
IMPORT LockMode;
IMPORT Buffer;
IMPORT TransT;
IMPORT WAL;
PROCEDURE OperateOnRegion(st: Storage.T; at: ActiveTrans.T;
			  lock: LockMode.T; pos, len: INTEGER; 
		   callback: PROCEDURE(VAR c: ARRAY OF CHAR;
				       buf: Buffer.T; pos: INTEGER));
(* XXX we should use this instead of the method, since this is not
   a dynamically dispatched method.*)
  

PROCEDURE FindTrans(st: Storage.T; tr: TransT.T): ActiveTrans.T;
PROCEDURE InternTrans(st: Storage.T; tr: TransT.T): ActiveTrans.T;
(* Join the storage "st" into the transaction "tid" that originated in the
   Pre: "st" is locked. *)
  
PROCEDURE ModifyPage(st: Storage.T; page: Buffer.Offset;
		     READONLY diff: ARRAY OF CHAR; lsn := LAST(WAL.LSN));
(* Modify the storage content directly.
   "buf.lsn" is set to be "lsn". Pre: "st" is locked. *)

TYPE
  Iterator = ActiveTransQ.Iterator;
  
PROCEDURE IterateTrans(st: Storage.T): Iterator;
  (* Iterate over all the outstanding transaction on the storage "st".
     Use "ActiveTransQ.NextItr" to get the next item.
     Pre: "st" is locked.*)
  
PROCEDURE FlushModificationsForTrans(st: Storage.T; at: ActiveTrans.T)
  : BOOLEAN;
(* Flush all modification REDO logs queued up so far. Returns TRUE
   if there was any mods. *)
  
END StorageProtected.

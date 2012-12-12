(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(*
 This intf reveals all the data types related to Transaction, and
 exports some procedures used by a storage manager or a remote transaction
 manager peer. *)
 
INTERFACE TransPrivate;
IMPORT TID;
IMPORT IntRefTbl;
IMPORT CheckPoint;
IMPORT WAL;
IMPORT TransT;
IMPORT TransService;
IMPORT TransRPC;
IMPORT TransGroup;

VAR
  log: WAL.T;
  (* Log file used by this transaction manager *)

  logFileName: TEXT;
  
PROCEDURE Dump(log: WAL.T): IntRefTbl.T;
(* Take checkpoint dump and return the information on IntRefTbl.T.
   See "CheckPoint" also.
 *)
  
PROCEDURE RecoveryStart(log: WAL.T; READONLY info: CheckPoint.T);
PROCEDURE RecoveryFinished(log: WAL.T; READONLY info: CheckPoint.T);
PROCEDURE FreeupLogSpace(): WAL.LSN;
  (* This is an upcall from WAL, and is called when log space becomes scarce.
   Storage manager looks over all the buffer it has, and see up till what
   LSN the log records can be discarded. Returns the maximum LSN up to which
   the records can be discarded. *)

PROCEDURE Resolve(tid: TID.T): BOOLEAN;
(* See if the transaction "tid" is committed or not.
  "tid" can be a remote transaction, in which case, this proc
 does rpc. *)
PROCEDURE ResolveTransInDoubt (in: TransRPC.RecvBuf;
			       out: TransRPC.SendBuf;
			       group: TransGroup.T);

  
PROCEDURE Join(tr: TransT.T; st: TransService.T);
(* storage man -> local transman.

   Add the storage "st" to the transaction "tr".
   This is called both by proxy storage manager on the client side, and
   remote proxy manager on the server side. 
 *)
  
PROCEDURE Unjoin(tr: TransT.T; st: TransService.T);
(* Detach the storage "st" from the transaction. *)

PROCEDURE FlushDefaultLog(lsn: WAL.LSN);
  
VAR nNonAtomicTrans: CARDINAL;
  (* # of nonatomic local transactions currently taking place.
     This is used by BufferPurge to force write the buffer page
     when there is any nonatomic transaction running. This is because
     non-atomic transactions may modify the buffer without calling setrange
     beforehand(precisely, they call setrange the first time, but if the page
     is swapped out and then swapped in later, they may modify the page
     without setrange because they think the previous setrange is still
     valid. I know this is ugly. The real solution is to use dirty bit
     from pmap(but pmap doesn't provide dirty bit as of Dec 23 1996). *)
     
  
END TransPrivate.

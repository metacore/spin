(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added the iterator.
 * 26-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 * html
 *)

(*
   The lock manager runs in the same machine as [Storage] manager.
   LockMan provides fine-grain locking for a storage.
   There is one lockman on each node, and it manages all the
   storages running on that node.
 *)
INTERFACE LockMan;
IMPORT LockRecord;
IMPORT ActiveTrans;
IMPORT LockMode;
IMPORT Storage;

PROCEDURE Init(tr: ActiveTrans.T);
(* Initialize the lock related fields in "tr". *)
  
(*
   Lock Manager supports locking at byte unit. However, currently
   storage manager only locks in page granularity. 

   Locks are `recursive'. Recursive means that you can lock the
   same region many times as long as you belong to the same transaction.
   Also, you can promote a lock by specifying stronger "mode" than
   the one you currently hold.

   Returns one of [TransError] error codes.
 *)
PROCEDURE Lock(at: ActiveTrans.T; st: Storage.T;
	       mode: LockMode.T; from, end: INTEGER;
	       timeout: INTEGER := LAST(INTEGER)): INTEGER;
  

PROCEDURE IsLocked(at: ActiveTrans.T; st: Storage.T;
		   mode: LockMode.T; from, end: INTEGER): BOOLEAN;
  
(*
   The trans manager implements phase locking in a conservative way; ie,
   all the locks you hold has to be retained until you finish
   (i.e., commit or abort) the transaction. Therefore, there is no
   proc to unlock an individual region.
 *)
  
PROCEDURE UnlockAll(at: ActiveTrans.T; st: Storage.T);
(* "UnlockAll" removes all the locks held by "tr". *)

TYPE Iterator = RECORD
  at: ActiveTrans.T;
  st: Storage.T;
  lock: LockRecord.T;
END;
  
PROCEDURE Iterate(at: ActiveTrans.T; st: Storage.T): Iterator;
  (* Start iteration. Pre: "st" is locked. *)
PROCEDURE NextItr(VAR itr: Iterator; VAR lock: LockRecord.T): BOOLEAN;
  (* Get the next item from the iterator "itr". Returns TRUE and
     "lock" points to the lock information block if an item exists.
     Returns FALSE if reached end of data. *)
  
END LockMan.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 * html
 *)

(*
   "Transaction" provides atomic updates on file regions.

   This module is the topmost interface to create and terminate
   transactions. The access to actual transactional resources is provided
   by separate modules like [Storage].
*)

INTERFACE Transaction;
IMPORT Error;
IMPORT TID;
IMPORT WAL;
IMPORT TransMode;
IMPORT TransT;
IMPORT TransGroup;

TYPE
  T = TransT.T;
  (* "T" represents a transaction. This is just a opaque ref. *)

(*
 *** Starting and ending a transaction.
 *)
PROCEDURE Begin(group: TransGroup.T; mode := TransMode.Default) : T;
(* Begin a transaction. See [TransMode] for how "mode" affects the
   behavior of the transaction. "group" is used to identify the client.
   See also [TransGroup]. *)

PROCEDURE Commit(t: T) : BOOLEAN RAISES {Error.E};
(* Commit the transaction and make all the changes made to data
   permanent. Also, all the locks are released, and all the pins
   are released. Returns "TRUE" if
   the work was succesful. *)

PROCEDURE Abort(t: T) RAISES {Error.E};
(* Aborts the transaction. All the changes to data are restored to the
   state before the transaction start. Also, all the locks are released and
   all the pins are released.
 *)

(*
   Utilities
 *)
PROCEDURE GetTID(t: T) : TID.T;
(* Return the transaction ID. Transaction ID is network global bitsequence for
   the transaction. *)

PROCEDURE CheckPoint() RAISES {Error.E};
(* Take a checkpoint of everything that is on this machine. *)
  
PROCEDURE GetDefaultLogDevice(): WAL.T;
(* Get the log device the transaction manager is using to log the
   two phase commit stuff. *)

PROCEDURE FlushLogs();
(* Flush all the log files opened currently. You usually don't have to
   call this because log is usually flushed when transaction commits.
   The reasons this is provided are:
   <ol>
   <li> RVM compatibility.
   <li> <dfn>Noflush</dfn> transactions will be introduced in future.
   You need explicit flush in such transactions.
   </ol>
   *)
  
PROCEDURE Init();
(* Initialize everything and open the log device.
   
   XXX this should not be here. *)

(*
   Below are means to carry the transaction context with the thread.
*)
PROCEDURE AttachTransactionToCurrentThread(t: T);
  (* Bind the transaction "t" to the current kernel thread. *)
  
PROCEDURE GetCurrent(): T;
  (* Get the transaction attached to the current kernel thread. *)
  
END Transaction.




(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* "T" is a record created inside the storage manager for each transaction
   that involves the storage. Mapping between "TransCommon.T" and
  "T" is one to many. It is one to one if only one storage manager is
   used by the transaction.

   XXX The module name is not very informative.
*)

INTERFACE ActiveTrans;
IMPORT TransT;

TYPE T <: ROOT;

PROCEDURE Init(at: T; tr: TransT.T);

PROCEDURE Destroy(at: T);
(* Free the resources held by "at" and free "at" itself.
   Since it modifies the queue header, the storage to which "at"
   belongs to must be locked beforehand. *)
  
END ActiveTrans.

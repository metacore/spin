(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* "LockRecord.T" is a internal structure used by the LockManager.
 Each "LockRecord.T" represents a region that is locked by a client.
 *)
 
INTERFACE LockRecord;
IMPORT LockMode, Storage, Q;
IMPORT ActiveTrans;

TYPE T = Q.T OBJECT 
  at: ActiveTrans.T;
  st: Storage.T;
  from, end: INTEGER;
  mode: LockMode.T;
END;

CONST Brand = "LockRecord";

END LockRecord.

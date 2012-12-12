(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Introduced the <host, local> structure on TID.
 * 13-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* TID.T is a globally unique ID for the transaction.
 It has a internal structure; the first half is a host ID portion, and this
 identifies the host on which the transaction coordinator resides.
 The latter half is a local ID portion.*)
INTERFACE TID;
IMPORT HostID;

CONST
  (* invalid tid *)
  Void = -1;
  
TYPE T = INTEGER;

PROCEDURE GetHostID(tid: T): HostID.T;
PROCEDURE GetLocal(tid: T): INTEGER;
PROCEDURE CreateT(hid: HostID.T; local: INTEGER): T;

END TID.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Some manipulation procs available only to trusted servers, ie,
   storage managers. *)

INTERFACE TransGroupPrivate;
IMPORT TransGroup;
IMPORT Storage;
IMPORT TransT;
IMPORT TID;
IMPORT SID;

PROCEDURE FindFromGid(gid: TransGroup.ID): TransGroup.T;

(* For all the procedures below,
 Pre: "t" is unlocked.
 *)
  
PROCEDURE InternTrans(t: TransGroup.T; tid: TID.T): TransT.T;
(* Add the transaction "tr" to "t". *)

PROCEDURE DeleteTrans(tr: TransT.T);

PROCEDURE FindResource(t: TransGroup.T; id: SID.T): Storage.T;
(* Find storage "id" from the group. *)
  
PROCEDURE RegisterResource(t: TransGroup.T; re: Storage.T);
(* Duplicate register is allowed. *)
  
PROCEDURE DeleteResource(t: TransGroup.T; re: Storage.T);

PROCEDURE PrintStat();
END TransGroupPrivate.




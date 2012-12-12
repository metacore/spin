(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Transaction Manager also acts as a local coordinator of a
 transaction proceeding on another host. 
 "T" manages a transaction that is going on on a host
 other than this host. *)

INTERFACE TransRemote;
IMPORT TransCommon;

TYPE T = TransCommon.T BRANDED OBJECT END;

END TransRemote.

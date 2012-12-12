(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* This modules is an server side interface to remote storage manipulation *)

INTERFACE StorageRemote;
IMPORT TransRPC;
IMPORT TransGroup;

CONST
  (* The below two are Read "op" values *)
  LockAndRead = VAL(2, CHAR);
  (* Lock the page and return the contents. *)
  Lock = VAL(3, CHAR);
  (* Lock the page and return the contents only if the page is newer than
     the specified LSN.
     XXX LSN checking is not implemented now!!! *)

TYPE
  CallbackProc = PROCEDURE(in: TransRPC.RecvBuf;
			   out: TransRPC.SendBuf; group: TransGroup.T);

VAR
  Callbacks: ARRAY [TransRPC.Hello .. TransRPC.Last] OF CallbackProc;

END StorageRemote.

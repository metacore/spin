(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new spin shell commands.
 *
 *)

(* Untrusted *)
INTERFACE IcmpClassification;
IMPORT Net;
VAR debug_level : Net.Level := Net.oLevel.NODEBUG;
PROCEDURE Init(verbose:BOOLEAN);
END IcmpClassification.

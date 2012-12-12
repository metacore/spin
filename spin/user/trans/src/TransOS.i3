(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* This module defines various platform dependent procedures. *)
 
INTERFACE TransOS;
IMPORT SID;

PROCEDURE SIDToFileName(sid: SID.T): TEXT;
PROCEDURE FileNameToSID(fileName: TEXT): SID.T;
PROCEDURE GetDefaultLogFileName(): TEXT;
PROCEDURE Exit(x: INTEGER);
(* Terminate the process *)

END TransOS.

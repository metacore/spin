(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE VMDebug;
IMPORT RefRefTbl;
IMPORT PhysAddr;

CONST DebugMessage = FALSE;
(* Turn on very verbose debugging messages. *)
  
CONST DebugStat = TRUE;
(* Turn on somewhat verbose debugging messages. *)

CONST DoChecksum = FALSE;
(* Turn on checksumming; remember the checksum for each purged page, compare
   it upon checkin. *)

VAR
  mu: MUTEX;
  memObjTbl: RefRefTbl.T;
  spaceTbl: RefRefTbl.T;

PROCEDURE CalculateChecksum(p: PhysAddr.T): INTEGER;
  
PROCEDURE PrintStat();
  
END VMDebug.

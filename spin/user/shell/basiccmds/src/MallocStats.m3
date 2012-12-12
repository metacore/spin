(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *)
MODULE MallocStats;

IMPORT SALDECUnix, ParseParams;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
BEGIN
  SALDECUnix.MallocStats();
  RETURN TRUE;
END Run;

BEGIN
END MallocStats.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Whisted. ParseParams
 *
 *)
MODULE Dump;
IMPORT Log, ParseParams;

PROCEDURE Run (<* UNUSED *>pp: ParseParams.T): BOOLEAN =
  BEGIN
    Log.Dumplog();
    RETURN TRUE;
  END Run;

BEGIN
END Dump.

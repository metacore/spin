(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 21-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Whisted. ParseParams.
 *
 *)
MODULE Halt;

IMPORT SALPrivate, ParseParams;

PROCEDURE Run (<*UNUSED*> pp: ParseParams.T) : BOOLEAN =
  BEGIN
    SALPrivate.Halt();
    RETURN TRUE;
  END Run;

BEGIN
END Halt.

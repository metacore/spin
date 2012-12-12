(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Jan-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Yield from the shell thread.
 *)
MODULE Yield;

IMPORT Strand, ParseParams;

PROCEDURE Run (<*UNUSED*>pp: ParseParams.T): BOOLEAN =
  BEGIN
    Strand.Yield();
    RETURN TRUE;
  END Run;

BEGIN
END Yield.

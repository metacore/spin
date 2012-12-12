(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 26-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)
MODULE Help;

IMPORT Shell, IO, ParseParams;

PROCEDURE Run(<*UNUSED*>pp: ParseParams.T) : BOOLEAN = 
  BEGIN
    IO.Put("Help.Run running\n");
    Shell.Help();	(* would like to implement help cmd *)
    RETURN TRUE;
  END Run;

BEGIN
END Help.


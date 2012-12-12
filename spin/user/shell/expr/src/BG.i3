(*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 * 02-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)
INTERFACE BG;

IMPORT ParseParams;

CONST CommandName = "bg";
CONST CommandHelp = " -- run the remaining args as a command in the background";

PROCEDURE Run (pp: ParseParams.T) : BOOLEAN;

END BG.

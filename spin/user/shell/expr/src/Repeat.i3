(*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 * 16-May-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

INTERFACE Repeat;

IMPORT ParseParams;

CONST CommandName = "repeat";
CONST CommandHelp = " repeat n command args\n";

PROCEDURE Run (pp: ParseParams.T) : BOOLEAN;

END Repeat.

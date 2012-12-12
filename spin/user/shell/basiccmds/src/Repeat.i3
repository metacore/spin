(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 13-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

INTERFACE Repeat;

IMPORT ParseParams;

CONST CommandName = "repeat";
CONST CommandHelp = " -- repeat commands <n> times ";

PROCEDURE Run(pp: ParseParams.T) : BOOLEAN;

END Repeat. 


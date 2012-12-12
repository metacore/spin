(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 21-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)
INTERFACE Set;

IMPORT ParseParams;

CONST CommandName = "set";
CONST CommandHelp = " variable value";

PROCEDURE Run (<*UNUSED*> pp: ParseParams.T) : BOOLEAN;

END Set.



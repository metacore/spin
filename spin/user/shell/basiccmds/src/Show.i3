(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 10-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *)

INTERFACE Show;
IMPORT ParseParams;
CONST CommandName = "show";
CONST CommandHelp = " [-html] jobs|types|threads|nlist";
PROCEDURE Run (pp: ParseParams.T): BOOLEAN;
END Show.
















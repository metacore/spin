(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Whisted. Added ParseParams.
 *
 *)
INTERFACE Echo;

IMPORT ParseParams;

CONST CommandName = "echo";
CONST CommandHelp = "string -- echo string to keyboard";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END Echo.




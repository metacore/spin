(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 1-Aug-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *)
INTERFACE BigWrite;

IMPORT ParseParams;

CONST BRAND = "BigWrite";

CONST CommandName = "bigwrite";
CONST CommandHelp = "bigwrite path number string";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END BigWrite.







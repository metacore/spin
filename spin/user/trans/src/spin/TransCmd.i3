(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE TransCmd;
IMPORT ParseParams;

CONST CommandName = "trans";
CONST CommandHelp = " [-s]";
PROCEDURE Run (pp : ParseParams.T) : BOOLEAN;

END TransCmd.

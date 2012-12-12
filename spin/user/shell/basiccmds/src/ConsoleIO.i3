(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 *)
INTERFACE ConsoleIO;

IMPORT ParseParams;

CONST CommandName = "console";
CONST CommandHelp = "terminal|file";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END ConsoleIO.




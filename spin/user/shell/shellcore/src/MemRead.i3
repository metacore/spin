(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-May-96  Stefan Savage (savage) at the University of Washington
 *	Allows you to read an address from the shell.
 *)
INTERFACE MemRead;

IMPORT ParseParams;

CONST CommandName = "memread";
CONST CommandHelp = " address";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END MemRead.

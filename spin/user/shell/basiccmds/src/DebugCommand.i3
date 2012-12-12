(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)
INTERFACE DebugCommand;

IMPORT ParseParams;

CONST CommandName = "debug";
CONST CommandHelp = " go to the debugger";

PROCEDURE Run (pp: ParseParams.T) : BOOLEAN;

END DebugCommand.





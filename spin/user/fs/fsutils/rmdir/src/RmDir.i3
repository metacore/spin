(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *
 *)
INTERFACE RmDir;

IMPORT ParseParams;

CONST BRAND = "RmDir";

CONST CommandName = "rmdir";
CONST CommandHelp = "rmdir filePath";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END RmDir.







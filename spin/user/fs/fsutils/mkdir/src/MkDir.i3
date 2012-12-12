(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 4-July-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *)
INTERFACE MkDir;

IMPORT ParseParams;

CONST BRAND = "MkDir";

CONST CommandName = "mkdir";
CONST CommandHelp = "mkdir filePath";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END MkDir.







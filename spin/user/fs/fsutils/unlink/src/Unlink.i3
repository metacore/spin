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
INTERFACE Unlink;

IMPORT ParseParams;

CONST BRAND = "Unlink";

CONST CommandName = "unlink";
CONST CommandHelp = "unlink filePath";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END Unlink.







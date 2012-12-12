(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 25-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *)
INTERFACE Cleanse;

IMPORT ParseParams;

CONST BRAND = "Cleanse";

CONST CommandName = "cleanse";
CONST CommandHelp = "cleanse path";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END Cleanse.







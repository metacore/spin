(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 5-Jun-96  Tim Bradley (tbradley) at the University of Washington
 *	Sync command for writing fs state to disk.  Created.
 *
 *)
INTERFACE Sync;

IMPORT ParseParams;

CONST BRAND = "Sync";

CONST CommandName = "sync";
CONST CommandHelp = "sync devName fsName";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END Sync.

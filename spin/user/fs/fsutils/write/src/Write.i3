(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 5-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *)
INTERFACE Write;

IMPORT ParseParams;

CONST BRAND = "Write";

CONST CommandName = "write";
CONST CommandHelp = "write <-t> path startLocationInBytes text";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END Write.







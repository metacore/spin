(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Oct-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *)
INTERFACE GetFreeList;

IMPORT ParseParams;

CONST BRAND = "GetFreeList";

CONST CommandName = "getfreelist";
CONST CommandHelp = "getfreelist path";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END GetFreeList.







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
INTERFACE Read;

IMPORT ParseParams;

CONST BRAND = "Read";

CONST CommandName = "read";
CONST CommandHelp = "read filePath startLocationInBytes lengthToRead";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END Read.







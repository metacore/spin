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
INTERFACE GetSegDesc;

IMPORT ParseParams;

CONST BRAND = "GetSegDesc";

CONST CommandName = "getsegdesc";
CONST CommandHelp = "getsegdesc path segmentNumber";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END GetSegDesc.







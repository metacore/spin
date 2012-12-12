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
INTERFACE SetCurrentSeg;

IMPORT ParseParams;

CONST BRAND = "SetCurrentSeg";

CONST CommandName = "setcurrentseg";
CONST CommandHelp = "setcurrentseg aValidLfsPath segmentNumber";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END SetCurrentSeg.







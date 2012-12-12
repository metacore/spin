(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)
INTERFACE Touch;

IMPORT ParseParams;

CONST BRAND = "Touch";

CONST CommandName = "touch";
CONST CommandHelp = "touch path";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END Touch.







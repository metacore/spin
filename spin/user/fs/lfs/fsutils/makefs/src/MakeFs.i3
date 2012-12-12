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
INTERFACE MakeFs;

IMPORT ParseParams;

CONST BRAND = "MakeFs";

CONST CommandName = "makefs";
CONST CommandHelp = "makefs dev blocks_in_segment [Format]";

PROCEDURE Run (pp: ParseParams.T):BOOLEAN;

END MakeFs.

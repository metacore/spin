(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 13-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE Unmount;

IMPORT ParseParams;

CONST CommandName = "unmount";
CONST CommandHelp = " path -- unmount device";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;

END Unmount.


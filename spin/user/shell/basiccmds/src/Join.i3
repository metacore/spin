(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Whisted.
 *)
INTERFACE Join;

IMPORT ParseParams;

CONST CommandName = "join";
CONST CommandHelp = " <commands> -- run commands and wait until all threads terminate";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;

END Join.



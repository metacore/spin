(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 26-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)
INTERFACE Script;

IMPORT ParseParams;

CONST CommandName = "script";
CONST CommandHelp = " [-i] file";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END Script.











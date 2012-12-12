(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added reset.
 *
 * 26-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)
INTERFACE SpyControl;

IMPORT ParseParams;

CONST CommandName = "spy";
CONST CommandHelp = " spy on|off|dump|reset";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;

END SpyControl.













(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 04-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Shell interface to manipulate kernel preemptible/ras regions.
 *)
INTERFACE RasControl;

IMPORT ParseParams;

CONST CommandName = "ras";
CONST CommandHelp = " [dump|np from to|check from]";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;

END RasControl.








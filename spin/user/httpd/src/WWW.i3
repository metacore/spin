(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Shell interface to change the web servers parameters.
 *)
INTERFACE WWW;
IMPORT ParseParams;

CONST CommandName = "www";
CONST CommandHelp = "-- [showconf|swapon device partitionnumber|swapoff|pageout|flush|debug|verbose|quiet|log filename|logoff|priority pri|srv port]";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END WWW.

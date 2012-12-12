(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 18-Jan-98  Tian Fung Lim (tian) at the University of Washington
 *	Front end for simple blur benchmark
 *
 *)
INTERFACE BlurCmd;
IMPORT ParseParams;

CONST CommandName = "blur";
CONST CommandHelp = "-- [n <iterations>] [live <max>] [dist <x> <y>] \n";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END BlurCmd.



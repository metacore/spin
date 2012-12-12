(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 11-Feb-95  Przemek Pardyak (pardy) at the University of Washington
 *	More accurate help line.
 *
 * 15-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created
 *
 *)

INTERFACE Disp;

IMPORT ParseParams;

CONST CommandName = "disp";
CONST CommandHelp = " opt|debug|set <n>|show|event (-all|<unit>.<event> [<n>])";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END Disp. 


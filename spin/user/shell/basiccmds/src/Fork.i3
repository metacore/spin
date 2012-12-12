(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Test thread.t garbage collection.
 *
 *)
INTERFACE Fork;

IMPORT ParseParams;

CONST CommandName = "fork";
CONST CommandHelp = "num -- fork num number of threads";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END Fork.




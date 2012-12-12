(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 25-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Test for enqueue/dequeue ops using ras regions.
 *)
INTERFACE RASTest;
IMPORT ParseParams;

CONST Brand = "RASTest";  (* Use this name to link with this extension *)

CONST CommandName = "rastest";
CONST CommandHelp = " [init test] -- tests ras based enqueue/dequeue ops ";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;

END RASTest.

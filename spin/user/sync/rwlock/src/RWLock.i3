(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 20-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Test for reader/writer locks.
 *)
INTERFACE RWLock;

IMPORT ParseParams;

CONST Brand = "RWLock";  (* Use this name to link with this extension *)

CONST CommandName = "rwl";
CONST CommandHelp = " [rlock runlock wlock wunlock] ";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;


END RWLock.


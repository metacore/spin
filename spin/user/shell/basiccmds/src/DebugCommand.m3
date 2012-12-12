(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)
MODULE DebugCommand;

IMPORT Debugger, ParseParams;

PROCEDURE Run (<*UNUSED*>          pp: ParseParams.T) : BOOLEAN =
  BEGIN
    Debugger.Enter();
    RETURN TRUE;
  END Run;


BEGIN
END DebugCommand.

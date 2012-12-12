(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 12-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Added Error function and Uninstall.
 *
 * 9-Jan-95 Przemek Pardyak (pardy) at the University of Washington
 *	Switched to a single dispatcher exception.
 *
 * 26-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)
GENERIC MODULE Command(Cmd);
IMPORT Commands;
IMPORT ParseParams;

VAR binding: Commands.T;

PROCEDURE Uninstall () =
  BEGIN
    Commands.Uninstall(binding);
  END Uninstall;

PROCEDURE Run (<*UNUSED*>c: Commands.T; pp: ParseParams.T): BOOLEAN =
  BEGIN
    RETURN Cmd.Run(pp);
  END Run;
  
PROCEDURE Install () =
  BEGIN
    binding := Commands.Install(Run, Cmd.CommandName, Cmd.CommandHelp);
  END Install;

BEGIN
  Install();
END Command.

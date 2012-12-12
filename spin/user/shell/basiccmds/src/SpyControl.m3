(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added reset.
 *
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Whisted. ParseParams
 *
 *)
MODULE SpyControl EXPORTS CoreCommands;
IMPORT Spy, IO, ParseParams, Commands;

CONST CommandHelp = " on|off|dump|reset";

PROCEDURE Run (<*UNUSED*>c: REFANY; pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();	(* the command *)
      IF pp.keywordPresent("dump") THEN
        Spy.Dump();
      ELSIF pp.keywordPresent("reset") THEN
        Spy.Reset();
      ELSE
        RAISE ParseParams.Error;
      END;
    EXCEPT
      ParseParams.Error =>
	IO.PutError("spy " & CommandHelp);
        RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;

BEGIN
  EVAL Commands.Install(Run, "spy", CommandHelp);
END SpyControl.

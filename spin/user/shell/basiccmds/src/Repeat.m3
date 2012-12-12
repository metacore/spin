(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

MODULE Repeat EXPORTS CoreCommands;
IMPORT Commands;
IMPORT Shell, ParseParams;

PROCEDURE Run (r: REFANY; pp: ParseParams.T): BOOLEAN =
  VAR 
    n: INTEGER;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      n := pp.getNextInt();
      FOR i := 1 TO n DO
	EVAL Shell.Run(pp.dup());
      END;
    EXCEPT
    | ParseParams.Error => 
      Commands.ParseError(r);
      RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;


BEGIN
  EVAL Commands.Install(Run, "repeat", "n command [args...]",
			"Execute COMMAND N times.");
END Repeat. 

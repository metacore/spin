(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

MODULE Time EXPORTS FsCommands;
IMPORT Spy;
IMPORT Clock;
IMPORT Commands;
IMPORT IO, Fmt, Shell, ParseParams;

PROCEDURE Run (r: REFANY; pp: ParseParams.T): BOOLEAN =
  VAR 
    spy: Spy.T;
    s, e: Clock.TimeVal;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      spy := Spy.Create(pp.peekNext());
      Clock.TimeOfDay(s);
      Spy.Enter(spy);
      EVAL Shell.Run(pp.dup());
      Spy.Exit(spy);
      Clock.TimeOfDay(e);
      Spy.DumpTime(spy);
      Spy.Delete(spy);
      IO.Put(pp.peekNext() &" : " & Fmt.Int(e.tv_sec - s.tv_sec) & " sec.\n");
    EXCEPT
    | ParseParams.Error => 
      Commands.ParseError(r);
    END;
    RETURN TRUE;
  END Run;


BEGIN
  EVAL Commands.Install(Run, "time", "command [args...]",
			"run COMMAND, and measure the elapsed time.");
END Time. 

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Whisted.
 *)
MODULE Join;

IMPORT JobControl, IO, Shell, ParseParams;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR cmd: TEXT;
  BEGIN
    pp.reset();
    TRY
      cmd := pp.getNext();
      IO.Put("Waiting on " & pp.peekNext() & "\n");
      EVAL Shell.Run(pp.dup());
      JobControl.WaitYoungsters();
    EXCEPT
      ParseParams.Error => IO.Put("join what?\n"); RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;


BEGIN
END Join.

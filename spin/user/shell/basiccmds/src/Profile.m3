(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Added command to set the number of arcs in profiling data array.
 *
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Added cmd and flush options for profile command.
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Extended to permit both PC sampling alone and full call graph
 *	 profiling. If your kernel was compiled with call graph support,
 *	 then you will get that. Otherwise you will only get PC sample
 *        counts.
 *
 * 17-Apr-96  Charles Garrett (garrett) at the University of Washington
 *	A PC sampling profiler. Type "profile on", then run some tests
 *	and type "profile off". The profile information will reside in
 *	memory until the next time you type "profile on". While the 
 *	information is in memory, you can attach with m3gdbttd and 
 *	display it.
 *
 *)
MODULE Profile EXPORTS CoreCommands;
IMPORT IO, ProfileSupport, ParseParams, Shell;
IMPORT Commands;

PROCEDURE Run (r: REFANY; pp: ParseParams.T): BOOLEAN =
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();

      (* Only permit one profiling command to be processed at a time. *)
      IF pp.testNext("on") THEN
        IF NOT ProfileSupport.On(FALSE) THEN
          IO.Put("Profiling is already on\n");
        END;
      ELSIF pp.testNext("off") THEN
        IF NOT ProfileSupport.Off() THEN
          IO.Put("Profiling was not on\n");
        END;
      ELSIF pp.testNext("flush") THEN
        ProfileSupport.Flush();
        IO.Put("Profile data wiped out\n");
      ELSIF pp.testNext("cmd") THEN
        VAR cmdLine: TEXT := "";
        BEGIN
          FOR i := pp.next TO LAST(pp.arg^) DO
            cmdLine := cmdLine & pp.arg[i];
            IF i < LAST(pp.arg^) THEN cmdLine := cmdLine & " "; END;
          END;

          IF NOT ProfileSupport.On(TRUE) THEN
            IO.Put("Profiling is already on\n");
          END;

          TRY
            IF NOT Shell.OneShellCommand(cmdLine) THEN
              IO.Put("Command failed\n");
            END;
          EXCEPT
          ELSE
          END;

          IF NOT ProfileSupport.Off() THEN
            IO.Put("Profiling was not on\n");
          END;
        END;
      ELSIF pp.testNext("dump") THEN
        EVAL ProfileSupport.Off();

        ProfileSupport.PrintSamples();
      ELSIF pp.testNext("arcs") THEN
        ProfileSupport.SetNumArcs(pp.getNextInt());
      ELSE
	Commands.ParseError(r);
      END;
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(r);
    END;
    RETURN TRUE;
  END Run;

BEGIN
  EVAL Commands.Install(Run, "profile", "on |off |cmd args...| dump| arcs",
			"Control the profiler. \n",
			"\"cmd\" runs a command with profiling.");
END Profile.


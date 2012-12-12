(*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 21-Oct-97  Przemek Pardyak (pardy) at the University of Washington
 *      Created.
 *)

MODULE AutoExec;

IMPORT IO, RTArgs, Text, Shell;

EXCEPTION
  Error;

PROCEDURE RunBootScript () RAISES { Error } =
  VAR
    argc  := RTArgs.ArgC();
    found : BOOLEAN := FALSE;
  BEGIN
    FOR i := 0 TO argc-1 DO
      IF Text.Equal(RTArgs.GetArg(i), "-noboot") THEN
        found := TRUE;
        EXIT;
      END;
    END;
    IF NOT found THEN
      IO.Put("AutoExec: executing the boot script\n");
      IF NOT Shell.OneShellCommand("script -b") THEN
        RAISE Error;
      END;
    END;
  END RunBootScript;

PROCEDURE RunAutoScripts () RAISES { Error } =
  VAR
    argc := RTArgs.ArgC();
    arg  : TEXT;
  BEGIN
    FOR i := 0 TO argc-1 DO
      arg := RTArgs.GetArg(i);
      WITH c = Text.GetChar(arg, 0) DO
        IF c = '+' THEN
          arg := Text.Sub(arg, 1);
          IO.Put("AutoExec: executing: " & arg & "\n");
          IF NOT Shell.OneShellCommand("script " & arg) THEN
            RAISE Error;
          END;
        END;
      END;
    END;
  END RunAutoScripts;

BEGIN
  IO.Put("AutoExec: loading scripts\n");
  TRY
    RunBootScript();
    RunAutoScripts();
  EXCEPT
  | Error =>
    IO.Put("AutoExec: shell command failed\n");
  END;
  IO.Put("AutoExec: done\n");
END AutoExec.

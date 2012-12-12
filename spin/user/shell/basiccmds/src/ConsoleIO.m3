(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 *)
MODULE ConsoleIO;

IMPORT BootIO, IO, ParseParams, InfoFile, TextWr, Wr;

VAR
  ConsoleText : TextWr.T;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
BEGIN
  pp.reset();
  TRY
    pp.skipNext();               (* command *)

    IF pp.testNext("terminal") THEN 
      BootIO.Redirect(NIL);
    ELSIF pp.testNext("file") THEN
      BootIO.Redirect(ConsoleText);
    ELSE
      Usage();
    END;
  EXCEPT ELSE END;
  RETURN TRUE;
END Run;

PROCEDURE Usage() =
  BEGIN
    IO.Put("console " & CommandHelp & "\n");
  END Usage;

PROCEDURE GatherOutput(wr:Wr.T) =
  BEGIN
    (* Send the data from one writer to another. *)
    Wr.PutText(wr, TextWr.ToText(ConsoleText));
  END GatherOutput;

BEGIN
  (* Create the /proc/console file to hold the output. We don't put
     anything into the file until someone uses the console command
     to redirect it. *)

  ConsoleText := TextWr.New();
  InfoFile.Create("/proc/console", GatherOutput);
END ConsoleIO. 

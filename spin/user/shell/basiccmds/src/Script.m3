(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	Relative script path is parsed from ~/spin/user/scripts
 *	
 * 05-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added some tracing code to debug an IO.GetLine error.
 *
 * 4-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Used IO.GetLine instead of IO.ReadLine to avoid echoing 
 *	the whole script.
 *
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Whisted. ParseParams.
 *
 *)
MODULE Script EXPORTS CoreCommands;
IMPORT Commands;
IMPORT Rd, RdClass, IO, Thread, Shell, ParseParams, Glob;
IMPORT Text, ThreadExtra;

CONST trace = FALSE;


PROCEDURE Home(): TEXT = 
  BEGIN
    TRY
      RETURN Glob.Lookup(Shell.Vars(), Shell.HOME);
    EXCEPT
      Glob.Error => IO.Put("Where is my home directory??\n");
      RETURN NIL;
    END;
  END Home; 

PROCEDURE User(): TEXT = 
  BEGIN
    TRY
      RETURN Glob.Lookup(Shell.Vars(), Shell.USER);
    EXCEPT
      Glob.Error => IO.Put("Who built this system??\n");
      RETURN NIL;
    END;
  END User;

PROCEDURE HomeScriptArea (t: TEXT): TEXT =
  BEGIN
    RETURN Home() & "/spin/user/scripts/" & t;
  END HomeScriptArea;

PROCEDURE Run (r: REFANY; pp: ParseParams.T): BOOLEAN =
  VAR
    filename: TEXT;
    rd      : Rd.T;
    oldRd   : Rd.T;
    verbose : BOOLEAN := FALSE;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();             (* "script" *)
      IF pp.testNext("-i") THEN
        filename := HomeScriptArea(User() & ".spininit");
      ELSIF pp.testNext("-b") THEN
	IO.Put("boot.rc is already loaded.\n");
	RETURN TRUE;
      ELSE
        filename := pp.getNext();
      END;
    EXCEPT
    | ParseParams.Error =>
      IF filename = NIL THEN
	Commands.ParseError(r);
	RETURN FALSE;
      END;
    END;

    verbose := Shell.Verbose();


    IF verbose OR trace THEN
      IO.Put("script: opening file " & filename & " for reading.\n");
    END;
    IF Text.GetChar(filename, 0) # '/' THEN
      TRY
        (* relative path *)
        filename := Glob.Lookup(Shell.Vars(), "home")
                      & "/spin/user/scripts/" & filename;
      EXCEPT
        Glob.Error => IO.Put("script: no home\n"); RETURN FALSE;
      END;
    END;

    (* IO.OpenRead is probably hijacked via the magic of events by the
       FileIO module *)
    rd := IO.OpenRead(filename);

    IF rd = NIL THEN
      IO.PutError("Could not open file " & filename & "\n");
      RETURN FALSE;
    END;
    TRY
      IF trace THEN
        IO.Put("script: opened file " & filename & " for reading.\n");
      END;

      (* Grab the reader *)
      oldRd := ThreadExtra.SetRdSelf(rd);

      Shell.CommandLoop(showPrompt := FALSE, echoChars := FALSE);
    FINALLY
      TRY
        rd.close();
        IF oldRd # NIL THEN
           EVAL ThreadExtra.SetRdSelf(oldRd);
        END;
      EXCEPT
        Rd.Failure, Thread.Alerted =>
          IO.PutError("Script rd close failed on " & filename & "\n");
      END;
    END;

    IF trace THEN IO.Put("script: returning.\n"); END;
    RETURN TRUE;
  END Run;

BEGIN
  EVAL Commands.Install(Run, "script", "[-i] | file",
			"Execute a script FILE. FILE is searched from the current directory, \n",
			"then from ~/spin/user/scripts.\n",
			"-i loads USER.spininit.");
END Script.

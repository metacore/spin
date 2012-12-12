(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Show functions.
 *
 * HISTORY
 * 4-oct-96  becker at the University of Washington
 *	Added /proc files
 *
 * 22-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup. Removed all of the html'ization support.  It was never 
 *      used and the WWW server places all shell output in <pre></pre>.
 *
 * 10-Mar-96 Przemek Pardyak (pardy) at the University of Washington
 *	Added "show types". 
 *
 * 10-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Took out add-symbol-file prefix, since domain sweep is now
 *	implemented in the debugger.
 *
 * 08-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	added "show fs"
 *	
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread.
 *
 * 10-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

MODULE Show;

IMPORT Fmt, IO, ParseParams,
       RTTypeSRC, ShowCmd, Textify, Thread, ThreadPrivate;
IMPORT InfoFile, Wr, ThreadExtra, Error;

(***************************************************************************)
(*									   *)
(* Thread support							   *)
(*									   *)

TYPE ThreadCl = ThreadPrivate.ThreadApplyClosure OBJECT
                  count : CARDINAL := 0;
                OVERRIDES
                  apply := ThreadApply;
                END;

PROCEDURE ThreadApply(cl: ThreadCl; t: Thread.T)  : BOOLEAN =
  BEGIN
   cl.count := cl.count + 1;
   IO.Put(Fmt.Int(cl.count) & "." );
   IO.Put(Textify.ThreadName(t, FALSE));
   RETURN TRUE;
  END ThreadApply;

(*******************************************************************)

PROCEDURE Usage()   =
  BEGIN
      IO.Put("show " & CommandHelp & "\n");
  END Usage;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();             (* show *)
      IF pp.keywordPresent("zap") THEN
        ShowCmd.Uninstall();
      ELSIF pp.keywordPresent("threads") THEN
        EVAL ThreadPrivate.Apply(NEW(ThreadCl));
      ELSIF pp.keywordPresent("types") THEN
        RTTypeSRC.ShowTypes(FALSE);
      ELSE
        RAISE ParseParams.Error;
      END;
    EXCEPT
      ParseParams.Error => Usage(); RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;

PROCEDURE Threads (wr: Wr.T) =
  VAR
    realWr := ThreadExtra.SetWrSelf(wr);
  BEGIN
    EVAL ThreadPrivate.Apply(NEW(ThreadCl));
    EVAL ThreadExtra.SetWrSelf(realWr);
  END Threads;

BEGIN
  TRY
     InfoFile.Create("/proc/threads",Threads);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("gc procfs files:" & e.message() & "\n");
  END;
END Show.

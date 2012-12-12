(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Regression tests are supposed to install themselves as commands by
 * using RegressionTest generic. Type "regress -all" from shell to
 * execute all regression tests and "regress <test-name> to execute a
 * particular test.  Type "regress -clean" to uninstall all regression
 * tests. 
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 25-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Clean up the state of the thread running regression tests
 *	by installing a procedure as the last handler on the Shell.Run
 *	event.
 *
 * 18-Jan-96 Przemek Pardyak (pardy) at the University of Washington
 *	Filtered output.
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created.  Control over regression tests.
 *
 *)

MODULE Regress;
IMPORT ParseParams, Wr, NullWr, Thread, ThreadExtra, Fmt, Shell;
IMPORT Auth, RegressInterface, NameServer, IO, Dispatcher;

<* FATAL Thread.Alerted, Wr.Failure *>

(* a /dev/null sort of thingy *)
VAR
  NullWriter : NullWr.T := NEW(NullWr.T).init();
  iterate: INTEGER := 0;
  RealWriter : Wr.T;

(* executed before all installed regression tests *)
PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  BEGIN
    IF iterate = 0 THEN
      RealWriter := ThreadExtra.GetWrSelf();
    END;

    pp.reset();

    IF NUMBER(pp.arg^) < 2 THEN
      Wr.PutText(RealWriter, 
                 "regress [-verbose|-quiet] -all|-clean|<test-name>\n");
      RETURN FALSE;
    END;

    IF pp.keywordPresent("-10") THEN
      Wr.PutText(RealWriter, ">>> Iterating regression tests\n");
      RETURN Iterate(pp);
    END;

    ErrorWriter := RealWriter;

    IF pp.keywordPresent("-quiet") THEN
      Writer := NullWriter;
    ELSE
      Writer := RealWriter;
    END;

    IF NOT pp.keywordPresent("-verbose") THEN
      EVAL ThreadExtra.SetWrSelf(NullWriter);
    END;

    IF pp.keywordPresent("-all") THEN
      Wr.PutText(RealWriter, ">>> Running all regression tests\n");
    ELSIF pp.keywordPresent("-clean") THEN
      Wr.PutText(RealWriter, ">>> Removing all regression tests\n");
    END;

    RETURN TRUE;
  END Run;

PROCEDURE Clean (<* UNUSED *>pp: ParseParams.T): BOOLEAN =
  BEGIN
    IF RealWriter # NIL THEN
      EVAL ThreadExtra.SetWrSelf(RealWriter);
    END;
    RETURN TRUE;
  END Clean;

FUNCTIONAL PROCEDURE CleanGuard (pp: ParseParams.T): BOOLEAN =
  BEGIN
    RETURN pp.ftestNext("regress");
  END CleanGuard;

PROCEDURE Iterate (pp: ParseParams.T): BOOLEAN =
  VAR new_pp: ParseParams.T;
  BEGIN
    INC(iterate);
    FOR i := 1 TO 10 DO
      Wr.PutText(RealWriter, "Regress: --- " & Fmt.Int(i) & "---\n");
      TRY
        pp.reset();
        EVAL pp.keywordPresent("-10");
        new_pp := pp.dup();
        EVAL Shell.Run(new_pp);
      EXCEPT
      ELSE
        Wr.PutText(RealWriter, "Regress: shell command failed\n");
        DEC(iterate);
        RETURN FALSE;
      END;
    END;
    DEC(iterate);
    RETURN TRUE;
  END Iterate;

BEGIN
  TRY
    EVAL Dispatcher.InstallHandler(Shell.Run, CleanGuard, Clean
                                   (*
                                   , options := Dispatcher.Options{
                                                    Dispatcher.Opt.Last} *)
    ); 
    EVAL RegressInterface.Export(NEW (Auth.AuthAlways));
  EXCEPT
  | Dispatcher.Error =>
    IO.PutError("Dispatcher error while initializing regression tests\n");
  | NameServer.Error =>
    IO.PutError("Name server error while initializing regression tests\n")
  END;
END Regress.

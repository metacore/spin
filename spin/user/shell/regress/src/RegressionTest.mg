(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 12-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Added install. Speak to the dispatcher directly.
 *
 * 18-Jan-96 Przemek Pardyak (pardy) at the University of Washington
 *	Filtered output.
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created.  A generic to simplify writing regression tests.
 *
 *)

GENERIC MODULE RegressionTest(Test);

IMPORT Wr, ParseParams, Dispatcher, Shell, Regress, Thread, IO, Fmt;

<* FATAL Thread.Alerted, Wr.Failure *>

VAR
  runBinding : Dispatcher.Binding;
  helpBinding : Dispatcher.Binding;

(* guard for regression tests as commands *)
FUNCTIONAL PROCEDURE GuardTest(pp: ParseParams.T): BOOLEAN =
  BEGIN
    RETURN pp.ftestNext("regress") AND
           (NOT pp.fkeywordPresent("-10")) AND
           (pp.fkeywordPresent(Test.TestName) OR
            pp.fkeywordPresent("-clean") OR
            pp.fkeywordPresent("-all"));
  END GuardTest;


(* generic help procedure for installing regression tests as commands *)
PROCEDURE HelpTest (<*UNUSED*>command: TEXT; <*UNUSED*>detailed: BOOLEAN) =
  BEGIN
    IO.Put("regress " & Test.TestName & ": " & Test.TestHelp & "\n");
  END HelpTest;

(* generic run procedure for installing regression tests as commands *)
PROCEDURE RunTest(pp: ParseParams.T): BOOLEAN =
  VAR
    result     : BOOLEAN;
  BEGIN
    pp.reset();

    IF pp.keywordPresent("-clean") THEN
      Wr.PutText(Regress.Writer, ">>> Uninstalling regression test " & 
        Test.TestName & "\n");
      Uninstall();
      RETURN TRUE;
    END;

    Wr.PutText(Regress.Writer, ">>> running the regression test: " & 
      Test.TestName & "\n");

    result := TRUE;
    FOR i := 1 TO Test.nIterations DO
      IF Test.nIterations # 1 THEN
        IO.Put(">>> " & Test.TestName & " (iteration " & Fmt.Int(i) & ")\n");
      END;
      result := result AND ExecuteTest(i);
    END;

    IF NOT result THEN
      Error(NIL, "failed");
    ELSE
      Wr.PutText(Regress.Writer, ">>> regression test: " & Test.TestName & 
        " => OK\n");
    END;

    RETURN result;
  END RunTest;

PROCEDURE ExecuteTest (i: INTEGER): BOOLEAN = 
  VAR
    result     : BOOLEAN;
    testResult : BOOLEAN;
  BEGIN
    testResult := TRUE;

    (* execute startup code *)
    TRY
      IO.Put(">>>    executing start-up code\n");
      result := Test.Start(i);
      IF NOT result THEN
        Error("startup", "returned FALSE");
        testResult := FALSE;
      END;
    EXCEPT
    ELSE
      Error("startup", "exception raised");
      testResult := FALSE;
      result := FALSE;
    END;
    IF result THEN
      FOR i := FIRST(Test.TestList) TO LAST(Test.TestList) DO
        IO.Put(">>>    executing test case: " & Test.TestList[i].name & "\n");

        (* execute a test *)
        TRY
          result := Test.TestList[i].test();
        EXCEPT
        ELSE
          Error(Test.TestList[i].name, "exception raised");
          testResult := FALSE;
          result := FALSE;
        END;

        IF result THEN
          IO.Put(">>>    " & Test.TestList[i].name & " => OK\n");
        ELSE
          Error(Test.TestList[i].name, "returned FALSE");
          testResult := FALSE;
        END;
      END;

      (* execute cleanup code *)
      TRY
        IO.Put(">>>    executing clean-up code\n");
        result := Test.End();
        IF NOT result THEN
          Error("cleanup", "returned FALSE");
        END;
      EXCEPT
      ELSE
        Error("cleanup", "exception raised");
        testResult := FALSE;
        result := FALSE;
      END;
    END;

    RETURN testResult;
  END ExecuteTest; 

PROCEDURE Uninstall () =
  BEGIN
    TRY
      IF helpBinding # NIL THEN Dispatcher.Uninstall(helpBinding); END;
      IF runBinding # NIL THEN Dispatcher.Uninstall(runBinding); END;
    EXCEPT
    | Dispatcher.Error => Error("uninstall", "could not uninstall handlers");
    END;
  END Uninstall;

(* create and install the regression test descriptor *)
PROCEDURE Install () =
  BEGIN
    IO.Put(">>> Installing regression test " & Test.TestName & "\n");

    TRY
      helpBinding := Dispatcher.InstallHandler(Shell.Help, NIL, HelpTest);
      runBinding := Dispatcher.InstallHandler(Shell.Run, GuardTest, RunTest);
    EXCEPT
    ELSE
      IO.Put(
        ">>> Regression Installation of " & Test.TestName & "failed\n");
      RETURN;
    END;
  END Install;

(* print an error message *)
PROCEDURE Error(case: TEXT; msg: TEXT) =
  BEGIN
    IF case = NIL THEN
      Wr.PutText(Regress.ErrorWriter, ">>> ERROR: " & Test.TestName & 
        " - " & msg & "\n");
    ELSE
      Wr.PutText(Regress.ErrorWriter, ">>> ERROR: " & Test.TestName & 
        " " & case & " - " & msg & "\n");
    END;
  END Error;

BEGIN
  Install();
END RegressionTest.

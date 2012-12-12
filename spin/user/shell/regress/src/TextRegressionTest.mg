(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created.  
 *
 *)

GENERIC MODULE TextRegressionTest(Test);

IMPORT Wr, TextWr, ParseParams, Dispatcher, Shell, Regress, Thread;
IMPORT ThreadExtra, IO, Fmt, Text;

<* FATAL Thread.Alerted, Wr.Failure *>

VAR
  runSpindle : Dispatcher.Spindle;
  helpSpindle : Dispatcher.Spindle;

(* guard for regression tests as commands *)
PROCEDURE GuardTest(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    RETURN pp.testNext("regress") AND
           (NOT pp.keywordPresent("-10")) AND
           (pp.keywordPresent(Test.TestName) OR
            pp.keywordPresent("-clean") OR
            pp.keywordPresent("-all"));
  END GuardTest;

(* generic help procedure for installing regression tests as commands *)
PROCEDURE HelpTest() =
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
    wr         : Wr.T;
    textWr     : TextWr.T := NEW(TextWr.T);
    output     : TEXT;
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
          EVAL textWr.init();
          wr := ThreadExtra.SetWrSelf(textWr);
          result := Test.TestList[i].test();
          EVAL ThreadExtra.SetWrSelf(wr);
          output := TextWr.ToText(textWr);
          IF NOT Text.Equal(output, Test.TestText[i]) THEN
            Error(Test.TestList[i].name, "incorrect output");
            testResult := FALSE;
            IO.Put(">>>    is:\n");
            IO.Put(output);
            IO.Put(">>>    should be:\n");
            IO.Put(Test.TestText[i]);
            IO.Put(">>>    end\n");
          ELSE
            IO.Put(">>>    " & Test.TestList[i].name & " => output OK\n");
          END;
          Wr.Close(textWr);
        EXCEPT
        ELSE
          Error(Test.TestList[i].name, "exception raised");
          testResult := FALSE;
          result := FALSE;
        END;
        IF result THEN
          IO.Put(">>>    " & Test.TestList[i].name & " => result OK\n");
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
      IF helpSpindle # NIL THEN Dispatcher.Uninstall(helpSpindle); END;
      IF runSpindle # NIL THEN Dispatcher.Uninstall(runSpindle); END;
    EXCEPT
    | Dispatcher.Error => Error("uninstall", "could not uninstall handlers");
    END;
  END Uninstall;

(* create and install the regression test descriptor *)
PROCEDURE Install () =
  BEGIN
    IO.Put(">>> Installing regression test " & Test.TestName & "\n");

    TRY
      helpSpindle := Dispatcher.Install(Shell.Help, NIL, HelpTest,
                                        Dispatcher.DefaultOptions, NIL);
      runSpindle := Dispatcher.Install(Shell.Run, GuardTest, RunTest,
                                       Dispatcher.DefaultOptions, NIL);
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
END TextRegressionTest.

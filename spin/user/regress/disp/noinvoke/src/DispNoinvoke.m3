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
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

UNSAFE MODULE DispNoinvoke;
IMPORT IO, Sirpa, Dispatcher, SpinException, DispatcherPrivate;

<* FATAL Dispatcher.Error *>

FUNCTIONAL PROCEDURE GuardTRUE (): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END GuardTRUE;

FUNCTIONAL PROCEDURE GuardFALSE (): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END GuardFALSE;

VAR
  Bindings : ARRAY [1..12] OF Dispatcher.Binding;

PROCEDURE Test(proc: INTEGER; ok: BOOLEAN): BOOLEAN =
  VAR
    exception: BOOLEAN;
    otherException: BOOLEAN;
  BEGIN
    exception := FALSE;
    otherException := FALSE;
    TRY
      CASE proc OF
      | 1 => Sirpa.Proc1();
      | 2 => EVAL Sirpa.Proc2();
      ELSE 
        IO.Put("ERROR: wrong procedure number\n");
        RETURN FALSE;
      END;
    EXCEPT
    | SpinException.Exception(ec) =>
      IF ec.code = SpinException.ExceptionCode.NoHandlerInvoked THEN
        exception := TRUE;
        IO.Put("NoHandlerInvoked");
      ELSE
        otherException := TRUE;
        IO.Put("other SpinException"); 
      END
    END;
    IF NOT (exception OR otherException) THEN
      IO.Put("no exception");
    END;
    IO.Put(" raised => ");
    IF NOT otherException AND (ok # exception) THEN
      IO.Put("OK\n");
      RETURN TRUE;
    ELSE
      IO.Put("ERROR\n");
      RETURN FALSE;
    END
  END Test; 

PROCEDURE Test1 (): BOOLEAN =
  BEGIN
    IO.Put("procedure with no result, default: ");
    RETURN Test(1, TRUE);
  END Test1;

PROCEDURE Test2 (): BOOLEAN =
  BEGIN
    IO.Put("procedure with no result, no handler installed: ");
    Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(Sirpa.Proc1));
    RETURN Test(1, TRUE);
  END Test2;
    
PROCEDURE Test3 (): BOOLEAN =
  BEGIN
    IO.Put("procedure with no result, no handler invoked: ");
    Bindings[1] := Dispatcher.InstallHandler(Sirpa.Proc1, 
                                             GuardFALSE, 
                                             Sirpa.Proc1);
    Bindings[2] := Dispatcher.InstallHandler(Sirpa.Proc1, 
                                             GuardFALSE, 
                                             Sirpa.Proc1);
    Bindings[3] := Dispatcher.InstallHandler(Sirpa.Proc1, 
                                             GuardFALSE, 
                                             Sirpa.Proc1);
    RETURN Test(1, TRUE);
  END Test3;
    
PROCEDURE Test4 (): BOOLEAN =
  BEGIN
    IO.Put("procedure with no result, many handlers invoked: ");
    Bindings[4] := Dispatcher.InstallHandler(Sirpa.Proc1, 
                                             GuardTRUE, 
                                             Sirpa.Proc1);
    Bindings[5] := Dispatcher.InstallHandler(Sirpa.Proc1, 
                                             GuardTRUE, 
                                             Sirpa.Proc1);
    Bindings[6] := Dispatcher.InstallHandler(Sirpa.Proc1, 
                                             GuardTRUE, 
                                             Sirpa.Proc1);
    RETURN Test(1, TRUE);
  END Test4;
    
PROCEDURE Test5 (): BOOLEAN =
  BEGIN
    IO.Put("procedure with a result, default: ");
    RETURN Test(2, TRUE);
  END Test5;
    
PROCEDURE Test6 (): BOOLEAN =
  BEGIN
    IO.Put("procedure with a result, no handler installed: ");
    Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(Sirpa.Proc2));
    RETURN Test(2, FALSE);
  END Test6;
    
PROCEDURE Test7 (): BOOLEAN =
  BEGIN
    IO.Put("procedure with a result, no handler invoked: ");
    Bindings[7] := Dispatcher.InstallHandler(Sirpa.Proc2, 
                                             GuardFALSE, 
                                             Sirpa.Proc2);
    Bindings[8] := Dispatcher.InstallHandler(Sirpa.Proc2,
                                             GuardFALSE, 
                                             Sirpa.Proc2);
    Bindings[9] := Dispatcher.InstallHandler(Sirpa.Proc2,
                                             GuardFALSE, 
                                             Sirpa.Proc2);
    RETURN Test(2, FALSE);
  END Test7;
    
PROCEDURE Test8 (): BOOLEAN =
  BEGIN
    IO.Put("procedure with a result, many handlers invoked: ");
    Bindings[10] := Dispatcher.InstallHandler(Sirpa.Proc2, 
                                              GuardTRUE, 
                                              Sirpa.Proc2);
    Bindings[11] := Dispatcher.InstallHandler(Sirpa.Proc2, 
                                              GuardTRUE, 
                                              Sirpa.Proc2);
    Bindings[12] := Dispatcher.InstallHandler(Sirpa.Proc2, 
                                              GuardTRUE, 
                                              Sirpa.Proc2);
    RETURN Test(2, TRUE);
  END Test8;

PROCEDURE Start(i: INTEGER): BOOLEAN =
  BEGIN
    DispatcherPrivate.SetOptLevel(Sirpa.Proc1, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Proc2, i-1);
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    FOR i := 1 TO 12 DO
      Dispatcher.Uninstall(Bindings[i]);
    END;
    Dispatcher.Install(Dispatcher.GetOriginalHandler(Sirpa.Proc1));
    Dispatcher.Install(Dispatcher.GetOriginalHandler(Sirpa.Proc2));
    RETURN TRUE;
  END End;

BEGIN
END DispNoinvoke.


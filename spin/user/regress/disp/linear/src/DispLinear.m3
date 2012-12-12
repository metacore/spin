(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
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

UNSAFE MODULE DispLinear;
IMPORT IO, Sirpa, Dispatcher;

<* FATAL Dispatcher.Error *>

(*
 * test argument passing
 *)

PROCEDURE DoLinear (): BOOLEAN =
  VAR
    binding : Dispatcher.Binding;
    ok: BOOLEAN;
    result: BOOLEAN := TRUE;
    closure := NEW(REF INTEGER);
  BEGIN
    closure^ := 12345;

    ok := TRUE;
    TRY
      Sirpa.Sirpa0();
      binding := InstallLinear (Sirpa.Sirpa0, Sirpa.Linear0, 0);
      Sirpa.Sirpa0();
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa0, Sirpa.Linear0Cl, 0, closure);
      Sirpa.Sirpa0();
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa0();
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa1(111);
      binding := InstallLinear (Sirpa.Sirpa1, Sirpa.Linear1, 1);
      Sirpa.Sirpa1(111);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa1, Sirpa.Linear1Cl, 1, closure);
      Sirpa.Sirpa1(111);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa1(111);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa2(111,222);
      binding := InstallLinear (Sirpa.Sirpa2, Sirpa.Linear2, 2);
      Sirpa.Sirpa2(111,222);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa2, Sirpa.Linear2Cl, 2, closure);
      Sirpa.Sirpa2(111,222);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa2(111,222);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa3(111,222,333);
      binding := InstallLinear (Sirpa.Sirpa3, Sirpa.Linear3, 3);
      Sirpa.Sirpa3(111,222,333);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa3, Sirpa.Linear3Cl, 3, closure);
      Sirpa.Sirpa3(111,222,333);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa3(111,222,333);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa4(111,222,333,444);
      binding := InstallLinear (Sirpa.Sirpa4, Sirpa.Linear4, 4);
      Sirpa.Sirpa4(111,222,333,444);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa4, Sirpa.Linear4Cl, 4, closure);
      Sirpa.Sirpa4(111,222,333,444);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa4(111,222,333,444);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa5(111,222,333,444,555);
      binding := InstallLinear (Sirpa.Sirpa5, Sirpa.Linear5, 5);
      Sirpa.Sirpa5(111,222,333,444,555);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa5, Sirpa.Linear5Cl, 5, closure);
      Sirpa.Sirpa5(111,222,333,444,555);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa5(111,222,333,444,555);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa6(111,222,333,444,555,666);
      binding := InstallLinear (Sirpa.Sirpa6, Sirpa.Linear6, 6);
      Sirpa.Sirpa6(111,222,333,444,555,666);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa6, Sirpa.Linear6Cl, 6, closure);
      Sirpa.Sirpa6(111,222,333,444,555,666);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa6(111,222,333,444,555,666);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;
    
    ok := TRUE;
    TRY
      Sirpa.Sirpa7(111,222,333,444,555,666,777);
      binding := InstallLinear (Sirpa.Sirpa7, Sirpa.Linear7, 7);
      Sirpa.Sirpa7(111,222,333,444,555,666,777);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa7, Sirpa.Linear7Cl, 7, closure);
      Sirpa.Sirpa7(111,222,333,444,555,666,777);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa7(111,222,333,444,555,666,777);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa8(111,222,333,444,555,666,777,888);
      binding := InstallLinear (Sirpa.Sirpa8, Sirpa.Linear8, 8);
      Sirpa.Sirpa8(111,222,333,444,555,666,777,888);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa8, Sirpa.Linear8Cl, 8, closure);
      Sirpa.Sirpa8(111,222,333,444,555,666,777,888);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa8(111,222,333,444,555,666,777,888);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa9(111,222,333,444,555,666,777,888,999);
      binding := InstallLinear (Sirpa.Sirpa9, Sirpa.Linear9, 9);
      Sirpa.Sirpa9(111,222,333,444,555,666,777,888,999);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa9, Sirpa.Linear9Cl, 9, closure);
      Sirpa.Sirpa9(111,222,333,444,555,666,777,888,999);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa9(111,222,333,444,555,666,777,888,999);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa10(111,222,333,444,555,666,777,888,999,101010);
      binding := InstallLinear (Sirpa.Sirpa10, Sirpa.Linear10, 10);
      Sirpa.Sirpa10(111,222,333,444,555,666,777,888,999,101010);
      Dispatcher.Uninstall (binding);
      binding := InstallLinear (Sirpa.Sirpa10, Sirpa.Linear10Cl, 10, closure);
      Sirpa.Sirpa10(111,222,333,444,555,666,777,888,999,101010);
      Dispatcher.Uninstall (binding);
      Sirpa.Sirpa10(111,222,333,444,555,666,777,888,999,101010);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    RETURN result;
  END DoLinear;

PROCEDURE InstallLinear(event, handler: PROCANY;
                        nArgs: INTEGER;
                        closure: REFANY := NIL): Dispatcher.Binding =
  BEGIN
    RETURN Dispatcher.InstallHandler(event, 
                                     NIL, 
                                     Dispatcher.CreateLinear(handler,
                                                             nArgs,
                                                             closure));
  END InstallLinear;

PROCEDURE Start(<* UNUSED *>i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END DispLinear.

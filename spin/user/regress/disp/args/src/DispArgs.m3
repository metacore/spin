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

UNSAFE MODULE DispArgs;
IMPORT IO, Sirpa, Dispatcher, DispatcherPrivate;

<* FATAL Dispatcher.Error *>

(*
 * test argument passing
 *)

PROCEDURE DoArgs (): BOOLEAN =
  VAR
    binding : Dispatcher.Binding;
    ok: BOOLEAN;
    result: BOOLEAN := TRUE;
  BEGIN
    ok := TRUE;
    TRY
      Sirpa.Sirpa0();
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa0, NIL, Sirpa.Sirpa0);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa1, NIL, Sirpa.Sirpa1);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa2, NIL, Sirpa.Sirpa2);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa3, NIL, Sirpa.Sirpa3);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa4, NIL, Sirpa.Sirpa4);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa5, NIL, Sirpa.Sirpa5);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa6, NIL, Sirpa.Sirpa6);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa7, NIL, Sirpa.Sirpa7);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa8, NIL, Sirpa.Sirpa8);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa9, NIL, Sirpa.Sirpa9);
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
      binding := Dispatcher.InstallHandler (Sirpa.Sirpa10, NIL, Sirpa.Sirpa10);
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
  END DoArgs;

PROCEDURE Start(i: INTEGER): BOOLEAN =
  BEGIN
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa0, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa1, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa2, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa3, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa4, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa5, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa6, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa7, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa8, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa9, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa10, i-1);
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END DispArgs.

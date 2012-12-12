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

UNSAFE MODULE DispException;
IMPORT IO, Sirpa, Dispatcher;

<* FATAL Dispatcher.Error *>

VAR
  Binding: Dispatcher.Binding;

PROCEDURE Test (): BOOLEAN =
  VAR
    ok: BOOLEAN := FALSE;
    exception: BOOLEAN := FALSE;
  BEGIN
    TRY
      Sirpa.Proc();
    EXCEPT
    | Sirpa.Exception => 
      IO.Put("right exception caught => OK\n");
      ok := TRUE;
      exception := TRUE;
    ELSE
      IO.Put("some other exception caught => ERROR\n");
      exception := TRUE;
    END;
    IF NOT exception THEN
      IO.Put("no exception caught => ERROR\n");
    END;
    RETURN ok;
  END Test;

PROCEDURE ExceptionTest (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
    IO.Put("no extra handlers: ");
    IF NOT Test() THEN
      result := FALSE;
    END;

    IO.Put("handler installed: ");
    Binding := Dispatcher.InstallHandler(Sirpa.Proc, NIL, Sirpa.Proc);
    IF NOT Test() THEN
      result := FALSE;
    END;

    RETURN result;
  END ExceptionTest;

PROCEDURE Start(<* UNUSED *> i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    Dispatcher.Uninstall(Binding);
    RETURN TRUE;
  END End;

BEGIN
END DispException.

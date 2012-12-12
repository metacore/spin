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
 * 15-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

MODULE DispClosure;
IMPORT IO, Sirpa, Dispatcher, Fmt, DispatcherPrivate;

<* FATAL Dispatcher.Error *>

TYPE
  Closure = REF INTEGER;

VAR
  test: INTEGER;
  cl: Closure;

PROCEDURE Sirpa0 (cl: Closure)
                  RAISES { Sirpa.Exception } =
  BEGIN
    IO.Put ("----> Closure Sirpa0 <----\n" );
    IF test = 1 THEN
      IF cl # NIL THEN 
        IO.Put("closure not NIL\n");
        RAISE Sirpa.Exception; 
      END;
    ELSIF test = 2 THEN
      IF cl = NIL THEN 
        IO.Put("closure is NIL\n");
        RAISE Sirpa.Exception; 
      END;
      IF cl^ # 2 THEN 
        IO.Put("closure does not contain 2\n");
        RAISE Sirpa.Exception; 
      END;
    ELSE
      RAISE Sirpa.Exception; 
    END;
  END Sirpa0;

PROCEDURE Sirpa2 (cl: Closure; 
                  a1 : INTEGER; a2 : INTEGER)
                  RAISES { Sirpa.Exception } =
  BEGIN
    IO.Put ("----> Closure Sirpa2 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF test = 1 THEN
      IF cl # NIL THEN 
        IO.Put("closure not NIL\n");
        RAISE Sirpa.Exception; 
      END;
    ELSIF test = 2 THEN
      IF cl = NIL THEN 
        IO.Put("closure is NIL\n");
        RAISE Sirpa.Exception; 
      END;
      IF cl^ # 2 THEN 
        IO.Put("closure does not contain 2\n");
        RAISE Sirpa.Exception; 
      END;
    ELSE
      RAISE Sirpa.Exception; 
    END;
  END Sirpa2;

PROCEDURE Sirpa5 (cl: Closure;
                  a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER)
                  RAISES { Sirpa.Exception } =
  BEGIN
    IO.Put ("----> Closure Sirpa5 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & "\n");
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF a3 # 333 THEN RAISE Sirpa.Exception; END;
    IF a4 # 444 THEN RAISE Sirpa.Exception; END;
    IF a5 # 555 THEN RAISE Sirpa.Exception; END;
    IF test = 1 THEN
      IF cl # NIL THEN 
        IO.Put("closure not NIL\n");
        RAISE Sirpa.Exception; 
      END;
    ELSIF test = 2 THEN
      IF cl = NIL THEN 
        IO.Put("closure is NIL\n");
        RAISE Sirpa.Exception; 
      END;
      IF cl^ # 2 THEN 
        IO.Put("closure does not contain 2\n");
        RAISE Sirpa.Exception; 
      END;
    ELSE
      RAISE Sirpa.Exception; 
    END;
  END Sirpa5;

PROCEDURE Sirpa10 (cl: Closure;
                   a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER)
                   RAISES { Sirpa.Exception } =
  BEGIN
    IO.Put ("----> Closure Sirpa10 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & " ");
    IO.Put (Fmt.Int (a6) & " ");
    IO.Put (Fmt.Int (a7) & " ");
    IO.Put (Fmt.Int (a8) & " ");
    IO.Put (Fmt.Int (a9) & " ");
    IO.Put (Fmt.Int (a10) & "\n");
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF a3 # 333 THEN RAISE Sirpa.Exception; END;
    IF a4 # 444 THEN RAISE Sirpa.Exception; END;
    IF a5 # 555 THEN RAISE Sirpa.Exception; END;
    IF a6 # 666 THEN RAISE Sirpa.Exception; END;
    IF a7 # 777 THEN RAISE Sirpa.Exception; END;
    IF a8 # 888 THEN RAISE Sirpa.Exception; END;
    IF a9 # 999 THEN RAISE Sirpa.Exception; END;
    IF a10 # 101010 THEN RAISE Sirpa.Exception; END;
    IF test = 1 THEN
      IF cl # NIL THEN 
        IO.Put("closure not NIL\n");
        RAISE Sirpa.Exception; 
      END;
    ELSIF test = 2 THEN
      IF cl = NIL THEN 
        IO.Put("closure is NIL\n");
        RAISE Sirpa.Exception; 
      END;
      IF cl^ # 2 THEN 
        IO.Put("closure does not contain 2\n");
        RAISE Sirpa.Exception; 
      END;
    ELSE
      RAISE Sirpa.Exception; 
    END;
  END Sirpa10;

FUNCTIONAL PROCEDURE Guard0 (cl: Closure): BOOLEAN
                  RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> Closure Guard0 <----\n" );
*)
    IF test = 1 THEN
      IF cl # NIL THEN 
(*
        IO.Put("closure not NIL\n");
*)
        RAISE Sirpa.Exception; 
      END;
    ELSIF test = 2 THEN
      IF cl = NIL THEN 
(*
        IO.Put("closure is NIL\n");
*)
        RAISE Sirpa.Exception; 
      END;
      IF cl^ # 2 THEN 
(*
        IO.Put("closure does not contain 2\n");
*)
        RAISE Sirpa.Exception; 
      END;
    ELSE
      RAISE Sirpa.Exception; 
    END;
    RETURN TRUE;
  END Guard0;

FUNCTIONAL PROCEDURE Guard2 (cl: Closure; 
                  a1 : INTEGER; a2 : INTEGER): BOOLEAN
                  RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> Closure Guard2 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
*)
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF test = 1 THEN
      IF cl # NIL THEN 
(*
        IO.Put("closure not NIL\n");
*)
        RAISE Sirpa.Exception; 
      END;
    ELSIF test = 2 THEN
      IF cl = NIL THEN 
(*
        IO.Put("closure is NIL\n");
*)
        RAISE Sirpa.Exception; 
      END;
      IF cl^ # 2 THEN 
(*
        IO.Put("closure does not contain 2\n");
*)
        RAISE Sirpa.Exception; 
      END;
    ELSE
      RAISE Sirpa.Exception; 
    END;
    RETURN TRUE;
  END Guard2;

FUNCTIONAL PROCEDURE Guard5 (cl: Closure;
                  a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER): BOOLEAN
                  RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> Closure Guard5 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & "\n");
*)
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF a3 # 333 THEN RAISE Sirpa.Exception; END;
    IF a4 # 444 THEN RAISE Sirpa.Exception; END;
    IF a5 # 555 THEN RAISE Sirpa.Exception; END;
    IF test = 1 THEN
      IF cl # NIL THEN 
(*
        IO.Put("closure not NIL\n");
*)
        RAISE Sirpa.Exception; 
      END;
    ELSIF test = 2 THEN
      IF cl = NIL THEN 
(*
        IO.Put("closure is NIL\n");
*)
        RAISE Sirpa.Exception; 
      END;
      IF cl^ # 2 THEN 
(*
        IO.Put("closure does not contain 2\n");
*)
        RAISE Sirpa.Exception; 
      END;
    ELSE
      RAISE Sirpa.Exception; 
    END;
    RETURN TRUE;
  END Guard5;

FUNCTIONAL PROCEDURE Guard10 (cl: Closure;
                   a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER): BOOLEAN
                   RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> Closure Guard10 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & " ");
    IO.Put (Fmt.Int (a6) & " ");
    IO.Put (Fmt.Int (a7) & " ");
    IO.Put (Fmt.Int (a8) & " ");
    IO.Put (Fmt.Int (a9) & " ");
    IO.Put (Fmt.Int (a10) & "\n");
*)
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF a3 # 333 THEN RAISE Sirpa.Exception; END;
    IF a4 # 444 THEN RAISE Sirpa.Exception; END;
    IF a5 # 555 THEN RAISE Sirpa.Exception; END;
    IF a6 # 666 THEN RAISE Sirpa.Exception; END;
    IF a7 # 777 THEN RAISE Sirpa.Exception; END;
    IF a8 # 888 THEN RAISE Sirpa.Exception; END;
    IF a9 # 999 THEN RAISE Sirpa.Exception; END;
    IF a10 # 101010 THEN RAISE Sirpa.Exception; END;
    IF test = 1 THEN
      IF cl # NIL THEN 
(*
        IO.Put("closure not NIL\n");
*)
        RAISE Sirpa.Exception; 
      END;
    ELSIF test = 2 THEN
      IF cl = NIL THEN 
(*
        IO.Put("closure is NIL\n");
*)
        RAISE Sirpa.Exception; 
      END;
      IF cl^ # 2 THEN 
(*
        IO.Put("closure does not contain 2\n");
*)
        RAISE Sirpa.Exception; 
      END;
    ELSE
      RAISE Sirpa.Exception; 
    END;
    RETURN TRUE;
  END Guard10;

PROCEDURE DoClosure (): BOOLEAN =
  VAR
    binding1, binding2 : Dispatcher.Binding;
    ok: BOOLEAN;
    result: BOOLEAN := TRUE;
  BEGIN
    ok := TRUE;

    TRY
      Sirpa.Sirpa0();
      binding1 := Dispatcher.InstallHandler (Sirpa.Sirpa0, NIL, Sirpa.Sirpa0);
      Sirpa.Sirpa0();
      binding2 := Dispatcher.InstallHandler (Sirpa.Sirpa0,Guard0,Sirpa0,cl,cl);
      Sirpa.Sirpa0();
      Dispatcher.Uninstall (binding1);
      Dispatcher.Uninstall (binding2);
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
      Sirpa.Sirpa2(111,222);
      binding1 := Dispatcher.InstallHandler (Sirpa.Sirpa2, NIL, Sirpa.Sirpa2);
      Sirpa.Sirpa2(111,222);
      binding2 := Dispatcher.InstallHandler (Sirpa.Sirpa2,Guard2,Sirpa2,cl,cl);
      Sirpa.Sirpa2(111,222);
      Dispatcher.Uninstall (binding1);
      Dispatcher.Uninstall (binding2);
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
      Sirpa.Sirpa5(111,222,333,444,555);
      binding1 := Dispatcher.InstallHandler (Sirpa.Sirpa5, NIL, Sirpa.Sirpa5);
      Sirpa.Sirpa5(111,222,333,444,555);
      binding2 := Dispatcher.InstallHandler (Sirpa.Sirpa5,Guard5,Sirpa5,cl,cl);
      Sirpa.Sirpa5(111,222,333,444,555);
      Dispatcher.Uninstall (binding1);
      Dispatcher.Uninstall (binding2);
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
      Sirpa.Sirpa10(111,222,333,444,555,666,777,888,999,101010);
      binding1 := Dispatcher.InstallHandler (Sirpa.Sirpa10,NIL,Sirpa.Sirpa10);
      Sirpa.Sirpa10(111,222,333,444,555,666,777,888,999,101010);
      binding2 := Dispatcher.InstallHandler(Sirpa.Sirpa10,Guard10,
                                            Sirpa10,cl,cl);
      Sirpa.Sirpa10(111,222,333,444,555,666,777,888,999,101010);
      Dispatcher.Uninstall (binding1);
      Dispatcher.Uninstall (binding2);
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
  END DoClosure;

PROCEDURE Test1 (): BOOLEAN =
  BEGIN
    test := 1;
    cl := NIL;
    RETURN DoClosure();
  END Test1;

PROCEDURE Test2 (): BOOLEAN =
  BEGIN
    test := 2;
    cl := NEW(Closure);
    cl^ := 2;
    RETURN DoClosure();
  END Test2;

PROCEDURE Start(i: INTEGER): BOOLEAN =
  BEGIN
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa0, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa2, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa5, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa10, i-1);
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END DispClosure. 

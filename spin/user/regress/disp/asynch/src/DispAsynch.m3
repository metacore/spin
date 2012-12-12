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

UNSAFE MODULE DispAsynch;
IMPORT IO, Sirpa, Dispatcher, Thread;

<* FATAL Dispatcher.Error *>

VAR
  alias0 : PROCEDURE();
  alias1 : PROCEDURE(a1: INTEGER);
  alias2 : PROCEDURE(a1: INTEGER; a2: INTEGER);
  alias3 : PROCEDURE(a1: INTEGER; a2: INTEGER; a3: INTEGER);
  alias4 : PROCEDURE(a1: INTEGER; a2: INTEGER; a3: INTEGER; a4: INTEGER);
  alias5 : PROCEDURE(a1: INTEGER; a2: INTEGER; a3: INTEGER; a4: INTEGER;
                     a5: INTEGER);
  alias6 : PROCEDURE(a1: INTEGER; a2: INTEGER; a3: INTEGER; a4: INTEGER;
                     a5: INTEGER; a6: INTEGER);
  alias7 : PROCEDURE(a1: INTEGER; a2: INTEGER; a3: INTEGER; a4: INTEGER;
                     a5: INTEGER; a6: INTEGER; a7: INTEGER);

  aliasl : PROCEDURE();

(*
 * test argument passing
 *)

PROCEDURE DoArgs (): BOOLEAN =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    Sirpa.Result := TRUE;
    LOCK Sirpa.MuArgs DO
      Sirpa.Cnt := 0;
    END;

(*
    alias0 := Dispatcher.GetAsynchAlias(Sirpa.Sirpa0);
    Sirpa.Sirpa0();
    alias0();
    Synch(2);
    binding := Dispatcher.InstallHandler (Sirpa.Sirpa0, NIL, Sirpa.Sirpa0);
    Sirpa.Sirpa0();
    alias0();
    Synch(4);
    Dispatcher.Uninstall (binding);
    Sirpa.Sirpa0();
    alias0();
    Synch(2);
*)
    alias1 := Dispatcher.GetAsynchAlias(Sirpa.Sirpa1);
    Sirpa.Sirpa1(111);
    alias1(111);
    Synch(2);
    binding := Dispatcher.InstallHandler (Sirpa.Sirpa1, NIL, Sirpa.Sirpa1);
    Sirpa.Sirpa1(111);
    alias1(111);
    Synch(4);
    Dispatcher.Uninstall (binding);
    Sirpa.Sirpa1(111);
    alias1(111);
    Synch(2);

    alias2 := Dispatcher.GetAsynchAlias(Sirpa.Sirpa2);
    Sirpa.Sirpa2(111,222);
    alias2(111,222);
    Synch(2);
    binding := Dispatcher.InstallHandler (Sirpa.Sirpa2, NIL, Sirpa.Sirpa2);
    Sirpa.Sirpa2(111,222);
    alias2(111,222);
    Synch(4);
    Dispatcher.Uninstall (binding);
    Sirpa.Sirpa2(111,222);
    alias2(111,222);
    Synch(2);

    alias3 := Dispatcher.GetAsynchAlias(Sirpa.Sirpa3);
    Sirpa.Sirpa3(111,222,333);
    alias3(111,222,333);
    Synch(2);
    binding := Dispatcher.InstallHandler (Sirpa.Sirpa3, NIL, Sirpa.Sirpa3);
    Sirpa.Sirpa3(111,222,333);
    alias3(111,222,333);
    Synch(4);
    Dispatcher.Uninstall (binding);
    Sirpa.Sirpa3(111,222,333);
    alias3(111,222,333);
    Synch(2);

    alias4 := Dispatcher.GetAsynchAlias(Sirpa.Sirpa4);
    Sirpa.Sirpa4(111,222,333,444);
    alias4(111,222,333,444);
    Synch(2);
    binding := Dispatcher.InstallHandler (Sirpa.Sirpa4, NIL, Sirpa.Sirpa4);
    Sirpa.Sirpa4(111,222,333,444);
    alias4(111,222,333,444);
    Synch(4);
    Dispatcher.Uninstall (binding);
    Sirpa.Sirpa4(111,222,333,444);
    alias4(111,222,333,444);
    Synch(2);

    alias5 := Dispatcher.GetAsynchAlias(Sirpa.Sirpa5);
    Sirpa.Sirpa5(111,222,333,444,555);
    alias5(111,222,333,444,555);
    Synch(2);
    binding := Dispatcher.InstallHandler (Sirpa.Sirpa5, NIL, Sirpa.Sirpa5);
    Sirpa.Sirpa5(111,222,333,444,555);
    alias5(111,222,333,444,555);
    Synch(4);
    Dispatcher.Uninstall (binding);
    Sirpa.Sirpa5(111,222,333,444,555);
    alias5(111,222,333,444,555);
    Synch(2);

    alias6 := Dispatcher.GetAsynchAlias(Sirpa.Sirpa6);
    Sirpa.Sirpa6(111,222,333,444,555,666);
    alias6(111,222,333,444,555,666);
    Synch(2);
    binding := Dispatcher.InstallHandler (Sirpa.Sirpa6, NIL, Sirpa.Sirpa6);
    Sirpa.Sirpa6(111,222,333,444,555,666);
    alias6(111,222,333,444,555,666);
    Synch(4);
    Dispatcher.Uninstall (binding);
    Sirpa.Sirpa6(111,222,333,444,555,666);
    alias6(111,222,333,444,555,666);
    Synch(2);

    alias7 := Dispatcher.GetAsynchAlias(Sirpa.Sirpa7);
    Sirpa.Sirpa7(111,222,333,444,555,666,777);
    alias7(111,222,333,444,555,666,777);
    Synch(2);
    binding := Dispatcher.InstallHandler (Sirpa.Sirpa7, NIL, Sirpa.Sirpa7);
    Sirpa.Sirpa7(111,222,333,444,555,666,777);
    alias7(111,222,333,444,555,666,777);
    Synch(4);
    Dispatcher.Uninstall (binding);
    Sirpa.Sirpa7(111,222,333,444,555,666,777);
    alias7(111,222,333,444,555,666,777);
    Synch(2);

    RETURN Sirpa.Result;
  END DoArgs;

PROCEDURE Synch (i: INTEGER) =
  BEGIN
    LOCK Sirpa.MuArgs DO
      WHILE Sirpa.Cnt # i DO
        Thread.Wait(Sirpa.MuArgs, Sirpa.CondArgs);
      END;
      Sirpa.Cnt := 0;
    END;
  END Synch;

PROCEDURE DoAsynch (): BOOLEAN =
  BEGIN
    IO.Put("Expect four lines of output:\n");
    Sirpa.AsynchOK := FALSE;
    aliasl := Dispatcher.GetAsynchAlias(Sirpa.Asynch);
    IO.Put("This line should be printed first\n");
    aliasl();
    IO.Put("This line should be printed second\n");
    LOCK Sirpa.MuAsynch DO
      Sirpa.AsynchOK := TRUE;
      Thread.Signal(Sirpa.CondAsynch1);
      Thread.Wait(Sirpa.MuAsynch, Sirpa.CondAsynch2);
    END;    
    IO.Put("This line should be printed fourth\n");
    RETURN TRUE;
  END DoAsynch;

PROCEDURE Start(<* UNUSED *>i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END DispAsynch.

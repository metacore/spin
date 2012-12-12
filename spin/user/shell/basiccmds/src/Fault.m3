(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Shell interface.
 * Need to revise to export all services as extensions.
 *
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 22-Feb-96 Brian Bershad (bershad) at the University of Washington
 *	Added cfault test.
 *
 * 17-Feb-96 Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of warnings.
 *
 * 10-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted. Added nilref, narrow and raise arguments.
 *
 *)

(* This module is UNSAFE because it imports M3toC for fault purposes *)

UNSAFE MODULE Fault;
IMPORT SpinException, ParseParams, IO, M3toC;
IMPORT Thread, Dispatcher, ThreadException;
EXCEPTION UnhandledException;

PROCEDURE Foo() RAISES {UnhandledException} =
  BEGIN
    RAISE UnhandledException;
  END Foo;

PROCEDURE Finished() =
  BEGIN
    IO.Put("Finished\n");
  END Finished;

PROCEDURE CFault() =
  BEGIN
    EVAL M3toC.StoT(NIL);	(* should fault *)
  END CFault;


PROCEDURE CatchException(rf: REF Dispatcher.Binding; <*UNUSED*>t: Thread.T) =
  BEGIN
    IO.Put("Catching my own exception!\n");
    Dispatcher.Uninstall(rf^);
    RETURN;
  END CatchException;

FUNCTIONAL PROCEDURE MyThread(cl: Thread.T; t: Thread.T)  : BOOLEAN =
  BEGIN
(*
    IO.Put("Fault.MyThread checking thread\n");
*)
    RETURN cl = t;
  END MyThread;



PROCEDURE LateRaise (catchSelf: BOOLEAN) =
  VAR ei: SpinException.ExceptionInfo;
      rf: REF Dispatcher.Binding := NEW(REF Dispatcher.Binding);
  BEGIN
    IO.Put("INSIDE LATE RAISE\n");

    IF catchSelf THEN
      TRY
        rf^ := Dispatcher.InstallHandler(
               event := ThreadException.RaiseException, 
	       guard := MyThread,
               guardClosure := Thread.Self(),
               handler := CatchException,
               handlerClosure := rf,
               key := Thread.Self(),
               options := Dispatcher.Options{
                            Dispatcher.Opt.First, Dispatcher.Opt.Cancel});
               
      EXCEPT
        Dispatcher.Error=>
          IO.Put("Fault could not override exception\n");
      END;
    END;
    ei.code := SpinException.ExceptionCode.FunctionDidNotReturnValue;
    ei.msg := "the buck stops here";
    IO.Put("Late raising...\n");
    RAISE SpinException.Exception(ei);
    IO.Put("Done late raising!\n");<*NOWARN*>
  END LateRaise;


PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    t : REF INTEGER;
    b : REF BOOLEAN;
    ra: REFANY;
    lateRaise : BOOLEAN := FALSE;
    catchSelf : BOOLEAN := FALSE;
  BEGIN
    pp.reset();
    TRY
      IF pp.keywordPresent("nilref") THEN
        t^ := 100;
      ELSIF pp.keywordPresent("narrow") THEN
        b := NEW(REF BOOLEAN);
        ra := b;
        t := NARROW(ra, REF INTEGER);
      ELSIF pp.keywordPresent("raise") THEN
        Foo();  (* Potentially unhandled exception. Ok. Need runtime fault.*)
      ELSIF pp.keywordPresent("cfault") THEN
        CFault();
      ELSIF pp.keywordPresent("spinex1") THEN
        lateRaise := TRUE;
        catchSelf := FALSE;
      ELSIF pp.keywordPresent("spinex2") THEN
        lateRaise := TRUE;
        catchSelf := TRUE;
      END;
    EXCEPT
    | SpinException.Exception (ex) =>
      IO.Put("Exception is " & SpinException.ExceptionNames[ex.code] & " "
             & ex.msg & "\n");
    | ParseParams.Error =>
        IO.Put(CommandName & CommandHelp & "\n");
        RETURN FALSE;
    | UnhandledException =>
      IO.Put("UncchandledException raised\n");
      RETURN FALSE;  
    END;
    IF lateRaise THEN
      LateRaise(catchSelf);
    END;

    Finished();
    pp.reset();
    RETURN TRUE;
  END Run;

BEGIN
END Fault.

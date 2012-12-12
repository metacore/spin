(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Removed old signal handler scheme.  New one is
 *      based on BSDtty.sigHandler().
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Fixed bug where a user-space thread was allocated with NEW instead 
 *      of Create.  Also ensure that we don't dereference NIL while 
 *      iterating over the environment variables.
 *
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Removed JobControl reliance. 
 *
 * 12-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed TtySignal handler so that it won't signal the shell unless
 *	^C is typed 2 types rapidly.
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 * 07-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added -w option.
 * 08-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	environ passing(environ is shell var list).
 * 09-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made safe.
 * 26-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Argument passing.
 *  
 * 23-Jan-96  David Dion (ddion) at the University of Washington
 *	Added support for exec -noreturn in Run (skip -noreturn arg).
 *	Added FPUState to construction of UserSpaceThread.State
 *
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	ParseParams.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Start up a user level process.
 * 
 *)

MODULE ExecCmd;

IMPORT ThreadExtra;
IMPORT Nice, IO;
IMPORT Fmt;
IMPORT ParseParams;
IMPORT Error;
IMPORT VMError;
IMPORT ExecInterface, Auth, NameServer;
IMPORT Glob;
IMPORT Exec;
IMPORT Space, UserSpaceThread;
IMPORT Dispatcher, Thread;
IMPORT Shell;

PROCEDURE DoExec (arg: ThreadExtra.ArgT): ThreadExtra.ResultT =
  VAR
    pp := NARROW(arg, ParseParams.T); 
    filename: TEXT;
    argv, environ: REF ARRAY OF TEXT;
    varSpace := Shell.Vars();
    shellVars := Glob.GetVariableList(varSpace);
    space := Space.Create();
    thread := UserSpaceThread.Create(space);
    state: UserSpaceThread.State;
    info : Exec.Info;
    value: TEXT; 
 BEGIN
    IF pp.next > LAST(pp.arg^) THEN
      IO.Put("doexec : weird... I can't get exec file name. I'm over.\n");
      RETURN NIL;
    END;
    
    filename := pp.arg[pp.next];
    IO.Put("exec: starting " & filename & "\n");

    argv := NEW(REF ARRAY OF TEXT, LAST(pp.arg^) - pp.next + 1);
    FOR i := 0 TO LAST(argv^) DO
      argv[i] := pp.arg[pp.next + i];
    END;


    (* "envVars" contains the list of var names. Convert it into
     "var=value" format required by UNIX *)
    environ := NEW(REF ARRAY OF TEXT, NUMBER(shellVars^));    

    FOR i := 0 TO LAST(shellVars^) DO
      environ[i] := shellVars[i] & "=";
      value := Glob.GetVariable(varSpace, shellVars[i]);
      IF value # NIL THEN
        environ[i] := environ[i] & value;
      END;
    END;

    TRY
      Exec.LoadFile(filename, argv^, environ^, space, state, info);
      (*
       * Let it rip.
       *)
      UserSpaceThread.SetState(thread, state);
      UserSpaceThread.Resume(thread);
    EXCEPT
    | Error.E(e) =>
      IO.Put("exec : " & e.message() & ".\n");
    | VMError.E(e) =>
      IO.Put("exec : vm error " & Fmt.Int(e) & ".\n");
    END;
    RETURN NIL;
  END DoExec;

PROCEDURE Help () =
  BEGIN
    IO.Put("exec filename\n");
  END Help;

PROCEDURE Run (pp: ParseParams.T) : BOOLEAN =
  VAR 
    noReturn := FALSE;
    wait := FALSE;
  BEGIN
    TRY
      pp.skipNext(); (* skip "exec" itself *)

      IF pp.keywordPresent("-noreturn") THEN 
        noReturn := TRUE;
      END;
      
      IF pp.keywordPresent("-w") THEN 
	wait := TRUE;
      END;

      IF pp.next > LAST(pp.arg^) THEN
	(* no filename specified *)
	IF wait THEN 
	  Wait();
	  RETURN TRUE;
	ELSE
	  IO.Put("exec : you need to specify pathname.\n");
	  RETURN FALSE;
	END;
      ELSE
        IF noReturn THEN 
	  IO.Put("Blocking Exec thread ...\n");
          EVAL DoExec(pp); 
        ELSE
		EVAL ThreadExtra.PFork(DoExec, pp, Nice.Priority());
	END;
	
	IF wait THEN Wait(); END;
	
	RETURN TRUE;
      END;
    EXCEPT 
    | ParseParams.Error =>
      Help();
      RETURN FALSE;
    END;

  END Run;

VAR
  mu := NEW(MUTEX);
  cond := NEW(Thread.Condition);
  
PROCEDURE Wait () =
  BEGIN
    LOCK mu DO
      Thread.Wait(mu, cond);
    END;
  END Wait;
      

PROCEDURE Signal () =
  BEGIN
    LOCK mu DO
      Thread.Signal(cond);
    END;
  END Signal;


BEGIN
  TRY 
    EVAL ExecInterface.Export(NEW(Auth.AuthAlways));
  EXCEPT
  | NameServer.Error =>
    IO.Put("ExecCmd : some error in authorizer installation");
  | Dispatcher.Error =>
    IO.Put("ExecCmd : some error in dispatcher installation");
  END;
END ExecCmd.

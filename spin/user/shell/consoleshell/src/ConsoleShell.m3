(*
 * Copyright 1995,1996,1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed to new security management
 *
 * 03-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Export this domain
 *
 * 15-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	remove import of UnsafeRd (not exported by SpinPublic),
 *        replace with Rd, and add revelation of Rd.T
 *      remove import of CharDevice (unused)
 *
 * 31-May-97  David Becker at the University of Washington
 *      New signal handler scheme with Signal() handling tty sigs
 *
 * 19-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *	Took our printing of a message in CreateNewShell().  We cannot use 
 *	Wr there because this procedure is called by an interrupt and Wr
 *	will deadlock if called recursively which happens if crtl-a is
 *	pressed repeatedly.
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Replaced Identity with SecurityContext
 *
 * 22-Nov-96  becker at the University of Washington
 *	Change to TrackStrand from Spy
 *
 * 10-Jun-96  Brian Bershad (bershad) at the University of Washington
 *	Whisted.
 *
 *)


MODULE ConsoleShell;

IMPORT Shell, ThreadExtra, Thread, ThreadPrivate, Mutex, TrackStrand;
IMPORT IO, SecurityManager;
IMPORT Device, CharDevRd, Rd;
IMPORT NameServer, Error, Glob, Word, Fmt;
IMPORT BSDtty;
IMPORT ConsoleShellInterface;

REVEAL
  Rd.T <: Thread.Mutex;

VAR
  rd := NEW(CharDevRd.T);
  currentshell: Thread.T;

(*
 * Always get a new shell
 *)
PROCEDURE DoCommandLoop(<*UNUSED*>ra: REFANY): REFANY = 
  BEGIN
      EVAL ThreadExtra.SetRdSelf(rd);
      IO.Put("Console Shell\n");
      (* BIG HACK BELOW TO NOT ECHO CONSOLE CHARACTERS SINCE WE KNOW
        WE ARE ON A CONSOLE TTY WHICH DOES ECHO CHARACTERS ALREADY.
        GACK!
       *)

      Shell.CommandLoop(showPrompt := TRUE, echoChars := FALSE);
      currentshell:=NIL;
      IO.Put("Console Shell exit\n");
      RETURN NIL;
  END DoCommandLoop;

PROCEDURE DefaultSignalHandler (sig: Word.T) =
  BEGIN
    IO.Put("ConsoleShell.Signal " &Fmt.Int(sig) &"\n");
    IF sig = 2 OR sig = 3 THEN
      IF currentshell#NIL THEN
        ThreadPrivate.Kill(currentshell,NIL);
	EVAL Mutex.TryLock(rd); (* get lock if not already held *)
        Thread.Release(rd); (* so we can make sure to unlock it *)
        IO.Put("Killed current shell.  Forking new shell\n");
      END;
      CreateNewShell();
    END;
  END DefaultSignalHandler;

PROCEDURE CreateNewShell () =
  BEGIN
    currentshell := ThreadExtra.PFork(DoCommandLoop, NIL);
    TrackStrand.SetName(ThreadExtra.GetTracker(currentshell),"ConsoleShell");
  END CreateNewShell;
  
BEGIN
  TRY
    VAR
      shellVars: Glob.T;
      console: BSDtty.T;
    BEGIN
      console := Device.Lookup("console");
      console.open();
      console.sigHandler(DefaultSignalHandler);
      rd := rd.init(console);
      (* Set the console variable's shell variables to inherit the parents. *)
      shellVars := Glob.New(Shell.SysVars());
      EVAL SecurityManager.SetCurrentProperty(Shell.SHELLVARS, shellVars);
      EVAL ConsoleShellInterface.Export(NIL);
      EVAL Shell.OneShellCommand("script boot.rc");
      CreateNewShell();
    END;
  EXCEPT 
  | NameServer.Error =>
    IO.Put("ConsoleShell: cannot register console device name\n");
  | Error.E(e) =>
    IO.Put("ConsoleShell: " & e.message() & "\n");
  END;
END ConsoleShell.

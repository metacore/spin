(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Made RecreateCommandLine pretty.
 *
 * 13-Jun-97  David Becker at the University of Washington
 *      DoExec and Run now handle Errno.
 *
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel, Clock and CPU interfaces
 *
 * 16-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Set CWD at startup.
 * 17-Sep-96  becker at the University of Washington
 *	Uninstall takes out the sphinx command so you can reload sphinx
 *	
 * 20-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Use new Shell Vars interface.
 *
 * 14-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *)
MODULE SphinxCmd EXPORTS Types;
(* SphinxCmd actually doesn't export anything. *)

IMPORT ParseParams;
IMPORT MachineMem;
IMPORT SphinxPrivate;
IMPORT UserSpaceThread;
IMPORT Exec;
IMPORT Proc, ProcRep;
IMPORT Sphinx;
IMPORT IO, Fmt, Wr;
IMPORT Text;
IMPORT Glob;
IMPORT Shell;
IMPORT Error;
IMPORT Errno;
IMPORT VMError;
IMPORT ExecCmd;
IMPORT SyscallTrace;
IMPORT OpenFile;
IMPORT Fcntl;
IMPORT StreamFile;
IMPORT FileSystem;
IMPORT InfoFile;
IMPORT AddressSpace;
IMPORT NameServer;
IMPORT Clock;
IMPORT ProfileSupport;
IMPORT Commands;
IMPORT NSName;
IMPORT Word;
IMPORT SphinxUtils;
TYPE
  Flag = {Traced, Profiling};
  Flags = SET OF Flag;

VAR
  binding: Commands.T;
  
PROCEDURE PsInternal(proc: Proc.T; wr: Wr.T) =
  VAR
    state: TEXT;
    saltime: Clock.TimeVal;
    uptime: INTEGER;
    stat: AddressSpace.Stat;
  BEGIN
    IO.Put(Fmt.Pad(Fmt.Int(proc.pid), 6), wr);
    IO.Put(" ", wr);
      
    IF proc.parent # NIL THEN
      IO.Put(Fmt.Pad(Fmt.Int(proc.parent.pid), 6), wr);
      IO.Put(" ", wr);
    ELSE
      IO.Put(Fmt.Pad("-", 6), wr);
      IO.Put(" ", wr);
    END;
      
    IO.Put(Fmt.Pad(Fmt.Int(proc.grp.gid), 6), wr);
    IO.Put(" ", wr);

    proc.stat(stat);
    IO.Put(Fmt.Pad(Fmt.Int(stat.virtualSize), 6), wr);
    IO.Put(" ", wr);
    IO.Put(Fmt.Pad(Fmt.Int(stat.residentSize), 6), wr);
    IO.Put(" ", wr);

    Clock.TimeOfDay(saltime);
    uptime := saltime.tv_sec - proc.startTime;
    IO.Put(Fmt.Pad(Fmt.Int(uptime DIV 60), 2, '0'), wr);
    IO.Put(":", wr);
    IO.Put(Fmt.Pad(Fmt.Int(uptime MOD 60), 2, '0'), wr);
    IO.Put(" ", wr);
    
    CASE proc.state OF
    | Proc.State.Active =>
      state := "Running";
    | Proc.State.Stopped =>
      state := "Stopped";
    | Proc.State.Zombie =>
      state := "Zombie";
    END;
    IO.Put(Fmt.Pad(state, 6), wr);
    IO.Put(" " & proc.print() & "\n", wr);
  END PsInternal;
  
PROCEDURE Ps () =
  VAR
    itr := Proc.Iterate();
    proc: Proc.T;
  BEGIN
    IO.Put("   pid   ppid    gid vsize rsize state uptime path\n");
    WHILE itr.next(proc) DO
      PsInternal(proc, NIL);
    END;
  END Ps;
  
PROCEDURE Filter (wr: Wr.T) =
  VAR
    itr := Proc.Iterate();
    proc: Proc.T;
  BEGIN
    WHILE itr.next(proc) DO
      PsInternal(proc, wr);
    END;
  END Filter;

(* Return single command line string. *)
PROCEDURE RecreateCommandLine (pp: ParseParams.T): TEXT =
  VAR r := "";
  BEGIN
    FOR i := 0 TO LAST(pp.arg^) DO
      IF NOT pp.parsed[i] AND pp.arg[i] # NIL THEN
	r := r & pp.arg[i] & " ";
      END;
    END;
    RETURN r;
  END RecreateCommandLine;

(* Get the currert SPINshell working directory. *)
PROCEDURE GetCwd (): NameServer.T = 
  VAR 
    cwd: TEXT;
  BEGIN
    cwd := Glob.GetVariable(Shell.Vars(), "cwd");
    IF cwd = NIL THEN
      cwd := "/";
      Glob.SetVariable(Shell.Vars(), "cwd", cwd);
    END;
    TRY
      RETURN FileSystem.Lookup(NIL, cwd);
    EXCEPT
    | NameServer.Error(ec) =>
      IO.Put("getcwd: " & cwd & " : error " & Fmt.Int(ORD(ec)) & ".\n");
    END;
  END GetCwd;
  
PROCEDURE DoExec (pp: ParseParams.T; flags: Flags) =
  VAR
    arg: TEXT;
    shellVars := Glob.GetVariableList(Shell.Vars());
    argc: CARDINAL;
    tmpargv, argv, environ: REF ARRAY OF TEXT;
    path: TEXT;
    t: TEXT;
    newThread: UserSpaceThread.T;
    newProc : Proc.T;
    state: UserSpaceThread.State;
    info : Exec.Info;
    fd: INTEGER;
    stdin, stdout, stderr: OpenFile.T := NIL;
    
  PROCEDURE Open (path: TEXT; mode: Word.T): OpenFile.T =
    VAR
      tmp: NameServer.Name;
      of: OpenFile.T;
    BEGIN
      tmp := NSName.FromText(path);
      of := SphinxPrivate.OpenInternal(newProc, tmp, mode, 0);
      IF ISTYPE(of.h, StreamFile.T) THEN
	StreamFile.SetFG(of.h, newProc.grp.gid);
      END;
      RETURN of;
    END Open;
    
  BEGIN
    TRY
      WITH c = RecreateCommandLine(pp) DO
	newProc := Proc.Create(c, NIL);
      END;
      path := pp.getNext();
      environ := NEW(REF ARRAY OF TEXT, NUMBER(shellVars^));
      tmpargv := NEW(REF ARRAY OF TEXT, LAST(pp.arg^) - pp.next + 2);
      tmpargv[0] := path;
      argc := 1;


      WHILE pp.next < NUMBER(pp.arg^) DO
	arg := pp.getNext();
	IF Text.GetChar(arg, 0) = '>' THEN
	  (* output redirection *)
	  IF Text.Length(arg) = 1 THEN
	    (* > foo *)
	    TRY
	      stdout := Open(pp.getNext(),
			     Word.Or(Fcntl.O_WRONLY, Fcntl.O_CREAT));
	    EXCEPT
	    | ParseParams.Error =>
	      IO.PutError("filename missing after >");
	      END;
	    ELSIF Text.GetChar(arg, 1) = '>' THEN
	      TRY
		IF Text.Length(arg) = 2 THEN
		  (* >> foo *)
		  stdout := Open(pp.getNext(),
				 Word.Or(Word.Or(Fcntl.O_WRONLY,
					       Fcntl.O_APPEND),
					 Fcntl.O_CREAT));
		ELSE
		  (* >>foo *)
		  stdout := Open(Text.Sub(arg, 2),
				 Word.Or(Word.Or(Fcntl.O_WRONLY,
						 Fcntl.O_APPEND),
					 Fcntl.O_CREAT));
		END;
	      EXCEPT
	      | ParseParams.Error =>
		IO.PutError("filename missing after >>");
	      END;
	    ELSE
	      IO.PutError("SphinxExec: " & arg &
			  ": unrecognized redirection.\n");
	    END;
	  ELSE
	    tmpargv[argc] := arg;
	    INC(argc);
	END;
      END;

      argv := NEW(REF ARRAY OF TEXT, argc);
      argv^ := SUBARRAY(tmpargv^, 0, argc);
      
      FOR i := 0 TO LAST(shellVars^) DO
        t := Glob.GetVariable(Shell.Vars(), shellVars[i]);
        IF t = NIL THEN t := ""; END;
        environ[i] := shellVars[i] & "=" & t;
      END;
      

      newThread := UserSpaceThread.Create(newProc);
    
      Exec.LoadFile(path, argv^, environ^, newProc, state, info);
      UserSpaceThread.SetState(newThread, state);
    EXCEPT
    | ParseParams.Error =>
      IO.Put("doexec : path name not specified.\n");
      RETURN;
    | Error.E(e) =>
      IO.Put("doexec : " & e.message() & ".\n");
      RETURN;
    | VMError.E(e) =>
      IO.Put("doexec : " & Fmt.Int(e) & ".\n");
      RETURN;
    END;
    

    VAR saltime: Clock.TimeVal;
    BEGIN
      Clock.TimeOfDay(saltime);
      newProc.startTime := saltime.tv_sec;
    END;
    
    newProc.thread := newThread;
    newProc.break := MachineMem.RoundToPage(info.break);
    newProc.stackTop := info.stackTop;
    Proc.InstallStandardHandlers(newProc);
    (* XXX move somewhere *)

    (* Open stdin/out/err files. *)
    LOCK newProc.mu DO 
      TRY
	newProc.cwd := GetCwd();
	IF stdin = NIL THEN
	  stdin := Open("/dev/console", Fcntl.O_RDONLY);
	END;
	fd := Proc.AllocateFD(newProc, stdin);
	<*ASSERT fd = 0*>

	IF stdout = NIL THEN
	  stdout := Open("/dev/console", Fcntl.O_WRONLY);
	END;
	fd := Proc.AllocateFD(newProc, stdout);
	<*ASSERT fd = 1*>

	IF stderr = NIL THEN
	  stderr := Open("/dev/console", Fcntl.O_WRONLY);
	END;
	fd := Proc.AllocateFD(newProc, stderr);
	<*ASSERT fd = 2*>
      EXCEPT
      | Errno.E(e) =>
	IO.Put("stdin/out/err open: " & Errno.Fmt(e) & ".\n");
      | NameServer.Error(e) =>
	IO.Put("stdin/out/err open: " & NameServer.Fmt(e) & ".\n");
      END;
    END;
    
    IF Flag.Traced IN flags THEN
      Proc.AddBinding(newProc, SyscallTrace.Install(newProc));
    END;
    IF Flag.Profiling IN flags THEN
      EVAL ProfileSupport.On();
    END;
    UserSpaceThread.Resume(newThread);
    ExecCmd.Wait(); (* wait for the child process to exit *)
    IF Flag.Profiling IN flags THEN
      EVAL ProfileSupport.Off();
    END;
  END DoExec;
  
PROCEDURE Run (r: REFANY; pp : ParseParams.T) : BOOLEAN =
  BEGIN
    TRY 
      pp.reset(); 
      pp.skipNext(); (* skip "sphinx" itself *)
      IF pp.testNext("uninstall") THEN
	Commands.Uninstall(binding);
	binding := NIL;
      ELSIF pp.testNext("ps") THEN
	Ps();
      ELSIF pp.testNext("kill") THEN
	VAR pid, signo : INTEGER;
	BEGIN
	  pid := pp.getNextInt();
	  signo := 9;
	  IF pp.next <= LAST(pp.arg^) THEN
	    signo := pp.getNextInt();
	  END;
	  EVAL Sphinx.Kill(pid, signo);
	END;
      ELSIF pp.testNext("exec") THEN
	DoExec(pp, Flags{});
      ELSIF pp.testNext("texec") THEN
	DoExec(pp, Flags{Flag.Traced});
      ELSIF pp.testNext("pexec") THEN
	DoExec(pp, Flags{Flag.Profiling});
      ELSIF pp.testNext("trace") THEN
	EVAL SyscallTrace.Install();
      ELSIF pp.testNext("untrace") THEN
	SyscallTrace.Uninstall();
      ELSIF pp.testNext("clear") THEN
	SyscallTrace.Uninstall();
      ELSIF pp.testNext("initprof") THEN
	EVAL ProfileSupport.Off();
	SphinxUtils.profCount := 0;
      ELSE
	Commands.ParseError(r);
      END;
    EXCEPT
    | Errno.E(e) =>
      IO.PutError("sphinx: " & Errno.Fmt(e) & ".\n");
    | ParseParams.Error =>
      Commands.ParseError(r);
    END;
    RETURN TRUE;
  END Run;

BEGIN
  TRY
    InfoFile.Create("/proc/sphinx", Filter);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("/proc/sphinx:" & e.message() & "\n");
  END;
  binding := Commands.Install(Run, "sphinx",
		   "sphinx COMMAND...\n"
		   & "  install | uninstall |\n"
		   & "  exec path ... | texec path ... | pexec path ...\n"
		   & "  ps | kill PID [SIG]| trace | untrace");

END SphinxCmd.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 *
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Replaced SecurityContext with SecurityManager
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Replaced Identity with SecurityContext
 *
 * 05-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added history and alias.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Change to TrackStrand from Spy
 *	Set home shellvar more helpfully.
 *
 * 5-oct-96  becker at the University of Washington
 *	default $home is /spin again instead of /tftp/spin
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cleanup of code that breaks up IP address for machine independence.  
 *      Change to use the new BuildInfo instead of SAL.Version.
 *
 * 06-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed OneShellCommand to check NIL return result from
 *      Thread.Join.  If NIL it calls FailStop().
 *
 * 24-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Set wantText false for Readline since sal cooks IO.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 17-May-96  Brian Bershad (bershad) at the University of Washington
 *	Run OneShellCommand as same thread id as caller.
 *
 * 14-May-96  Brian Bershad (bershad) at the University of Washington
 *	Removed Init.
 *
 * 14-May-96  Brian Bershad (bershad) at the University of Washington
 *	Backed out Exit exception. Served no purpose.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 17-Feb-96 Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of warnings.
 *
 * 24-Jan-96  Brian Bershad (bershad) at the University of Washington
 *	Authorization.
 *
 * 16-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added exception handling to deal with dispatcher errors.
 *
 * 09-Jan-95 Przemek Pardyak (pardy) at the University of Washington
 *	Switched to a single dispatcher exception.
 *
 * 05-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added explicit uninstallation of commands.
 *	Changed return type of CommandNewInstall to an opaque descriptor.
 *
 * 04-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added comments to shell syntax (anything after '#' is ignored)
 *	Treat tabulation as a white character.  Made prompt string
 *	a constant.
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread.
 *
 * 19-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Use dispatcher generated exception to detect unknown commands.
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added tests for reader/write (move them to Commands directory).
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added implementation of RunCommand.
 *
 * 13-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Rewritten to use events.
 *
 * 03-Dec-95  Charlie Garrett (garrett) at the University of Washington
 *      Added ShowThreadsText and ShowDomainsText which return text
 *      strings containing the information.
 *
 * 01-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *      Added dispatcher exceptions to the list of exceptions raised
 *      by procedures that interface clients with handler installation.
 *
 * 29-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Hacked up ApplyShow to printout lines that can becut-n-Extrapasted
 *	into gdb.
 *
 * 28-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Modified MountBI to print warning message abouve
 *	Device.ErrorName() not being implemented.
 *
 * 28-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Not turning off RTOSMachine.OKToAllocAtSPL.
 *
 * 06-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Got rid of domain lists, we now use a big container domain to
 *      link against.
 *      Reorganized the core domain structure. Public domains are now
 *      a subdomain of trusted ones, so that a handle to top level
 *      trusted domains allows one to link against both trusted and public
 *      interfaces in one step.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Reminder of why UNSAFE.
 *
 * 21-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Fix to allow loading of  a trusted domain (AGAIN!!)
 *
 * 11-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	The spin public and trusted domains are imported, rather than
 *	calling Nameserver.Query().  
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Use JoinExc instead of Join; new "fault [raise|nilref|narrow]" command.
 *
 * 05-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Major restructure to break each builtin out into its own function in
 *	preparation for event based invocation. Also,
 *	  - use term interface, update to new Rofs interface, clean up
 *	    ParseArgs.
 *
 * 29-May-95  Stefan Savage (savage) at the University of Washington
 *	Added becker's DownLoad primitive and pardy's backspace fix
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. The SPIN Shell for debugging and bootstrap.
 *
 *)

(* The mother of all shells. *)

MODULE Shell;
IMPORT ShellAlias;
IMPORT Shell;
IMPORT Salnet, Word;
IMPORT Glob, Text, IO, Thread, ThreadExtra, TrackStrand, Dispatcher,
       ParseParams, SpinException;
IMPORT ShellInterface, Auth, Fmt;
IMPORT BuildInfo;
IMPORT SecurityManager;


VAR globVars : Glob.T;		(* global system variables *)

REVEAL
  T = BRANDED REF RECORD
    cmd  : Dispatcher.Binding;
    help : Dispatcher.Binding;
  END;

(*
 Given user input, parse it into a ParseParams.T;
 detect and ignore comments
 *)
PROCEDURE ParseArgs (line: TEXT): ParseParams.T =
  VAR
    begin: INTEGER := 0;
    len  : INTEGER;
    end  : INTEGER;
    argc : INTEGER:=0;
    argv : REF ARRAY OF TEXT;
    c    : CHAR;
    numWords : INTEGER := 0;
  BEGIN
    len := Text.Length(line);
    LOOP
      WHILE begin < len DO
        c :=  Text.GetChar(line, begin);
        IF c = '#' THEN len := begin; EXIT; END;
        IF c # ' ' AND c # '\t' THEN EXIT; END;
        INC(begin);
      END;
      IF begin >= len THEN EXIT; END;
      WHILE begin < len DO
        c := Text.GetChar(line, begin);
        IF c = '#' THEN len := begin; END;
        IF c = ' ' THEN EXIT; END;
        INC(begin);
      END;
      INC(numWords);
    END;
    IF numWords = 0 THEN RETURN NIL END;
    argv := NEW(REF ARRAY OF TEXT,numWords);
    begin := 0;
    LOOP
      WHILE begin < len AND Text.GetChar(line, begin) = ' ' DO
        INC(begin);
      END;
      IF begin >= len THEN EXIT; END;
      end := begin;
      WHILE end < len AND Text.GetChar(line, end) # ' ' DO INC(end); END;
      IF argc >= NUMBER(argv^) THEN EXIT; END; (* not possible? *)
      argv[argc] := Text.Sub(line, begin, end - begin);
      INC(argc);
      begin := end;
    END;
    RETURN NEW(ParseParams.T).init(ThreadExtra.GetWrSelf(), argc, argv);
  END ParseArgs; 



(* 
  Ugh. Unfortunately must run each command in a separate thread because
  children commands do all kinds of terrible things to their calling
  thread. Seems like a pretty substantial safety concern!
 *)

PROCEDURE DoIt (arg: ThreadExtra.ArgT): ThreadExtra.ResultT =
  VAR ra: REF BOOLEAN;
  BEGIN
    ra := NEW(REF BOOLEAN);
    TRY
      ra^ := Shell.Run(NARROW(arg, ParseParams.T));
    EXCEPT
    | SpinException.Exception (ec) =>
        IF ec.code = SpinException.ExceptionCode.NoHandlerInvoked THEN
          IO.Put("...command not found\n");
        ELSE
          IO.Put("ShellThread -- Thread failed. " & ec.msg & "\n");
        END;
        ra^ := FALSE;
    END;
    RETURN ra;
  END DoIt;

CONST MaxHist = 32;
TYPE
  HistIdx = [0 .. MaxHist-1];
VAR
  histList: ARRAY HistIdx OF REF ARRAY OF TEXT;
  lastHist: HistIdx;
  histMu: MUTEX;

PROCEDURE GetNextHistIdx(i: HistIdx): HistIdx =
  BEGIN
    IF i = LAST(HistIdx) THEN RETURN 0;
    ELSE RETURN i+1;
    END;
  END GetNextHistIdx;
PROCEDURE GetPrevHistIdx(i: HistIdx): HistIdx =
  BEGIN
    IF i = FIRST(HistIdx) THEN RETURN LAST(HistIdx);
    ELSE RETURN i-1;
    END;
  END GetPrevHistIdx;
  
PROCEDURE AddHist (arg: REF ARRAY OF TEXT) =
  BEGIN
    LOCK histMu DO 
      histList[lastHist] := arg;
      lastHist := GetNextHistIdx(lastHist);
    END;
  END AddHist;

PROCEDURE ArgListToText (arg: REF ARRAY OF TEXT): TEXT =
  VAR t: TEXT := arg[0];
  BEGIN
    FOR i := 1 TO LAST(arg^) DO
      t := t & " " & arg[i];
    END;
    RETURN t;
  END ArgListToText;

PROCEDURE FindHist (h: TEXT): TEXT =
  VAR
    i: HistIdx := lastHist;
    cnt: CARDINAL := 0;
    
  BEGIN
    LOCK histMu DO 
      IF Text.Equal(h, "!!") THEN
	WITH arg = histList[GetPrevHistIdx(lastHist)] DO
	  IF arg = NIL THEN RETURN NIL; END;
	  RETURN ArgListToText(arg);
	END;
      END;

      IF Text.Equal(h, "!*") THEN
	WITH arg = histList[GetPrevHistIdx(lastHist)] DO
	  IF arg = NIL THEN RETURN NIL; END;
	  RETURN arg[LAST(arg^)];
	END;
      END;
      
      (* H is of form "!string". Look for first history that starts with
       "	string" *)
      WITH str = Text.Sub(h, 1),
	strLen = Text.Length(str) DO 
	WHILE cnt < MaxHist DO
	  i := GetPrevHistIdx(i);
	  INC(cnt);
	  IF histList[i] = NIL THEN RETURN NIL; END;
	  IF Text.Length(histList[i][0]) >= strLen
	    AND Text.Equal(Text.Sub(histList[i][0], 0, strLen), str) THEN
	    RETURN ArgListToText(histList[i]);
	  END;
	END;
      END;
      RETURN NIL;
    END;
  END FindHist;

PROCEDURE OneShellCommand (t: TEXT): BOOLEAN =
  VAR pp  : ParseParams.T;
      res : REFANY;
    th:Thread.T;
    histCharFound := FALSE;
    cmd:TEXT;
    alias: TEXT;
  BEGIN
    pp := ParseArgs(t);
    IF pp = NIL THEN RETURN TRUE; END;

    (* Process alias *)
    alias := ShellAlias.Find(pp.arg[0]);
    IF alias # NIL THEN
      pp.arg[0] := alias;
      IO.Put(ArgListToText(pp.arg)); IO.Put("\n");
      pp := ParseArgs(ArgListToText(pp.arg));
    END;
    
    TRY
      (* Substitute history *)
      FOR i := 0 TO LAST(pp.arg^) DO
	IF Text.GetChar(pp.arg[i], 0) = '!' THEN
	  WITH substitute = FindHist(pp.arg[i]) DO
	    IF substitute = NIL THEN
	      IO.Put(pp.arg[i] & ": history not found.\n");
	    ELSE
	      pp.arg[i] := substitute;
	      histCharFound := TRUE;
	    END;
	  END;
	END;
      END;
      IF histCharFound THEN
	IO.Put(ArgListToText(pp.arg));IO.Put("\n");
	pp := ParseArgs(ArgListToText(pp.arg));
      END;
      
      pp := Glob.Substitute(Shell.Vars(), pp);

    EXCEPT
      Glob.Error => IO.Put("Command voided\n"); RETURN FALSE;
    END;
    
    TRY
      cmd := pp.peekNext();
    EXCEPT
      ParseParams.Error => 
      IO.Put("Parse fail. Command voided\n");
      RETURN FALSE;
    END;
    th := ThreadExtra.PFork(DoIt, pp);
    TrackStrand.SetName(ThreadExtra.GetTracker(th),
                        Text.CatMultiple(
                            ARRAY OF TEXT{"Shell_", cmd, "_", 
                                          Fmt.Int(ThreadExtra.GetId(th))}));
    res := Thread.Join(th);
    IF res # NIL THEN
      AddHist(pp.arg);
      RETURN NARROW(res, REF BOOLEAN)^;
    ELSE
      RETURN FailStop();
    END;
  END OneShellCommand;


(* By default, DO NOT FAIL when we find a command that can not execute
   correctly.  *)
PROCEDURE FailStop() : BOOLEAN =
  BEGIN
    RETURN Glob.IsTrue(Shell.Vars(), FAILSTOP);
  END FailStop;

PROCEDURE Verbose() : BOOLEAN =
  BEGIN
    RETURN Glob.IsTrue(Shell.Vars(), VERBOSE);
  END Verbose;


PROCEDURE Prompt (): TEXT =
  BEGIN
    TRY
      RETURN Glob.Lookup(Shell.Vars(), PROMPT);
    EXCEPT
      Glob.Error => RETURN "!>";
    END;
  END Prompt;

PROCEDURE Status () : TEXT =
  BEGIN
    TRY
      RETURN Glob.Lookup(Shell.Vars(), STATUS);
    EXCEPT
      Glob.Error => RETURN NIL;
    END;
  END Status;


PROCEDURE IsExit (t: TEXT): BOOLEAN =
  VAR
    pp : ParseParams.T;
    msg: TEXT;
  BEGIN
    pp := ParseArgs(t);
    IF pp = NIL THEN RETURN FALSE; END;
    IF NOT pp.testNext("exit") THEN
      (* Sitting at an exit *)
      RETURN FALSE;
    END;

    TRY
      msg := pp.getNext();
    EXCEPT
      ParseParams.Error => msg := MINUSONE;
    END;

    Glob.SetVariable(Vars(), EXIT_STATUS, msg);
    RETURN TRUE;
  END IsExit;

PROCEDURE CommandLoop (showPrompt: BOOLEAN := TRUE;
                       echoChars : BOOLEAN := TRUE  ) =
  VAR
    me    : TEXT;
    t     : TEXT;
    prompt: TEXT;
    status: TEXT;
    pos   : CARDINAL;
  BEGIN
    pos := 0;
    IF showPrompt THEN
      me := "(" & Fmt.Int(ThreadExtra.GetId(Thread.Self())) & ") ";
    END;
    LOOP
      IF showPrompt THEN
        prompt := me & Prompt();
        IO.Put(prompt);
        pos := Text.Length(prompt);
      END;
      IF IO.EOF() THEN EVAL IsExit("exit 0"); EXIT; END;
      TRY
        t := IO.ReadLine(
               position := pos, echo := echoChars, wantText := FALSE);
      EXCEPT
      | IO.Error => IO.Put("io-error\n"); EVAL IsExit("exit -2"); RETURN;
      END;
      IF Verbose() THEN IO.Put("read line <" & t & ">\n"); END;
      IF t # NIL THEN
        IF IsExit(t) THEN
          IF Verbose() THEN IO.Put("Exiting..." & Status() & "\n"); END;
          RETURN;
        END;
        Glob.SetVariable(Vars(), EXIT_STATUS, NIL);
	        (* Clear exit variable status *)
        IF OneShellCommand(t) = FALSE THEN
          (* Command failed. *)
          IF FailStop() THEN
            Glob.SetVariable(Vars(), STATUS, MINUSONE);
            Glob.SetVariable(Vars(), EXIT_STATUS, MINUSONE);
            IF Verbose() THEN IO.Put("Exiting..." & Status() & "\n"); END;
            IO.Put("Fatal error.\n");
            RETURN;
          END;
        ELSE
          (* If the command exited with an explicit EXIT_STATUS,
             then pass on as STATUS, else indicate good status
           *)
          status := Glob.GetVariable(Vars(), EXIT_STATUS);
          IF status = NIL THEN
            Glob.SetVariable(Vars(), STATUS, ZERO);
          ELSE
            Glob.SetVariable(Vars(), STATUS, status);
          END;
        END;
      END;
    END;
  END CommandLoop;


(* New and improved interface *)

PROCEDURE Run (<*UNUSED*>pp: ParseParams.T) : BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Run;

PROCEDURE Help(<*UNUSED*>command: TEXT; <*UNUSED*>detailed: BOOLEAN) =
  BEGIN
  END Help;

(***************************************************************)

PROCEDURE RunResultHandler(
	  VAR finalResult : BOOLEAN;
              thisResult  : BOOLEAN;
 <*UNUSED*>   last        : BOOLEAN;
 <*UNUSED*> VAR arg       : REFANY;
                pp        : ParseParams.T) =
  BEGIN
    pp.reset ();
    finalResult := thisResult OR finalResult;
  END RunResultHandler;


PROCEDURE InitDefaultHandlers () =
  VAR
    defaultResult := NEW (REF INTEGER);
  BEGIN
    TRY
      (* remove the default handler so that there will be an exception *)
      (* raised for a command not explicitly handled by some other handler *)
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(Shell.Run));
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(Shell.Help));

      (* result handlers *)
      defaultResult^ := ORD (FALSE);
      Dispatcher.InstallResultHandler (Shell.Run, 
                                       RunResultHandler,
                                       THIS_MODULE (),
                                       NIL);
    EXCEPT
    | Dispatcher.Error =>
        IO.PutError("Spinshell cannot remove default handlers\n");
    END;
  END InitDefaultHandlers;

VAR
  verbose: BOOLEAN := FALSE;

PROCEDURE InitDispatcher () =
  BEGIN
    InitDefaultHandlers();
    TRY
      Dispatcher.InstallAuthorizerForEvent(
        NEW(Auth.AuthAlways), Shell.Run, THIS_MODULE());
      Dispatcher.InstallAuthorizerForEvent(
        NEW(Auth.AuthAlways), Shell.Help, THIS_MODULE());
    EXCEPT
      Dispatcher.Error =>
        IO.Put("Spin shell could not install authorizer\n");
    END;
  END InitDispatcher;


 PROCEDURE SetIds () =
   VAR
     w : Word.T;
     ip: TEXT;
     ipbyte: TEXT;
   BEGIN

    w := Salnet.GetLocalIp();
    Glob.SetVariable(globVars, HOSTID, "0x" & Fmt.Unsigned(w));
    ip := "";

    ipbyte :=   Fmt.Int(Word.Extract(w, 0, 8));
    Glob.SetVariable(globVars, IPADDR0, ipbyte);
    ip := ipbyte & ".";
    
    ipbyte :=   Fmt.Int(Word.Extract(w, 8, 8));
    Glob.SetVariable(globVars, IPADDR1, ipbyte);
    ip := ip & ipbyte & ".";

    ipbyte :=   Fmt.Int(Word.Extract(w, 16, 8));
    Glob.SetVariable(globVars, IPADDR2, ipbyte);
    ip := ip & ipbyte & ".";

    ipbyte :=   Fmt.Int(Word.Extract(w, 24, 8));
    Glob.SetVariable(globVars, IPADDR3, ipbyte);
    ip := ip & ipbyte;

    Glob.SetVariable(globVars, IPADDR, ip);
  END SetIds;

PROCEDURE SetGlobals() =
  VAR
                   VAR version       : TEXT;
                   VAR buildDate     : TEXT;
                   VAR builder       : TEXT;
                   VAR thisTree      : TEXT;
                   VAR target        : TEXT;
                   VAR mount_point  := BuildInfo.GetMountPoint();
                   VAR mount_pad    := BuildInfo.GetMountPad();
    spinpath := mount_point &"/" &mount_pad;
    home :TEXT;
  BEGIN 
    globVars := Glob.New(NIL);

    BuildInfo.GetInfo(version, target, buildDate, builder, thisTree);

    IF Text.Equal(spinpath,Text.Sub(thisTree,0,Text.Length(spinpath))) THEN
      WITH rest = Text.Sub(thisTree, Text.Length(spinpath)),
           end = Text.FindCharR(rest, '/') DO
           IF end < 1 THEN
             home := "/spin";
           ELSE
             home := "/spin/" &Text.Sub(rest, 0, end);
           END;
      END;
    ELSE
      home := "/spin/" &builder;
    END;

    Glob.SetVariable(globVars, HOME, home);
    Glob.SetVariable(globVars, USER, builder);
    Glob.SetVariable(globVars, VERSION, version);
    Glob.SetVariable(globVars, BUILDDIR, thisTree);
    Glob.SetVariable(globVars, TARGET, target);

    SetIds();
    IF FALSE THEN EVAL Glob.Freeze(globVars); END;
  END SetGlobals;


PROCEDURE SysVars() : Glob.T =
  BEGIN
    RETURN globVars;
  END SysVars;

PROCEDURE Vars() : Glob.T =
  BEGIN
    RETURN SecurityManager.GetCurrentProperty("shell-vars");
  END Vars;
    
 
BEGIN
  IF verbose THEN IO.Put("Initializing Shell\n"); END;
  histMu := NEW(MUTEX);
  
  SetGlobals();
  TRY
    EVAL ShellInterface.Export(NEW(Auth.AuthAlways));
    InitDispatcher();
  EXCEPT
  ELSE
    IO.Put("Shell interface could not export\n");
  END;
END Shell.





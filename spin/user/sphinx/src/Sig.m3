(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Jun-97  David Becker at the University of Washington
 *      clean up imports
 *
 * 31-May-97  David Becker at the University of Washington
 *      Brought bsd unix interfaces into sphinx from urt
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 18-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 31-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	whisted
 *)
MODULE Sig EXPORTS Sphinx, SphinxPrivate, Sig;
IMPORT Proc, ProcRep;
IMPORT Translation, Errno, ErrnoDep;
IMPORT Word, IO, RefQ;
IMPORT Thread;
IMPORT BsdWait;
IMPORT BsdSignal;
IMPORT Fmt;
IMPORT Ctypes;
IMPORT VMError;
IMPORT Debugger;
IMPORT BSDtty;
IMPORT StreamFile;
IMPORT ConsoleShell;
IMPORT SphinxMachineDep;
FROM SphinxUtils IMPORT Debug, Msg;

CONST
  (* We use two constants in addition to SIG_IGN and SIG_DFL.
   They are used to articulate what action to take for SIG_DFL.
   (if the default action is to ignore the signal, we just use SIG_IGN.
   this might make sigaction ovalue wrong, but I don't care).
   *)
  SIG_TERMINATE = -2;
  SIG_STOP = -3;
  SIG_CONT = -4;
  
PROCEDURE Sigaction (signo: INTEGER; nAddr, oAddr, tramp: Word.T) 
  : INTEGER RAISES {Errno.E, VMError.E} =
  VAR
    proc: Proc.T := Translation.GetCurrent();
    oaction: sigaction;
  BEGIN
    
    (* You can't catch  certain signals. See signal(4) *)
    IF (signo = BsdSignal.KILL
	OR signo = BsdSignal.STOP
	OR signo = BsdSignal.CONT) THEN 
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;

    IF oAddr # 0 THEN 
      oaction := proc.sigHandler[signo].action;
      (* convert internal names into public names *)
      IF oaction.sa_handler = SIG_TERMINATE
	OR oaction.sa_handler = SIG_STOP
	OR oaction.sa_handler = SIG_CONT THEN
	oaction.sa_handler := BsdSignal.SIG_DFL;
      END;
      Translation.Write(proc, VIEW(oaction, ARRAY OF CHAR), oAddr);
    END;

    IF nAddr # 0 THEN
      WITH arg = proc.sigHandler[signo] DO
	Translation.Read(proc, nAddr, VIEW(arg.action, ARRAY OF CHAR));
	IF arg.action.sa_handler = BsdSignal.SIG_DFL THEN
	  arg.action.sa_handler := DefaultAction(signo);
	END;
	arg.tramp := tramp;
      END;
    END;
    
    Check(proc);    
    RETURN 0;
  END Sigaction;


PROCEDURE DefaultAction (signo: [0 .. BsdSignal.Max]): Word.T =
  BEGIN
    CASE signo OF
    | BsdSignal.URG, BsdSignal.CHLD, BsdSignal.IO,
      BsdSignal.WINCH, BsdSignal.INFO =>
	RETURN BsdSignal.SIG_IGN;
    | BsdSignal.STOP, BsdSignal.TSTP, BsdSignal.TTIN, BsdSignal.TTOU =>
	RETURN SIG_STOP;
    | BsdSignal.CONT =>
	RETURN SIG_CONT;
    ELSE
      RETURN SIG_TERMINATE;
    END;
  END DefaultAction;

PROCEDURE SetDefaultHandlers (VAR x: ARRAY [0..BsdSignal.Max] OF Proc.SigArgs) =
  BEGIN
    FOR i := BsdSignal.Min TO BsdSignal.Max DO
      x[i].action.sa_handler := DefaultAction(i);
      x[i].action.sa_mask := BsdSignal.Mask(i);
      x[i].action.sa_flags := 0;
    END;
  END SetDefaultHandlers;

(* Signal wait stuff *)

PROCEDURE Sigsuspend(mask: INTEGER) =
  VAR
    proc: Proc.T := Translation.GetCurrent();
  BEGIN
    LOCK proc.mu DO
      proc.sigMask := mask;
      IO.Put("sigsuspend: begin\n");
      IF Word.And(proc.sig, Word.Not(proc.sigMask)) = 0 THEN 
	Thread.Wait(proc.mu, proc.cond);
      END;
      IO.Put("sigsuspend: end\n");
    END;
    Check(proc);
  END Sigsuspend;

PROCEDURE Sigprocmask (how : INTEGER; mask : Ctypes.unsigned_long) : INTEGER
  RAISES {Errno.E} =
  VAR
    proc: Proc.T := Translation.GetCurrent();
    oldMask := proc.sigMask;
  BEGIN
    
    CASE how OF
    | 1 => (* SIG_BLOCK  *)
      proc.sigMask := Word.Or(proc.sigMask, mask);
    | 2 => (* SIG_UNBLOCK *)
      proc.sigMask := Word.And(proc.sigMask, Word.Not(mask));
    | 3 => (* SIG_SETMASK *)
      proc.sigMask := mask;
    ELSE
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;

    (* kill, stop, and cont can't be caught. See UNIX manual. *)
    BsdSignal.DelSet(proc.sigMask, BsdSignal.KILL);
    BsdSignal.DelSet(proc.sigMask, BsdSignal.STOP);
    BsdSignal.DelSet(proc.sigMask, BsdSignal.CONT);
    Check(proc);
    RETURN oldMask;
  END Sigprocmask;
  
PROCEDURE KillInternal (proc: Proc.T; signo: INTEGER) =
  BEGIN
    IF signo = 0 THEN
      (* signo 0 is there just to check the validity of "proc" argument. *)
      RETURN;
    END;
    IF signo = BsdSignal.ABRT THEN
      IO.Put("SIGABRT posted. Entering debugger for your convenience.\n");
      Debugger.Enter();
    END;
    
    LOCK proc.mu DO
      BsdSignal.AddSet(proc.sig, signo);
      (* The syscall handler for this proc may be waiting on
         sigsuspend. Wake her up *)
      Thread.Signal(proc.cond);
    END;
  END KillInternal;

(* Check a signal if it has arrived and is not masked.
   This proc sends at most only one signal regardless of # of pending signals.

   Pre: "proc" must be self.
  *)
PROCEDURE Check (proc: Proc.T) =
  VAR
    activeSigs: Proc.SigSet;
    sendSIGCHLDtoParent: BOOLEAN;
    needSuicide := FALSE;
    exitCode: INTEGER;
  BEGIN
    IF proc.sig = 0 THEN
      RETURN;
    END;

    LOOP 
      LOCK proc.mu DO 
	sendSIGCHLDtoParent := FALSE;
	
	activeSigs := Word.And(proc.sig, Word.Not(proc.sigMask));
	
	IF activeSigs = 0 THEN RETURN; END;
	
	FOR signo := BsdSignal.Min TO BsdSignal.Max DO
	  IF BsdSignal.IsMember(activeSigs, signo) THEN
	    (* Check this signal *)
	    IF Debug THEN
	      Msg("receive sig ", Fmt.Int(signo));
	    END;
	    
	    (* first, cancel the signal *)
	    BsdSignal.DelSet(proc.sig, signo);
	    <*ASSERT proc.sigHandler[signo].action.sa_handler # BsdSignal.SIG_DFL*>	    
	    CASE proc.sigHandler[signo].action.sa_handler OF
	    | BsdSignal.SIG_IGN =>
(*	      IO.Put(proc.print() & ": ignoring "
		     & BsdSignal.Name(signo) & "\n");*)
	      (* do nothing, continue on *)
	    | SIG_TERMINATE =>
(*	      IO.Put(proc.print() & ": terminated by  "
		     & BsdSignal.Name(signo) & ".\n"); *)
	      exitCode := BsdWait.Compose(signo, 0);
	      needSuicide := TRUE;
	    | SIG_STOP =>
	      IO.Put("stop signal " & Fmt.Int(signo) & ".\n");
	      
	      CASE proc.state OF
	      | Proc.State.Active =>
		(* We can't just call UserSpaceThread.Suspend , because
		 we may end up blocking myself *)
		proc.state := Proc.State.Stopped;
		proc.exit := BsdWait.Compose(BsdWait.WSTOPPED_, 0);
	      | Proc.State.Stopped, Proc.State.Zombie => (* do nothing *)
	      END;
	      Proc.AddToWaitList(proc);
	      sendSIGCHLDtoParent := TRUE;
	      
	    | SIG_CONT =>
	      IO.Put("continue signal " & Fmt.Int(signo) & ".\n");
	      CASE proc.state OF
	      | Proc.State.Stopped =>
		proc.state := Proc.State.Active;
		proc.exit := BsdWait.Compose(BsdWait.WCONTINUED_, 0);
	      | Proc.State.Active, Proc.State.Zombie => (* do nothing *)
	      END;
	      Proc.AddToWaitList(proc);
	      sendSIGCHLDtoParent := TRUE;
	    ELSE
	      (* User defined action *)
	      TRY
		SphinxMachineDep.SetupSignalContext(proc, signo,
						    proc.sigHandler[signo]);
		proc.sigMask := proc.sigHandler[signo].action.sa_mask;
	      EXCEPT
	      | VMError.E =>
		IO.Put("Sig.Check: segv while sending signal.");
		IO.Put("Caution! user app will get hosed\n");
	      END;
	    END;
	    EXIT;
	  END;
	END; (* FOR *)
	
	IF sendSIGCHLDtoParent AND proc.parent # NIL THEN
	  KillInternal(proc.parent, BsdSignal.CHLD);
	END;

	IF NOT needSuicide AND proc.state = Proc.State.Stopped THEN
	  IO.Put("proc " & Fmt.Int(proc.pid) & ": I'm stopped. Waiting\n");
	  Thread.Wait(proc.mu, proc.cond);
	  IO.Put("proc " & Fmt.Int(proc.pid) & ": wakeup\n");
	ELSE
	  EXIT;
	END;
      END; (* LOCK *)
    END; (* LOOP *)
    IF needSuicide THEN
      ExitInternal(proc, exitCode);
    END;
  END Check;

PROCEDURE Kill (pid : INTEGER; signo : INTEGER) : INTEGER RAISES {Errno.E} =
  BEGIN
    IF pid >= 0 THEN
      (* Destined to a process *)
      KillInternal(Proc.FindFromID(pid), signo);
      Check(Translation.GetCurrent());
    ELSE
      EVAL Killpg(-pid, signo);
    END;
    RETURN 0;
  END Kill;

PROCEDURE Killpg (gid : INTEGER; signo : INTEGER): INTEGER RAISES {Errno.E} =
  VAR
    grp := Proc.FindGroupFromID(gid);
    itr: RefQ.Iterator;
    rq: RefQ.T;
  BEGIN
    IF signo = 0 THEN RETURN 0; END;
    
    LOCK grp.mu DO 
      itr := RefQ.Iterate(grp.members);
      WHILE RefQ.NextItr(itr, rq) DO
	KillInternal(rq.data, signo);
      END;
    END;
    Check(Translation.GetCurrent());
    RETURN 0;
  END Killpg;

PROCEDURE Sigstack (in, out : Word.T) : INTEGER
  	RAISES {Errno.E, VMError.E} =
  TYPE Stack = RECORD
    sp: Word.T;
    onStack: Ctypes.int;
  END;
  VAR
    proc: Proc.T := Translation.GetCurrent();
    outStack: Stack;
  BEGIN
    IF in # 0 THEN
      IO.Put("sigstack : you can't set your own stack now.\n");
      RAISE Errno.E(ErrnoDep.ENOSYS);
    END;
    
    IF out # 0 THEN
      outStack.sp := 0;
      outStack.onStack := 0;
      Translation.Write(proc, VIEW(outStack, ARRAY OF CHAR), out);
    END;
    RETURN 0;
  END Sigstack;
  
PROCEDURE TtySignal (signo: INTEGER) =
  VAR
    f := StreamFile.InternTty("/dev/console");
  BEGIN
    IF signo = 2 OR signo = 3 THEN
      ConsoleShell.DefaultSignalHandler(signo);
      RETURN;
    END;
    
    TRY 
      EVAL Kill(-f.gid, signo);
    EXCEPT
    | Errno.E(e) =>
      IO.Put("error on tty signal " & Errno.Fmt(e) & ".\n");
    END;
  END TtySignal;


PROCEDURE SetSigHandler(tty: BSDtty.T) =
  BEGIN
    tty.sigHandler(TtySignal);
  END SetSigHandler;
  
BEGIN
END Sig.

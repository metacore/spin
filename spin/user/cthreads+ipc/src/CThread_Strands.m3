(*
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	CThreads, native implementation.
 *)
MODULE CThread_Strands;
IMPORT UserSpaceThread, Space, Strand, Machine, CPUState, Clib, Fmt;
IMPORT Condition, Boundary, SMutex, Word, RefRefTbl, OS, Sched, FPUState;
IMPORT Dispatcher, When, Trap;

CONST
  MaxTCBs = 32;

VAR
  freemutex: SMutex.T;
  freethreads: T := NIL;

PROCEDURE MyStrand(s: REFANY) : BOOLEAN =
  BEGIN
    RETURN TYPECODE(s) = TYPECODE(T);
  END MyStrand; 

PROCEDURE RunHandler(s: REFANY) =
  VAR th : T;
  BEGIN
    th := NARROW(s, T);
    Space.Activate(th.space);
    (*
      Clib.Print("Resuming cthread at pc=" & Fmt.Unsigned(th.cpustate.pc) &
                                      " sp=" & Fmt.Unsigned(th.cpustate.usp) &
                                      "\n");
                                      *)
    CPUState.SetUser(th.cpustate);
  END RunHandler;
 
PROCEDURE StopHandler(s: REFANY) : BOOLEAN =
  VAR th : T;
  BEGIN
    th := NARROW(s, T);
    CPUState.GetUser(th.cpustate);
    (*
    Clib.Print("Stopping cthread at pc " & Fmt.Unsigned(th.cpustate.pc) &
                                     " sp=" & Fmt.Unsigned(th.cpustate.usp) &
                                     "\n");
    *)
    IF th.fpuused THEN
      FPUState.Get(th.fpustate);
    END;
    RETURN TRUE;
  END StopHandler;

(**********************************************************************) 

PROCEDURE GetT() : T =
  VAR th: T;
  BEGIN
    SMutex.Lock(freemutex);
    IF freethreads = NIL THEN
      FOR i := 1 TO MaxTCBs DO
        th := NEW(T);
        th.lock := SMutex.Alloc();
        th.done := Condition.Alloc();
        th.ctnext := freethreads;
        freethreads := th;
      END;
    END;
    th := freethreads;
    freethreads := freethreads.ctnext;
    SMutex.Unlock(freemutex);
    RETURN th;
  END GetT;

PROCEDURE Fork(th: UserSpaceThread.T; startpc,arg,gp,ra,startsp: Word.T) : T = 
  VAR
    newt: T;
  BEGIN
    (*
     * We assume that the stack has already been allocated
     *)
    newt := GetT();
    newt.space := UserSpaceThread.GetSpace(th);
    newt.uth := th;
    newt.cpustate.usp := startsp;
    newt.cpustate.pc := startpc;
    newt.cpustate.a0 := arg;
    newt.cpustate.gp := gp;
    newt.cpustate.ra := ra;
    newt.cpustate.pv := newt.cpustate.pc;
    newt.cpustate.ps := 16_0018;
    Strand.Unblock(newt);
    RETURN newt;
  END Fork;

PROCEDURE Exit(cthread: T; exitcode: Word.T) =
  VAR 
    th: REFANY;
  BEGIN
    SMutex.Lock(cthread.lock);
    cthread.returned := TRUE;
    cthread.result := exitcode;
    Condition.Broadcast(cthread.done);
    SMutex.Unlock(cthread.lock);
    Strand.Block(cthread);
  END Exit;

PROCEDURE Join(cthread: T) : Word.T =
  VAR exitcode: Word.T;
  BEGIN
    SMutex.Lock(cthread.lock);
    IF cthread.returned = FALSE THEN
      Condition.Wait(cthread.done, cthread.lock);
    END;
    exitcode := cthread.result;
    SMutex.Unlock(cthread.lock);
    RETURN exitcode;
  END Join;

PROCEDURE Syscall(ss: REF Machine.SavedState) =
  VAR
    cthreadcaller, cthread: T;
    uth, caller: UserSpaceThread.T;
    stack: CARDINAL;
  BEGIN
    Clib.Print("Client specific system call handler. Syscall is " &
               Fmt.Int(ss.v0) & "\n");
    CASE ss.v0 OF
    | 50 => 
      caller := NARROW(ss.strand, UserSpaceThread.T);
      ss.v0 := UserSpaceThread.Externalize(caller, 
                                     Fork(caller,
                                          ss.a0, ss.a1, ss.a2, ss.a3, ss.a4));
    | 51 => Exit(NARROW(ss.strand, T), ss.a0);
    | 52 => 
      IF TYPECODE(ss.strand) = TYPECODE(UserSpaceThread.T) THEN
        uth := NARROW(ss.strand, UserSpaceThread.T);
        cthread := NARROW(UserSpaceThread.Internalize(uth, ss.a0), T);
        ss.v0 := Join(cthread);
      ELSIF TYPECODE(ss.strand) = TYPECODE(T) THEN
        cthreadcaller := NARROW(ss.strand, T);
        cthread:=NARROW(UserSpaceThread.Internalize(cthreadcaller.uth,ss.a0),T);
        ss.v0 := Join(cthread);
      END;
    ELSE
    END;
  END Syscall;

BEGIN
    freemutex := SMutex.Alloc();
    EVAL Dispatcher.Install(Strand.Run, MyStrand, RunHandler);
    EVAL Dispatcher.Install(Strand.Stop, MyStrand, StopHandler);
    EVAL Trap.InstallSyscallHandler(Syscall, NIL);
    Clib.Print("Cthread_strands added to the kernel...\n");
END CThread_Strands.

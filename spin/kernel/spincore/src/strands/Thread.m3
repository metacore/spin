(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Simplified exception handing.  Removed ProcessStacksCl.
 * 
 * 10-Dec-97  Tian Fung Lim (tian) at the University of Washington
 *	Fixed idle time computation.
 *
 * 09-Dec-97  David Becker at the University of Washington
 *      Assert that Exit_main() does not return.
 *
 * 03-Dec-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added support for DebugOption.Security.
 *
 * 19-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Added RemoveDeadThreads to support real-time collection of dead
 *	threads from activeThreads list.  Also changed ProcessStacks to
 *	handle the case when threads are not yet initialized.
 *
 * 17-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added computation of idle time, which is reset and dumped through
 *	Spy.  It slows down context switching slightly but can be turned
 *	off through DebugOption.MeasureIdleTime.
 *
 * 01-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added support for new security info management.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Sal and CPU interfaces
 *
 * 09-Apr-97  Charles Garrett (garrett) at the University of Washington
 *	Added SetProfiled which enables/disables profiling of an
 *	 individual thread. The idle thread and the collector thread
 *       are not profiled by default, all other threads are.
 *
 * 19-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *	Moved checking the redzones to after logging to be able to
 *	log even if arguments are screwed-up.  Renamed stack_bottom to
 *	stack_top in ProcessStack.  
 *
 * 10-Mar-97  Robert Grimm (rgrimm) at the University of Washington
 *      Domain stack is now newly created on thread creation
 *      out of a pool of stack elements, cleaned up some
 *
 * 08-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added preliminary version IsOnCurrentStack written by Przemek
 *
 * 05-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Uncommented in the use of BootIO.Reader&Writer.
 *
 * 24-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made most of the TrackStrand operations conditional.
 *
 * 24-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	Made all profile operations conditional on the 
 *      ProfileSupport.CompiledIn constant. Commented out the use
 *      of BootIO.Reader and Writer which are used for the demo.
 *
 * 10-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity and inlined copying of security relevant info
 *
 * 23-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed GetBoundUserStrand to return NIL when there is no user strand.
 *
 * 10-Dec-96  Charles Garrett (garrett) at the University of Washington
 *	Added the ProfilingRunHandler which is installed when call graph
 *	 profiling support is compiled into the Sal.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Change to TrackStrand from Spy
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cleanup for machine-independence.
 *
 * 17-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Added translation from Strand.RaiseException to
 *	 Thread.RaiseException.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 18-May-96 Przemek Pardyak (pardy) at the University of Washington
 *	Help the garbage collector by zeroing the prev and next fields
 *	of Strand.T inside of Collector.  
 *
 * 01-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Stacks are now statically sized to 64KB.  
 *	Applied support to either register stacks
 *	with strongref or as clean untraced regions
 *	with the GC.  The former requires that the
 *	ThreadPrivate.StackT type is changed to be
 *	a REF ARRAY.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Removed GetSal from register strand (bershad)
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 16-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Set up io reader/writer during trap handler creation.
 *	
 * 29-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Reduced DeadWakeupThresh to 2 until we have cooperative GC
 *	 between primary GC and other modules.
 *
 * 07-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed stack reuse and garbage collection scan problems.
 *
 * 13-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Made stacks traced & strongreffed instead of untraced to make life
 *	easier for the untraced heap consistency checker.
 *
 * 12-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Return excess stacks back to the untraced heap.
 *
 * 22-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Inherit rd/wr from parent thread.
 *
 * 01-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *      Added exception handling for dispatcher calls.  Corrected
 *      type errors in handler installation. 
 *
 * 15-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added Pause.
 *
 * 06-Nov-95 Przemek Pardyak (pardy) at the University of Washington
 *	Fixed ProcessStacks to look at the top of the stack for the
 *      current thread.
 *
 * 21-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Strengthened LOOPHOLE(s,T) to NARROW(s,T)
 *
 * 31-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	use ref procedure instead of procedure.
 *
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Now implements the M3 Thread interface. Moved MachSupport
 *      out of this file. General cleanup.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Added firewall support.
 *
 * 23-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added preallocate to shield fast spawning clients from
 *	the M3 heap allocator.
 *
 * 19-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Moved continuation support out of the context switch path.
 *	Place the thread state at the top of stack, reduce allocator pressure.
 *
 * 14-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use fastlists to manage stacks.
 *
 * 11-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Made AssertWait allocate a condition variable even if Event = 0
 *
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Picked up Sanislo's thread mods for CAM and OSF/1 devices.  
 *	Use console interface.
 *
 * 30-Jul-95  Przemek Pardyak (pardy) at the University of Washington
 *	Fixed ProcessStacks to not collect stacks which were inactive.
 *
 * 13-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added support for Mach/OSF compatibility layer.
 *
 * 04-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	activeThreads management for GC was screwing up dead thread chains.
 *	Fixed the GC ambiguous root management code (see the comments below)
 *      to not only work but to not take up any time on the fork/exit/join
 *      fastpath.
 *
 * 10-Apr-95  Marc Fiuczynski (mef) at the University of Washington
 *	Changed activelist mngmt for GC.
 *	Added support to collect timing statistics. 
 *
 * 06-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *	Added support for garbage collection.
 *	Added support for frame-based exception handling.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. In-Sal threads.
 *      This routine implements the M3 Thread interface, as well as
 *      the compiler required RTHooks interface to deal with exceptions,
 *      in addition to some SPIN extensions to the standard Thread interface
 *      for high performance.
 *
 *) 
UNSAFE (* has to cast from function pointer to word *)
MODULE Thread EXPORTS Thread, ThreadExtra, ThreadRep, ThreadPrivate, 
                             ThreadF, ThreadExtern, RT0u, RTHooks;
IMPORT ThreadException, ThreadPrivate;
IMPORT Translation;
IMPORT Strand, StrandRep, Sema, FastList, SpinException;
IMPORT AtomicOps, CPU, CPUExtern;
IMPORT  CPUPrivate;
IMPORT Debugger, Dispatcher, DebugOption, Word;
IMPORT MachineThread, RTThread;
IMPORT Clock;
IMPORT Log, SalExtern;
IMPORT Wr, Rd, IO, Fmt;
IMPORT RT0u;
IMPORT ProfileSupport;
IMPORT SecurityContext;
IMPORT RTIO, RTOS;
IMPORT BootIO;
IMPORT TrackStrand;
IMPORT RTCollectorSRC;




(* There are two things we can do with stacks WRT the collector.
   1. place them in the traced heap and strongref the referent.
   2. a) place them in the untraced heap.
      b) register stacks as clean regions in the untraced heap.
*)

(* We cannot dispose Thread.T's, because people may have outstanding   *)
(* references to them. We do, however, collect and reuse the stacks.    *)

(* The FastList.T next field in the strand is used by the mutex code. *)
(* So no privileged code should enqueue or dequeue active threads. *)

CONST
  MaxStacks = 16; (* number of stacks allocated at a time when we run out *)
  MaxTCBs = 250;  (* number of TCBs allocated at a time when we run out *)
  MaxDeadThreads = 10; (* no. of dead threads that trigger stack recollect *)
  StackDebug = FALSE;
  
VAR 
    (* 
     * list of active threads (used by GC to find ambiguous roots)
     *)
    activeThreads: REF REFANY := NIL;

    (*
     * Unused thread control blocks
     *)
    freeThreads: REF FastList.T;

    (*
     * Graveyard for terminated (pre-collection) threads
     *)
    deadThreads: REF FastList.T;
    deadSema: Sema.T;
    deadcount: INTEGER := 0;

    (* 
     * Our own pool of stacks for fast allocation.
     *)
    freeStacks: REF FastList.T;
    freeStackCnt: INTEGER := 0;

    (* Primitive red zones until we have page mode bits work for the *)
    (* Sal space. *)
    RedZoneMsg := ARRAY [0..8] OF CHAR{'w','H','i','R','l','Y','g','I','g'};


(* Globals for measurement of idle time *)
VAR
  idleTime  : Word.T;
  idleStart : Word.T;
  last : T;
  idleT     : T;


(*  Trackstrand, the memory accounting object for strands, is now optional
 *      and on its way out. Some calls
 *	to trackstrand are still necessary to setup pointers that are
 *	used by other Track services during Domain Link operations.  It
 *	turned out to be too difficult at this stage to turn off
 *	TrackStrand completely.  However, with DebugOption.DoTrackStrand
 *	set to FALSE there will (should) be no allocations on the
 *	critical path, which I think was the major problem.  
 *)


(*
 * Guard for Stop and Run events
 *)
FUNCTIONAL PROCEDURE MyStrand(s: Strand.T) : BOOLEAN =
  BEGIN
    RETURN TYPECODE(s) = TYPECODE(T);
  END MyStrand; 

PROCEDURE GetStack() : StackT =
  VAR stack: StackT;
  BEGIN
    LOOP
      stack := FastList.Dequeue(freeStacks);
      IF stack = NIL THEN
        FOR i := 1 TO MaxStacks DO
          stack := NEW(StackT);
          stack.base := NEW(StackData);
          IF DebugOption.DoCleanUntracedStacks THEN 
            (*
             * register with the collector that this stack is clean.
             *  the collector will sweep through stacks seperatedly.
             *)
            RTCollectorSRC.RegisterClean(ADR(stack.base[0]),
                                         ThreadPrivate.StackSize,
                                         TRUE);
          END;
          IF StackDebug THEN IO.Put("allocating a new stack\n"); END;
          FastList.Enqueue(stack, freeStacks);
          EVAL AtomicOps.AtomicInc(freeStackCnt);
        END; 
      ELSE
        EVAL AtomicOps.AtomicDec(freeStackCnt);
        RETURN stack;
      END;
    END;
  END GetStack; 

(* 
 * wipe out all possible pointers from the stack in order to help
 * the untraced heap scanner, collector and debugging.
 *)
PROCEDURE CleanStack(stack: StackT) =
  BEGIN
    (* and now some incredibly slow code -pardy *)
    FOR i := FIRST(stack.base^) TO LAST(stack.base^) DO
      stack.base[i] := VAL(0,CHAR);
    END;
  END CleanStack;

PROCEDURE NewT (): T =
  VAR t: T;
  BEGIN
    t := NEW(T);
    t.lock := NEW(MUTEX);
(*
    RTIO.PutText ("allocated a new thread at ");
    RTIO.PutAddr (LOOPHOLE (t, ADDRESS));
    RTIO.PutText (" with mutex at ");
    RTIO.PutAddr (LOOPHOLE (t.lock, ADDRESS));
    RTIO.PutText ("\n");
*)
    t.done := NEW(Condition);
    t.state := State.Nascent;
    t.exception.code := SpinException.ExceptionCode.NoException;
    t.exception.msg := NIL;
    t.reader := NIL;
    t.writer := NIL;
    RETURN t;
  END NewT;

VAR ids := 0; (* XXX temporary *)

FUNCTIONAL PROCEDURE GetId(th: T): INTEGER =
  BEGIN
    RETURN th.intid;
  END GetId;

PROCEDURE SetProfiled(th: T; b: BOOLEAN) =
  BEGIN
    IF ProfileSupport.CompiledIn THEN
      th.profileStack.flag := ORD(b);
    END;
  END SetProfiled;

PROCEDURE GetT (): T =
  VAR th: T;
  BEGIN
    LOOP
      th := FastList.Dequeue(freeThreads);
      IF th # NIL THEN
        EXIT
      ELSE
        FOR i := 1 TO MaxTCBs DO
          th := NewT();
          FastList.Enqueue(th, freeThreads);
          th.intid := ids; INC(ids); (* XXX temporary *)
          LOOP
            th.gcnext := activeThreads^;
            IF AtomicOps.CompareAndSwap(activeThreads, th.gcnext, th) THEN
              EXIT;
            END;
          END;
        END;
      END;
    END;
    th.state := State.Active;
    th.exception.code := SpinException.ExceptionCode.NoException;
    th.exception.msg := NIL;
    RETURN th;
  END GetT;

PROCEDURE Preallocate(n: CARDINAL) =
  VAR th: T;
  BEGIN
    (*
     * XXX need admission control
     * notice that we are running with the client's id here,
     * so anyone who looks at current id will do admin control
     * correctly.
     *)
    FOR i := 1 TO n DO
      th := NewT();
      FastList.Enqueue(th, freeThreads);
      th.intid := ids; INC(ids); (* XXX temporary *)
      LOOP
        th.gcnext := activeThreads^;
        IF AtomicOps.CompareAndSwap(activeThreads, th.gcnext, th) THEN
          EXIT;
        END;
      END;
    END;
  END Preallocate;

(*
 * Collector thread reclaims stacks from dead threads
 * It is only started when it is worth starting it.
 *)
PROCEDURE Collector(<*UNUSED*>arg: ArgT) : ResultT = 
  VAR th: T;
      stack: StackT;
      notDeadYet: REF FastList.T;
  BEGIN
    notDeadYet := NEW(REF FastList.T);
    LOOP
      Sema.P(deadSema);
      LOOP
        th := FastList.Dequeue(deadThreads);
        IF th = NIL THEN 
          EXIT;
        ELSIF th.state # State.Dead THEN
          (*
           * he is not dead yet, give him a chance to die
           * before we collect his stack.
           *)
          FastList.Enqueue(th, notDeadYet);
        ELSE
          (*
           * Ok, now collect his stack, reuse it if needed or give
           * it back to the system allocator if we are already consuming
           * too much memory.
           *)
          EVAL AtomicOps.AtomicDec(deadcount);
          stack := th.stack;
          (* Collect SID stack *)
          IF DebugOption.Security THEN
            SecurityContext.CollectSIDStack( th.sid );
          END;
          (* 
           * help the garbage collector by removing dangling pointers.
           *)
          IF ProfileSupport.CompiledIn THEN
            <* ASSERT th.profileStack # curProfileStack *>
            DISPOSE(th.profileStack);
          END;

          th.nextelem := NIL;
          th.stack := NIL; 
          <* ASSERT th.profileStack # curProfileStack *>
          DISPOSE(th.profileStack);
          th.prev := NIL;
          th.next := NIL;
          th := NIL;

          IF DebugOption.DoCleanUntracedStacks THEN CleanStack(stack); END;

          IF AtomicOps.AtomicInc(freeStackCnt) > MaxStacks THEN
            (*
             * Too many cached stacks already, give this one back
             * to the system.
             *)
            EVAL AtomicOps.AtomicDec(freeStackCnt);
            IF StackDebug THEN IO.Put("disposing of a stack\n"); END;
            IF DebugOption.DoCleanUntracedStacks THEN 
              RTCollectorSRC.UnregisterClean(ADR(stack.base[0]));
            END;
            DISPOSE(stack.base);
            stack := NIL;
          ELSE
            (*
             * We want to reuse this stack.
             *)
            IF StackDebug THEN IO.Put("queueing a stack for reuse\n"); END;
            FastList.Enqueue(stack, freeStacks);
          END;
          (*
           * TCBs cannot be reused because clients may have outstanding
           * references to them. GC will pick them up when they are no
           * longer used and we'll get them back through NEW.
           *)
        END;
      END;
      (*
       * There were a number of threads that were about to die but
       * not yet dead. Put these back on the deadThreads queue so
       * we can reap their stacks next time we wake up.
       *)
      LOOP
        th := FastList.Dequeue(notDeadYet);
        IF th = NIL THEN EXIT; END;
        FastList.Enqueue(th, deadThreads);
      END;
    END;
  END Collector;

PROCEDURE Exit(result: ResultT) =
  VAR th: T;
  BEGIN
    th := NARROW(Strand.GetCurrent(), T);
    Exit_main(th, result);
    (* NOT REACHED *)
  END Exit;

PROCEDURE Kill(th: T; result: ResultT) =
  VAR
    stack : StackT;
  BEGIN
    IF th = Strand.GetCurrent() THEN
      Exit_main(th, result);
      (* NOT REACHED *)
      <* ASSERT FALSE *>
    END;
    LOCK th.lock DO
      th.res := result;
      (* the thread is dead automatically, since we can reap its stack *)
      (* right here *)
      th.state := State.Dead;
      Broadcast(th.done);
    END;
    Strand.Block(th);
    stack := th.stack;
    th.stack := NIL;
    IF stack # NIL THEN
      IF DebugOption.DoCleanUntracedStacks THEN CleanStack(stack); END;
      IF AtomicOps.AtomicInc(freeStackCnt) > MaxStacks THEN
        EVAL AtomicOps.AtomicDec(freeStackCnt);
        IF StackDebug THEN IO.Put("disposing of a stack\n"); END;
        IF DebugOption.DoCleanUntracedStacks THEN 
          RTCollectorSRC.UnregisterClean(ADR(stack.base[0]));
        END;
        DISPOSE(stack.base);
      ELSE
        IF StackDebug THEN IO.Put("queueing a stack for reuse\n"); END;
        FastList.Enqueue(stack, freeStacks);
      END;
    END;
    IF ProfileSupport.CompiledIn THEN
      <* ASSERT th.profileStack # curProfileStack *>
      DISPOSE(th.profileStack);
    END;
    IF DebugOption.DoTrackStrand THEN TrackStrand.Delete(th.track); END;
    IF DebugOption.DoStrandDebug THEN Debugger.DeregisterStrand(th); END;
  END Kill;

PROCEDURE CreateTrapHndlr() : T =
  VAR
    res: T;
  BEGIN
    res := Create(NIL, NIL, defaultPriority);

    (* we need the redirection for the X-Windows demo and for sphinx *)
    res.reader := BootIO.Reader();
    res.writer := BootIO.Writer();

    (* up the trap handler's suspend count so that when he starts *)
    (* running automaticallly in response to a trap, his suspend *)
    (* count shows that he is actually runnable. *)
    res.count := 1;

    IF DebugOption.DoTrackStrand THEN 
      TrackStrand.SetName(GetTracker(res),"SyscallHandler_"&Fmt.Int(res.tid));
    END;
    RETURN res;
  END CreateTrapHndlr;

(* Kill the syscall handler. It managed to queue itself for the next
 * syscall handling while we were in the Sal. It should not be
 * runnable at the time of the call from TrapInternal.m3.
 *)
PROCEDURE KillTrapHndlr(th: T) =
  BEGIN
    FastList.Enqueue(th, deadThreads);
    IF AtomicOps.AtomicInc(deadcount) > MaxDeadThreads THEN
      Sema.V(deadSema);
    END;
    IF DebugOption.DoTrackStrand THEN TrackStrand.Delete(th.track);  END;
    IF DebugOption.DoStrandDebug THEN Debugger.DeregisterStrand(th); END;
    th.state := State.ReadyToDie;
    Strand.Block(th);
  END KillTrapHndlr; 

(* Exit the thread th, with result *)
PROCEDURE Exit_main (th: T; result: ResultT) =
  BEGIN
    (* Fixin' to die *)
    LOCK th.lock DO
      th.res := result;
      th.state := State.ResultAvailable;
      Broadcast(th.done);
    END;
    IF DebugOption.DoTrackStrand THEN TrackStrand.Delete(th.track);  END;
    IF DebugOption.DoStrandDebug THEN Debugger.DeregisterStrand(th); END;
    th.syncnext := NIL;
    <* ASSERT th.dbgprev = NIL *>
    <* ASSERT th.dbgnext = NIL *>
    <* ASSERT th.syncnext = NIL *>
    FastList.Enqueue(th, deadThreads);
    IF AtomicOps.AtomicInc(deadcount) > MaxDeadThreads THEN Sema.V(deadSema); END;
    (*
     * We are still using the stack, so we need to wait for the
     * scheduler to take us from our current state to the dead
     * state atomically via a strand event.
     *)
    th.state := State.ReadyToDie;
    Strand.Block(th);
    (* NOT REACHED *)
    <* ASSERT FALSE *>
  END Exit_main;

PROCEDURE Create(func: FuncT;
                 arg: ArgT;
                 pri: Strand.PriorityT := defaultPriority) : T =
  VAR newt: T;
      stack: StackT;
      stackBase: Word.T;
      profileStack: ProfileData;
      state: UNTRACED REF CPU.GeneralRegs;
      t: T;
      s: Strand.T;
  BEGIN
    (*
     * Thread state is kept in a static region at the top of the stack
     * (for orientation: stack grows "down") so we put less pressure on
     * the allocator
     *)
    newt := GetT();
    stack := GetStack();

    IF ProfileSupport.CompiledIn THEN
      profileStack := NEW(ProfileData);
      profileStack.pointer := ADR(profileStack.pointer);
    END;

    (* tracking stuff *)
    newt.tid := newt.intid;
    IF DebugOption.DoTrackStrand THEN
      TrackStrand.Create(newt); 
    ELSE
      newt.track := TrackStrand.untracked;
    END;

    (*
     * Security Management:
     *   Inherit parents per-user context (inlined copy)
     *   and per-thread subject and object SID stack.
     *   Note that the current SID stack pointer
     *   is not in the Strand.T but in SecurityContext.currentSid!
     *)
    s := Strand.GetCurrent();
    <* ASSERT newt.context = NIL *>
    newt.context := s.context;
    IF DebugOption.Security THEN
      SecurityContext.InitSIDStack( newt.sid, SecurityContext.currentSid );
    END;

    (* This should probably get shoved into the identity structure *)
    IF ISTYPE(s, T) THEN
      t := NARROW(s, T);
      newt.reader := t.reader;
      newt.writer := t.writer;
    END;

    stackBase := MachineThread.InitialSP(stack);
    state := LOOPHOLE(stackBase, UNTRACED REF CPU.GeneralRegs);
    
    MachineThread.Setup(state, stackBase, KThreadBody,
                        LOOPHOLE(newt, Word.T));      
                 
    newt.ms := state;
    newt.stack := stack;
    newt.func := func;
    newt.arg := arg;
    newt.pri := pri;

    IF ProfileSupport.CompiledIn THEN
      newt.profileStack := profileStack;
    END;

    IF DebugOption.DoRedZones THEN PlaceRedZone(newt); END;
    IF DebugOption.DoStrandDebug THEN Debugger.RegisterStrand(newt); END;

    RETURN newt;
  END Create;

PROCEDURE PFork(func: FuncT;
               arg: ArgT;
               pri: Strand.PriorityT := defaultPriority) : T =
  VAR newt: T;
  BEGIN
    newt := Create(func, arg, pri);
    Strand.Unblock(newt);
    RETURN newt;     
  END PFork;

PROCEDURE Fork(cl: Closure) : T =
  BEGIN
    RETURN PFork(CallApply, cl);
  END Fork;

PROCEDURE CallApply(arg: ArgT): ResultT =
  VAR
    cl: Closure;
  BEGIN
    cl := NARROW(arg, Closure);
    RETURN cl.apply();
  END CallApply;

PROCEDURE Join (th: T): ResultT =
  BEGIN
    LOCK th.lock DO
      WHILE th.state = State.Active DO Wait(th.lock, th.done); END;
    END;
    RETURN th.res;
  END Join;

PROCEDURE AlertJoin (t: T): REFANY RAISES {Alerted} =
  BEGIN
    LOCK t.lock DO
      WHILE t.state = State.Active DO
	AlertWait(t.lock, t.done);
      END;
    END;
    RETURN t.res;
  END AlertJoin;

PROCEDURE Yield() =
  BEGIN
    Strand.Yield();
  END Yield;

PROCEDURE Self() : T =
  BEGIN
    RETURN NARROW(Strand.GetCurrent(), T);
  END Self;

(* ----------------------------------------- primitive stack size management *)
(* These interfaces make no sense in a Sal thread implementation. *)
(* Our stacks should get automatically extended on demand when spaces *)
(* work. *)
PROCEDURE GetDefaultStackSize(): CARDINAL =
  BEGIN
    RETURN ThreadPrivate.StackSize;
  END GetDefaultStackSize;

PROCEDURE MinDefaultStackSize(<*UNUSED*>min: CARDINAL) =
  BEGIN
  END MinDefaultStackSize;

PROCEDURE IncDefaultStackSize(<*UNUSED*>inc: CARDINAL) =
  BEGIN
  END IncDefaultStackSize;

(*------------------------------------------------------ internal procedures *)

PROCEDURE KThreadBody(th: T) =
  BEGIN
    Exit_main(th, th.func(th.arg));
  END KThreadBody; 
 
PROCEDURE RunHandler(s: Strand.T) =
  VAR
    th := LOOPHOLE(s, T);
    (* we know that s is a T since the guard evaluated to true. *)
  BEGIN
    IF DebugOption.Cswtch THEN
      Log.Log("Kthread (0x"); Log.LogRef(s);
      Log.Log(" pri="); Log.Logx(NARROW(s, Strand.T).pri);
      Log.Log(") resuming at pc "); Log.Logx(th.ms.pc);
      Log.Log(" and sp "); Log.Logx(th.ms.ksp);
      Log.Log("\n");
    END;
    (* Set currentSid *)
    IF DebugOption.Security THEN
      SecurityContext.currentSid := th.sid;
    END;
    (* switch the space if necessary. *)
    IF th.translation # NIL THEN
      Translation.Activate(th.translation);
    END;
    (* restore M3 exception handler stack (until we have a stack walker) *)
    SetCurrentHandlers(th.handlerStack);

    IF DebugOption.MeasureIdle  THEN
      IF  last = idleT THEN
        VAR now := CPU.CycleCounter();
            delta := CPU.CycleMinus(now,idleStart);
        BEGIN
          INC(idleTime, delta);
        END;
      END;
      IF  th = idleT THEN
        idleStart := CPU.CycleCounter();
      END;
      last := th;
    END;

    
    CPUPrivate.RestoreCalleeSaveGeneralRegs(th.ms);
  END RunHandler;

PROCEDURE ProfilingRunHandler(s: Strand.T) =
  VAR th := LOOPHOLE(s, T);
    (* we know that s is a T since the guard evaluated to true. We don't
       strictly need a NARROW here. *)
  BEGIN
    IF DebugOption.Cswtch THEN
      Log.Log("Profiled Kthread (0x"); Log.LogRef(s);
      Log.Log(" pri="); Log.Logx(NARROW(s, Strand.T).pri);
      Log.Log(") resuming at pc "); Log.Logx(th.ms.pc);
      Log.Log(" and sp "); Log.Logx(th.ms.ksp);
      Log.Log("\n");
    END;
    IF th.translation # NIL THEN
      Translation.Activate(th.translation);
    END;
    (* Set currentSid *)
    IF DebugOption.Security THEN
      SecurityContext.currentSid := th.sid;
    END;
    (* restore M3 exception handler stack (until we have a stack walker) *)
    SetCurrentHandlers(th.handlerStack);
    curProfileStack := th.profileStack;
    CPUPrivate.RestoreCalleeSaveGeneralRegs(th.ms);
  END ProfilingRunHandler;

PROCEDURE StopHandler(s: Strand.T;
                      regs: UNTRACED REF CPU.CalleeSavedRegs): BOOLEAN =
  VAR
    th := LOOPHOLE(s, T);
    (* we know that s is a T, since the guard evaluated to true; no need
       for a NARROW *)
  BEGIN
    IF DebugOption.Cswtch THEN
      Log.Log("Kthread (0x"); Log.LogRef(s);
      Log.Log(" pri="); Log.Logx(NARROW(s, Strand.T).pri);
      Log.Log(") stopping at pc "); Log.Logx(regs.ra);
      Log.Log(" and sp "); Log.Logx(regs.sp);
      Log.Log("\n");
    END;
    (* Restore sid *)
    IF DebugOption.Security THEN
      th.sid := SecurityContext.currentSid;
    END;

    IF DebugOption.DoRedZones THEN CheckRedZone(th); END;

    (* save M3 exception handler stack (until we have a stack walker) *)
    th.handlerStack := GetCurrentHandlers();

    CPU.CopyCalleeSavedRegs(regs, th.ms);

    RETURN TRUE;
  END StopHandler;



(*
 * We would really like to move ExceptionHandler to ThreadException,
 * but the dispatcher won't let us then raise the higher level ThreadException
 * RaiseException from within that module.  
 *)

(*
 * Guard for Strand.RaiseException event
 *)
FUNCTIONAL PROCEDURE MyExceptionStrand(s: Strand.T;
                    <* UNUSED *>reason: SpinException.ExceptionInfo): BOOLEAN =
  BEGIN
    RETURN TYPECODE(s) = TYPECODE(T);
  END MyExceptionStrand;


PROCEDURE ExceptionHandler(s: Strand.T; reason: SpinException.ExceptionInfo) =
  VAR th: T;
  BEGIN 
   th := NARROW(s, T);
   LOCK th.lock DO
	   th.exception := reason;
	   th.state := State.ExceptionPosted;
   END;
   ThreadException.RaiseException(th);	(* Should not return *)

   IO.Put("Thread.ExceptionHandler. Inappropriate return\n");
   EVAL ThreadException.TerminateInException(th);
   Debugger.Enter();
  END ExceptionHandler;

(* Red Zones.
 * We place a known string at the bottom of the stack and check
 * at every checkpoint. This low tech solution is necessary because
 * the Sal stacks are mapped through the superpage, which makes
 * it impossible to invalidate individual pages.
 *)
PROCEDURE PlaceRedZone(th: T) =
  BEGIN
    FOR i := FIRST(RedZoneMsg) TO LAST(RedZoneMsg) DO
      th.stack.base[111 * i] := RedZoneMsg[i];
    END;
  END PlaceRedZone;

PROCEDURE CheckRedZone(th: T) =
  BEGIN
    FOR i := FIRST(RedZoneMsg) TO LAST(RedZoneMsg) DO
      IF th.stack.base[111 * i] # RedZoneMsg[i] THEN
        IO.Put("Stack corruption!!!\n");
        Debugger.Enter();
      END;
    END;
  END CheckRedZone;

PROCEDURE Apply(cl: ThreadApplyClosure) : BOOLEAN =
  VAR
    t        : T;
    tCopySet : REF ARRAY OF T;
    totaln, n: INTEGER        := 0;
    spl      : CPU.InterruptLevel;
  BEGIN
    (*
     * Suspend all other threads to take a snapshot of the system.
     * If you don't want a coherent snapshot but can do with a snapshot
     * that guarantees that all threads that are active throughout a call
     * to apply are reported, then you can take the Spls out.
     *)
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    t := activeThreads^;
    WHILE t # NIL DO INC(totaln); t := t.gcnext; END;
    tCopySet := NEW(REF ARRAY OF T, totaln);
    n := 0;
    t := activeThreads^;
    WHILE t # NIL AND n < totaln DO 
      IF t.stack # NIL THEN (* Active right now? *)
        tCopySet^[n] := t;
        INC(n);
      END;
      t := t.gcnext;
    END;
    CPUPrivate.RestoreInterruptMask(spl);
    FOR i := FIRST(tCopySet^) TO LAST(tCopySet^) DO
      IF tCopySet^[i] # NIL THEN
        IF NOT cl.apply(tCopySet^[i]) THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END Apply;

(*------------------------------------ default input/output rd/wr support --*)

(*
 * Change the default input reader or output writer.
 *)
PROCEDURE SetRdSelf(rd: Rd.T) : Rd.T =
  VAR
    th: T := NARROW(Strand.GetCurrent(), T);
    old: Rd.T;
  BEGIN
    IF th # NIL THEN
      LOCK th.lock DO
        old := th.reader;
        th.reader := rd;
      END;
    END;
    RETURN old;
  END SetRdSelf;

PROCEDURE GetRdSelf (): Rd.T =
  VAR th: T := NARROW(Strand.GetCurrent(), T);
  BEGIN
    IF th # NIL THEN RETURN th.reader; ELSE RETURN NIL; END;
  END GetRdSelf;

PROCEDURE SetWrSelf(wr: Wr.T) : Wr.T =
  VAR
    th: T := NARROW(Strand.GetCurrent(), T);
    old: Wr.T;
  BEGIN
    IF th # NIL THEN
      LOCK th.lock DO
        old := th.writer;
        th.writer := wr;
      END;
    END;
    RETURN old;
  END SetWrSelf;

PROCEDURE GetWrSelf (): Wr.T =
  VAR th: T := NARROW(Strand.GetCurrent(), T);
  BEGIN
    IF th # NIL THEN RETURN th.writer; ELSE RETURN NIL; END;
  END GetWrSelf;
    
PROCEDURE GetTracker(th: T): TrackStrand.T =
  BEGIN
    RETURN th.track;
  END GetTracker;

(*--------------------------------------------- exception handling support --*)

PROCEDURE GetCurrentHandlers (): ADDRESS =
  BEGIN
    RETURN RTThread.handlerStack;
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers (h: ADDRESS) =
  BEGIN
    RTThread.handlerStack := h;
  END SetCurrentHandlers;

PROCEDURE PushEFrame (frame: ADDRESS) =
  TYPE Frame = UNTRACED REF RECORD next: ADDRESS END;
  VAR f := LOOPHOLE (frame, Frame);
  BEGIN
    f.next := RTThread.handlerStack;
    RTThread.handlerStack := f;
  END PushEFrame;

PROCEDURE PopEFrame (frame: ADDRESS) =
  BEGIN
    RTThread.handlerStack := frame;
  END PopEFrame;

(*-------------------------------------------------------------------- ??? --*)

PROCEDURE WakeMeUp(arg: REFANY) =
  VAR
    thread: Strand.T;
  BEGIN
    thread := NARROW(arg, Strand.T);
    Strand.Unblock(thread);
  END WakeMeUp;

(* n is in usecs *)
PROCEDURE Pause(n: INTEGER) =
  VAR
    me: Strand.T;
  BEGIN
    me := Strand.GetCurrent();
    Clock.SetAlarm(n * 1024 (* BUG Machine.Hertz() *) DIV 1000000,
                   WakeMeUp,
                   me);
    Strand.Block(me);
  END Pause;

(* n is in usecs *)
(* SMURT
PROCEDURE PauseUsecs(n: INTEGER) =
  BEGIN
    Pause(n);
  END PauseUsecs;
*)

(* n is in usecs *)
PROCEDURE AlertPause(n: INTEGER) RAISES {Alerted} =
  BEGIN
    Pause(n);
    (* XXX Alert implementation *)
    IF FALSE THEN RAISE Alerted; END;
  END AlertPause;

PROCEDURE GetBoundUserStrand(th: T) : Strand.T =
  BEGIN
    RETURN th.bound_to_user;
  END GetBoundUserStrand;

(*------------------------------------------------------- GC support ---*)
PROCEDURE SuspendOthers() =
  BEGIN
    INC(inCritical);
  END SuspendOthers;

PROCEDURE ResumeOthers() =
  BEGIN
    DEC(inCritical);
  END ResumeOthers;

(* find all active stacks and saved registers *)
(* It is illegal to do any sort of allocation in this routine *)
(* This routine is executed with inCritical high, so it will not *)
(* be preempted. Accordingly, it must not block. *)
PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS(*; th: REFANY*))) =
  VAR
    allregs: CPU.GeneralRegs;      (* must be stack allocated *)
    prev, next, th, curr: T;
    stack_bottom: Word.T;
  BEGIN
    <* ASSERT RT0u.inCritical # 0 *>
    (* save registers onto the current stack *)
    EVAL CPUExtern.SaveAllGeneralRegs(allregs);

    (* this allows collections to occur before threads are initialized *)
    IF activeThreads = NIL THEN
      p(LOOPHOLE(allregs.ksp, ADDRESS), SalExtern.boot_stack_bottom);
      RETURN;
    END;

    curr := NARROW(Strand.GetCurrent(), T);

    (* save registers onto the current stack *)
    EVAL CPUExtern.SaveAllGeneralRegs(allregs);

    (*
     * Take dead threads out of the activethreads list.
     *)
    th := activeThreads^;
    WHILE th # NIL DO
      next := th.gcnext;
      IF th.state = State.Dead THEN
        <* ASSERT th # curr *>
        (*
         * This thread is dead, take it out of the activeThreads list.
         *
         * The modifications here to activeThreads don't need to be done
         * with compare and swap because this code executes atomically
         * with inCritical held.
         *)
        IF th = activeThreads^ THEN
          activeThreads^ := th.gcnext;
        ELSE
          prev.gcnext := th.gcnext;
        END;
        th.gcnext := NIL;
      ELSE
        prev := th;
      END;
      th := next;
    END;
    IF ThreadPrivate.BootStackTop # 0 THEN
      (*
       * We have started the system and are in multithreaded mode.
       *)
      (* process the current stack *)
      stack_bottom := Word.Plus(LOOPHOLE(ADR(curr.stack.base[0]), Word.T),
                                ThreadPrivate.StackSize);

      IF stack_bottom < allregs.ksp OR
        stack_bottom > allregs.ksp + ThreadPrivate.StackSize
       THEN
        RTIO.PutText("ERROR >> stack scan during context switch\n");
        RTOS.Crash();
      END;

      p(LOOPHOLE(allregs.ksp, ADDRESS), LOOPHOLE(stack_bottom, ADDRESS));

      (* process the bootstrap stack for remaining refs to M3 *)
      p(LOOPHOLE(ThreadPrivate.BootStackTop,ADDRESS),
	      SalExtern.boot_stack_bottom);
    ELSE
      (*
       * We are still on the bootstrap stack, process it
       *)
      p(LOOPHOLE(allregs.ksp, ADDRESS), SalExtern.boot_stack_bottom);
    END;

    th := activeThreads^;
    WHILE th # NIL DO
      (*
       * Dead threads must be trimmed out of activethreads by now.
       *)
      <* ASSERT th.state # State.Dead *>
      IF th.stack # NIL AND th.ms.ksp # 0 AND th # curr THEN
        (*
         * Figure out the stack range for ambiguous roots.
         *)
        stack_bottom := Word.Plus(LOOPHOLE(ADR(th.stack.base[0]), Word.T),
                               ThreadPrivate.StackSize);
        p(LOOPHOLE(th.ms.ksp, ADDRESS), LOOPHOLE(stack_bottom, ADDRESS));

        (*
         * We don't have to explicitly scan the registers, since the
         * registers are stored on the stack and the scan
         * of the stack includes them. Should the machine state be
         * stored somewhere other than the stack, add the following 
         * code in: p(th.ms, th.ms + ADRSIZE(th.ms^));
         *)
      END;
      th := th.gcnext;
    END;
  END ProcessStacks;

(* Clean out dead threads from the activeThreads list.  In order to
   support real-time collection, an integer n indicates the maximum
   number of threads that can be processed.  TRUE is returned if
   all dead threads were dequeued *)

PROCEDURE RemoveDeadThreads (n : INTEGER) : BOOLEAN =
  VAR
    prev, next, th : T;
    threadsProcessed := 0;
  BEGIN
    (*
     * Take dead threads out of the activethreads list.
     *)
    th := activeThreads^;
    WHILE th # NIL AND threadsProcessed < n DO
      next := th.gcnext;
      IF th.state = State.Dead THEN
        (*
         * This thread is dead, take it out of the activeThreads list.
         *
         * The modifications here to activeThreads don't need to be done
         * with compare and swap because this code executes atomically
         * with inCritical held.
         *)
        IF th = activeThreads^ THEN
          activeThreads^ := th.gcnext;
        ELSE
          prev.gcnext := th.gcnext;
        END;
        th.gcnext := NIL;
      ELSE
        prev := th;
      END;
      th := next;
      INC(threadsProcessed);
    END;

    (* if we reached end of list, then we're done *)
    RETURN th = NIL;
  END RemoveDeadThreads;

(* FIXME: can do without SPL *)
PROCEDURE IsOnCurrentStack (addr: ADDRESS): BOOLEAN =
  VAR
    spl: CPU.InterruptLevel;
    allregs: CPU.GeneralRegs;      (* must be stack allocated *)
    th: T;
    stack_bottom: Word.T;
    res: BOOLEAN;
    ptr := LOOPHOLE(addr, Word.T);
  BEGIN
    spl := CPUPrivate.SetInterruptMask(
               CPUPrivate.InterruptClass.High);
    th := NARROW(Strand.GetCurrent(), T);

    (* save registers onto the current stack *)
    EVAL CPUExtern.SaveAllGeneralRegs(allregs);

    IF ThreadPrivate.BootStackTop # 0 THEN
      (*
       * We have started the system and are in multithreaded mode.
       * process the current stack
       *)
      stack_bottom := Word.Plus(
                          LOOPHOLE(ADR(th.stack.base[0]), Word.T),
                          ThreadPrivate.StackSize);
    ELSE
      (*
       * We are still on the bootstrap stack, process it
       *)
      stack_bottom := LOOPHOLE(SalExtern.boot_stack_bottom, Word.T);
    END;

    IF stack_bottom < allregs.ksp OR
      stack_bottom > allregs.ksp + ThreadPrivate.StackSize
     THEN
      (* We are trying to check the write in the middle of context switch;
         the stack information is out of synch with current thread during
         that period, so just check all the possible stacks.  We assume
         that only a write to a current stack could possibly happen.
         FIXME:  this is very expensive and should be replaced either
         with more accurate information given by the scheduler or with
         conservative assumption that this is NOT a stack *)
      th := activeThreads^;
      WHILE th # NIL DO
        IF th.state # State.Dead AND th.stack # NIL AND th.ms.ksp # 0 THEN
          stack_bottom := Word.Plus(LOOPHOLE(ADR(th.stack.base[0]), Word.T),
                                    StackSize);
          IF LOOPHOLE(th.ms.ksp, ADDRESS) <= addr AND
            addr <= LOOPHOLE(stack_bottom, ADDRESS)
           THEN
            res := TRUE;
            EXIT;
          END;
        END;
        th := th.gcnext;
      END;
    ELSE
      res := allregs.ksp <= ptr AND ptr <= stack_bottom;
    END;
    CPUPrivate.RestoreInterruptMask(spl);
    RETURN res;
  END IsOnCurrentStack;

PROCEDURE Idle(<*UNUSED*>arg: ArgT) : ResultT =
  BEGIN
    LOOP
      Strand.Yield();
    END;
  END Idle;
   
PROCEDURE GetIdleTime() : INTEGER =
  BEGIN

    RETURN idleTime;
  END GetIdleTime; 

   
PROCEDURE SetIdleTime(i : INTEGER) =
  BEGIN
    idleTime := i;
  END SetIdleTime; 

(*
 * EVENT MANAGEMENT 
 *)

PROCEDURE InstallHandlers () =
  BEGIN
    TRY
      (* The Thread RunHandler does not return, so the Cancel option is
	      redundant but helps optimal dispatch *)
      WITH options = Dispatcher.Options{
                       Dispatcher.Opt.First , Dispatcher.Opt.Cancel } DO
        IF ProfileSupport.CompiledIn THEN
          EVAL Dispatcher.InstallHandler(
                   Strand.Run, MyStrand, ProfilingRunHandler, 
                   options := options);
        ELSE
          EVAL Dispatcher.InstallHandler(
                   Strand.Run, MyStrand, RunHandler, options := options);
        END;

        IF DebugOption.DoTrackStrand THEN 
          (* TrackStrand.ContextSwitch is installed here for correct ordering *)
          EVAL Dispatcher.InstallHandler(Strand.Run,NIL,TrackStrand.ContextSwitch,
                                         options:= Dispatcher.Options{Dispatcher.Opt.First});
        END;

        EVAL Dispatcher.InstallHandler(
               Strand.Stop, MyStrand, StopHandler, options := options);
        EVAL
          Dispatcher.InstallHandler(Strand.RaiseException, MyExceptionStrand,
                                    ExceptionHandler, options := options);
      END;
    EXCEPT
    ELSE
      IO.PutError("Thread:  handler installation failed\n");
    END;

  END InstallHandlers;

PROCEDURE Init(verbose: BOOLEAN) =
  VAR
    t : T;
  BEGIN
    (*
     * no need to lock here because we are single threaded.
     *)
    deadSema    := Sema.Alloc(0);
    activeThreads := NEW(REF REFANY);
    activeThreads^ := NIL;
    freeStacks  := NEW(REF FastList.T);
    deadThreads := NEW(REF FastList.T);
    freeThreads := NEW(REF FastList.T);

    InstallHandlers();

    IF verbose THEN
       IO.Put("Sal Threads initialized...\n");
       IO.Put("Thread init - Starting the idle thread...\n");
    END;
    idleT := PFork(Idle, NIL, 0);
    idleTime := 0;
    IF DebugOption.DoTrackStrand THEN 
      TrackStrand.SetName(GetTracker(idleT),"IdleThread");
    END;

    IF verbose THEN
       IO.Put("Thread init - Starting collector...\n");
    END;
    t := PFork(Collector, NIL, defaultPriority+1);
    IF DebugOption.DoTrackStrand THEN 
      TrackStrand.SetName(GetTracker(t),"ThreadReaper");
    END;
  END Init;

BEGIN
END Thread.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * The scheduler is a trusted service. It can manipulate kernel state and
 * diddle with thread identities.
 *
 *
 * HISTORY
 *
 * 03-Feb-98  Robert Grimm (rgrimm) at the University of Washington
 *      Fixed bug related to switching off security.
 * 
 * 09-Dec-97  David Becker at the University of Washington
 *      Unblock checks for dead threads.
 *
 * 03-Dec-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added support for DebugOption.Security.
 *
 * 01-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed security code to fit into new security manager
 *
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Kernel and CPU interfaces
 *
 * 19-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Do not use concatenation or Textify in logging because the
 *	allocate traced memory.
 *
 * 25-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made TrackStrand conditional.
 *
 * 10-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity and added initialization for Taz
 *
 * 22-Nov-96  becker at the University of Washington
 *	Set name of PriorityAdjuster thread
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt management and cleanup of architecture
 *      dependencies.
 *
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Name clean up for code region.
 *
 * 13-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Use Textify.
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	AnyReady works fine even if called before the scheduler
 *	has been initialized. Whoever is doing this should be shot.
 *
 * 06-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Use console device interface.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Fixed-priority global scheduler.
 *
 *)
UNSAFE MODULE Sched EXPORTS Sched, SchedPrivate, StrandExtern, Strand;
IMPORT Strand, StrandRep, CPU, CPUPrivate;
IMPORT Sema, Thread, ThreadRep, ThreadExtra, ThreadPrivate;
IMPORT DebugOption, Log, Debugger;
IMPORT RT0u, KernelRegions;
IMPORT BootIO;
IMPORT IO;
IMPORT SecurityContext;
IMPORT TrackStrand; <* NOWARN *>

VAR Runq: Strand.T := NIL;
    Dummy: Strand.T := NIL;
    strandcount: INTEGER := 0;

VAR vticks: INTEGER := 0; (* virtual ticks for a thread, counts down from *)
                          (* thread's quantum *)

CONST
  Quanta = 5; (* in ticks *)

CONST
  DoInternalCheck = FALSE;

(* Check that the priority queue order is not violated *)
PROCEDURE Consistent() : BOOLEAN =
  VAR prev, cur: Strand.T;
  BEGIN
    prev := Runq;
    cur := Runq.next;
    WHILE cur # Runq DO
      IF prev.pri < cur.pri THEN
        Log.Log("Priority mismatch between ");
        Log.Logi(prev.pri); Log.Log(" and ");
        Log.Logi(cur.pri);  Log.Log(" Runq=0x");
        Log.LogRef(Runq); Log.Log("\n");
        RETURN FALSE;
      ELSE
        prev := cur;
        cur := cur.next;
      END;
    END;
    RETURN TRUE;
  END Consistent;

PROCEDURE AddIntoQ(VAR s: T) =
  VAR
    p : Strand.T;
  BEGIN
    IF DoInternalCheck AND NOT Consistent() THEN Dump(); Debugger.Enter() END;
    p := Runq;
    IF p.pri > s.pri THEN
      p := p.next;
      WHILE p.pri > s.pri AND p # Runq DO
          p := p.next;
      END;
    END;
    s.next := p;
    s.prev := p.prev;
    p.prev.next := s;
    p.prev := s;
    IF p = Runq THEN 
      Runq := s;
    END;
    IF DoInternalCheck AND NOT Consistent() THEN Dump(); Debugger.Enter() END;
  END AddIntoQ;

PROCEDURE ReplaceInQ(VAR tobereplaced, replacement: T) =
  BEGIN
    IF DoInternalCheck AND NOT Consistent() THEN Dump(); Debugger.Enter() END;
    replacement.next := tobereplaced.next;
    replacement.prev := tobereplaced.prev;
    tobereplaced.prev.next := replacement;
    tobereplaced.next.prev := replacement;
  END ReplaceInQ; 

PROCEDURE DeleteFromQ(VAR s: T) : BOOLEAN =
  VAR washead: BOOLEAN := FALSE;
  BEGIN
    IF DoInternalCheck AND NOT Consistent() THEN Dump(); Debugger.Enter() END;
    s.prev.next := s.next;
    s.next.prev := s.prev;
    IF s = Runq THEN washead := TRUE; Runq := s.next; END;
    IF DoInternalCheck AND NOT Consistent() THEN Dump(); Debugger.Enter() END;
    RETURN washead;
  END DeleteFromQ; 

PROCEDURE FindNext() : T =
  BEGIN
    vticks := Quanta;
    IF Runq.pri > Cur.pri THEN 
      RETURN Runq;
    ELSIF Cur.next.pri = Cur.pri THEN
      RETURN Cur.next;
    ELSE
      RETURN Runq;
    END;
  END FindNext;

PROCEDURE ClockTick(VAR ss: CPU.SavedState) =
  BEGIN
    DEC(vticks);
    INC(gticks);
    PreemptIfNeeded(ss);
  END ClockTick;

PROCEDURE PreemptIfNeeded(VAR ss: CPU.SavedState) =
  VAR
    spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    (*
     * We preempt if the quanta for this thread has expired and there is
     * somebody else of equal or higher priority who is runnable, or
     * if a higher priority strand is runnable.
     *)
    IF (vticks <= 0
	(* any ready thread that can replace me?*)
	AND (Cur.pri = Cur.next.pri
	     OR Cur # Runq))
      OR Runq.pri > Cur.pri THEN
      (*
       * We decided to preempt. Check to make sure it is safe to do so.
       * The runtime should not be locked, we don't preempt other interrupt
       * handlers, we respect the preemption flags, and we adjust the pc
       * according to RAS regions.
       *)
      IF RT0u.inCritical = 0 AND
        CPUPrivate.IsLow(CPU.ExtractInterruptLevel(ss)) AND
        (kernelPreemptive AND KernelRegions.InKernelLand(ss.pc) AND
         NOT KernelRegions.NonPreemptibleRegions.contains(ss.pc))
        OR 
        (userPreemptive AND KernelRegions.InUserLand(ss.pc)) THEN
        IF DebugOption.Cswtch THEN
          Log.Logi(gticks);
          Log.Log(" Preempted at 0x"); Log.Logx(ss.pc);
          (*Log.Log(" ra=0x"); Log.Logx(ss.ra);*)
          Log.Log("\n");
        END;
        (*
         * Do the preemption.
         *)
        IF Strand.Stop(Cur) THEN
          Next();
        END;
      ELSE
        (*
         * We could not preempt the thread even though a higher priority
         * thread is runnable. Mark it as having exceeded its quanta.
         *)
        vticks := 0;
      END;
    ELSE
      CPUPrivate.RestoreInterruptMask(spl);
    END;
  END PreemptIfNeeded;

PROCEDURE Block(s: Strand.T) =
  VAR spl: CPU.InterruptLevel;
      picknext : BOOLEAN := FALSE;
      washead: BOOLEAN;
  BEGIN 
    IF DebugOption.Sched THEN Dprint(s, "Blocking"); END;
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    DEC(s.count);
    IF s.count = 0 THEN
      IF s.bound_to_user # NIL THEN
        (*
         * dequeue the user level thread on whose behalf he is in the
         * kernel.
         *)
        IF DebugOption.Sched THEN Log.Log("\tblocking user thread.\n"); END;
        Block(s.bound_to_user);
        CPUPrivate.RestoreInterruptMask(spl);
        RETURN;
      ELSE
        IF DebugOption.Sched THEN Log.Log("\tunqueuing the strand.\n"); END;
        washead := DeleteFromQ(s);
        IF s.bound_to_kernel # NIL AND s.bound_to_kernel.count > 0 THEN
          (*
           * we have to requeue the kernel strand instead of the 
           * user strand here.
           *)
          WITH k = s.bound_to_kernel DO
            IF Cur = s THEN
              IF DebugOption.Sched THEN 
                Log.Log("\treplaced ");
                Log.LogRef(s);
                Log.Log(" with ");
                Log.LogRef(k);
                Log.Log("\n");
              END;
              ReplaceInQ(s, k);
              k.pri := s.pri;
              Cur := k;
              IF washead THEN Runq := k; END;
              IF DoInternalCheck AND NOT Consistent() THEN Dump();
                                                           Debugger.Enter() END;
            ELSE
              AddIntoQ(k);
            END;
          END;
        ELSE
          DEC(strandcount);
          picknext := Cur = s;
        END;
      END;
      (*
       * see if it needs to be stopped and pick a new guy to run
       *)
      IF picknext THEN
        IF DebugOption.Sched THEN Log.Log("\tpicking next.\n"); END;
        IF TYPECODE(Cur) = TYPECODE(Thread.T) AND 
	  LOOPHOLE(Cur, Thread.T).state = ThreadPrivate.State.ReadyToDie THEN
	  LOOPHOLE(Cur, Thread.T).state := ThreadPrivate.State.Dead;
          IF DebugOption.Security THEN
            LOOPHOLE(Cur, Thread.T).sid := SecurityContext.currentSid;
          END;
          (*
             Store currentSid back into thread state,
             to make sure sid storage is properly reclaimed.
           *)
          Cur := FindNext();
          Strand.Run(Cur);
        ELSIF Strand.Stop(Cur) THEN
          Cur := FindNext();
          Strand.Run(Cur);
        END;
      END;
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END Block;

PROCEDURE Unblock(s: Strand.T) =
  VAR 
    spl: CPU.InterruptLevel;
  BEGIN

    IF TYPECODE(s) = TYPECODE(Thread.T)
         AND NARROW(s, Thread.T).state = ThreadPrivate.State.Dead THEN
       IO.Put("Unblocking dead thread\n");
       Debugger.Enter();
       RETURN;
    END;

    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    IF DebugOption.Sched THEN Dprint(s, "Unblocking"); END;
    INC(s.count);
    IF s.count = 1 THEN
      IF s.bound_to_user # NIL THEN
        AddIntoQ(s.bound_to_user);
        INC(s.bound_to_user.count);
      ELSE
        AddIntoQ(s);
      END;
      INC(strandcount);
    END;
    IF DebugOption.Sched THEN Dump(); END;
    CPUPrivate.RestoreInterruptMask(spl);
  END Unblock;

PROCEDURE Yield() =
  VAR spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);

    IF Strand.Stop(Cur) THEN
      Cur := FindNext();
      Strand.Run(Cur);
    END;
  END Yield;

FUNCTIONAL PROCEDURE GetCurrent (): Strand.T =
  VAR cur: Strand.T;
  BEGIN
    cur := Cur;
    IF cur # NIL THEN
      IF cur.bound_to_kernel # NIL THEN cur := cur.bound_to_kernel; END;
    END;
    RETURN cur;
  END GetCurrent;

FUNCTIONAL PROCEDURE GetCurrentUserStrand (): Strand.T =
  BEGIN
    IF Cur # NIL THEN
      IF Cur.bound_to_kernel # NIL THEN RETURN Cur;
      ELSE RETURN Cur.bound_to_user; END;
    END;
    RETURN NIL;
  END GetCurrentUserStrand;

PROCEDURE GetRunqHead() : Strand.T =
  BEGIN
    RETURN Cur;
  END GetRunqHead; 

(* assumes it is called at splhigh *)
PROCEDURE Next() =
  BEGIN
    IF TYPECODE(Cur) = TYPECODE(Thread.T) THEN
      (*
       * If we have preempted a kernel thread, respect the RAS regions before
       * making it run.
       *)
      KernelRegions.RASRegions.rasEnsure(LOOPHOLE(Cur, Thread.T).ms.pc);
    END;
    Cur := FindNext();
    Strand.Run(Cur);
  END Next;

PROCEDURE Dprint(s: T; text: TEXT) =
  BEGIN
    Log.Log(text);
    Log.Log(" a thread(");
    Log.Log("type=");    Log.Logi(TYPECODE(s));
    Log.Log(",0x");      Log.LogRef(s);
    Log.Log(",scount="); Log.Logi(s.count);
    Log.Log(",pri=");    Log.Logi(s.pri);
    Log.Log(")");
    IF s.bound_to_kernel # NIL THEN
      Log.Log("bound to a kernel thread\n");
    ELSIF s.bound_to_user # NIL THEN
      Log.Log("bound to a user thread\n");
    ELSE
      Log.Log("\n");
    END;
  END Dprint;

PROCEDURE PrintInfo(p: Strand.T) =
  BEGIN
    IF Cur = p THEN Log.Log(">"); END;
    Log.Log("\t0x");     Log.LogRef(p);
    Log.Log("(type=");   Log.Logi(TYPECODE(p));
    Log.Log(",scount="); Log.Logi(p.count);
    Log.Log(",pri=");    Log.Logi(p.pri); Log.Log(")");
    Log.Log(" prev=0x"); Log.LogRef(p.prev);
    Log.Log(" next=0x"); Log.LogRef(p.next);
    Log.Log("\n");
  END PrintInfo;

PROCEDURE Dump() =
  VAR p: Strand.T;
  BEGIN
    Log.Log("strandcount = ");
    Log.Logi(strandcount);
    Log.Log("\n");
    p := Runq;
    PrintInfo(p);
    p := p.next;
    WHILE p # Runq DO
      PrintInfo(p);
      p := p.next;
    END;
  END Dump;

VAR
  priadjust, priadjustdone: Sema.T;
  newpri: Strand.PriorityT;
  pristrand: Strand.T;
  priadjustlock: MUTEX;

PROCEDURE PriorityAdjuster(<*UNUSED*> arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  BEGIN
    LOOP
      Sema.P(priadjust);
      Block(pristrand);
      pristrand.pri := newpri;
      Unblock(pristrand);
      Sema.V(priadjustdone);
    END;
  END PriorityAdjuster;

PROCEDURE SetPriority(s: Strand.T; pri: Strand.PriorityT) =
  BEGIN
    LOCK priadjustlock DO
      pristrand := s;
      newpri := pri;
      Sema.V(priadjust);
      Sema.P(priadjustdone);
    END;
  END SetPriority;

PROCEDURE GetPriority(s: Strand.T) : Strand.PriorityT =
  BEGIN
    RETURN s.pri;
  END GetPriority;

PROCEDURE Start() = 
  VAR
    t: Thread.T;
  BEGIN
    priadjust := Sema.Alloc(0);
    priadjustdone := Sema.Alloc(0);
    priadjustlock := NEW(MUTEX);
    t := ThreadExtra.PFork(PriorityAdjuster, NIL);
    IF DebugOption.DoTrackStrand THEN
      TrackStrand.SetName(ThreadExtra.GetTracker(t),"PriorityAdjuster");
    END;
    (* we are single-threaded, so no need to lock *)
    Cur := Runq;
    IO.Put("Scheduling started...transfering to SPIN stack\n");
    (* capture the stack pointer before switching to M3 threads *)
    ThreadPrivate.BootStackTop := CPU.CurrentStackPointer();
    Strand.Run(Cur);
  END Start;

PROCEDURE Init(verbose: BOOLEAN) =
  BEGIN
    Dummy := NEW(Thread.T);
    Dummy.pri := -1;
    Dummy.prev := Dummy;
    Dummy.next := Dummy;
    Runq := Dummy;
    Cur  := Dummy;
    WITH t = NARROW(Dummy, Thread.T) DO
      t.reader := BootIO.Reader();
      t.writer := BootIO.Writer();
    END;
    (* All initial strands are inherited from Dummy.
     * Therefore, we set the per-user security context
     * and the subject and object SID stack of Dummy here.
     * This call must not be domain exported or used anywhere else!
     * Look in auth/SecurityContext.i3 and auth/SecurityManager.m3
     *)
    SecurityContext.Tazify(Dummy);
    IF verbose THEN 
      IO.Put("Global scheduler initialized...\n"); 
    END; 
  END Init;

BEGIN
END Sched.

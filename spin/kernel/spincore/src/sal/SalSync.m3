(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 17-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Added stopgap fix by Becker to Sleep().  THIS MUST BE FIXED.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Renamed ThreadForSAL to SalSync
 *	This module provides sync services for underlying Sal code.
 *
 * 10-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added a cache for eventTable. Streamlined the critical path.
 * 25-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made TrackStrand conditional.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Change to TrackStrand from Spy
 *
 * 05-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made SalThread return result, this was expected in kern_malloc.c
 *
 * 30-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added an event into the event table if wakeup 
 *	occurs before the sleep. This should not happen,
 *	and it will obscure more fundamental races, but
 *	we are too lazy to find the actual cause of the problem.
 *
 * 07-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added a last event cache to avoid allocation of semaphores for
 *	event zero and to support fast lookup.
 *
 * 01-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed the implementation to forget the asserted wait event
 *	after a clearwait, wakeup, wakeupone or timeout.
 *	Changed Block to mimic the braindead Mach semantics of doing
 *	a yield when no event has been asserted.
 *
 * 15-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added WakeupOne. Changed Wakeup to awaken all waiting threads.
 *
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use semaphores for events instead of cond. variables.
 *      Got rid of use of OKtoAllocatSPL.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Switched to the simplified StrongRef interface.
 *
 * 25-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to pass StrongRef'ed strands.
 *      Changed the mutex operations to be GC-safe, by encapsulating
 *      a traced strongrefed mutex in a wrapper structure. The strongref
 *      handle is kept in the structure and is used to deallocate it.
 *
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. In-Sal thread support for the Mach/OSF Sal thread
 *      synchronization interface.
 *
 *) 
UNSAFE (* This module is unsafe because it imports externs. *)
MODULE SalSync;
IMPORT Thread, ThreadExtra, ThreadPrivate, ThreadRep, MachineThread, Sema;
IMPORT Strand, StrandRep, SemaRep, Clock, CPUPrivate;
IMPORT Word, IntSemaTbl;
IMPORT StrongRef;
IMPORT IO, Fmt;
IMPORT DebugOption, TrackStrand; <*NOWARN*>
IMPORT Debugger; <*NOWARN*>
(*
 * Mach synchronization support. Tightly integrated with threads.
 *
 * This is actually a scheduling and synchronization interface all
 * mixed together. Not clear why they didn't choose synchronization
 * primitives that are well understood by everyone in the field and
 * can be found in text books.
 *)

(*
 * Mach event table to be used for Mach support.
 *)

VAR
  eventTable: IntSemaTbl.Default;
  cache: ARRAY [0 .. 1] OF RECORD sema: Sema.T; event: EventT; END;
  (* a direct mapped cache for eventTable entries. *)
  
TYPE
  CFuncClosure = Thread.Closure OBJECT
    cfunc: PROCEDURE(carg: ADDRESS);
    carg : ADDRESS;
  OVERRIDES
    apply := RunCFunction;
  END;

REVEAL CSideMutex = BRANDED "CSideMutex" REF RECORD
    mutex: MUTEX;
  END;

PROCEDURE RunCFunction(self: CFuncClosure) : REFANY =
  BEGIN
    self.cfunc(self.carg);
    RETURN NIL;
  END RunCFunction;

PROCEDURE KernelThread(cfunc: PROCEDURE(carg: ADDRESS);
                       carg: ADDRESS): Thread.T =
  VAR
    c: CFuncClosure;
    t: Thread.T;
  BEGIN
    c := NEW(CFuncClosure);
    c.cfunc := cfunc;
    c.carg := carg;
    t := Thread.Fork(c);
    IF DebugOption.DoTrackStrand THEN
      TrackStrand.SetName(ThreadExtra.GetTracker(t),
                          "SAL_"&Fmt.Int(ThreadExtra.GetId(t)));
    END;
    RETURN t;
  END KernelThread;

PROCEDURE CurrentThread() : Thread.T =
  VAR s: Thread.T;
  BEGIN
    s := NARROW(Strand.GetCurrent(), Thread.T);
    (*
     * We cannot strongref the strand here because we don't know
     * when to unstrongref things. So we rely on the fact that the
     * thread descriptor is pinned in memory throughout its lifetime
     * due to an ambiguous reference on its own stack.
     *)
    RETURN s;
  END CurrentThread;

(*
 * We assume that calls to block are preceded by calls to assert_wait
 * with valid event handles.
 *)
PROCEDURE BlockWithContinuation(continuation: ThreadExtra.FuncT; arg: ThreadExtra.ArgT) =
  VAR
    thread: Thread.T;
    newsp: Word.T;
  BEGIN
    (*
     * Continuation support for OSF thread primitives.
     * We don't reuse stacks here, because that involves
     * writing an assembler routine. This interface is
     * here solely to support SAL and CAM.
     *)
    thread := NARROW(Strand.GetCurrent(), Thread.T);
    IF thread.wait_event # NIL THEN
      Sema.P(thread.wait_event);
    ELSE
      (*
       * Argh again. The Mach folks went off the deep end when writing
       * synch primitives. Block is actually Yield if the thread has not
       * done an assertwait immediately before the block. There is no
       * rhyme or reason to any of this, except that we have to emulate 
       * a braindead synchronization primitive.
       *)
      ThreadExtra.Yield();
    END;
    IF continuation # NIL THEN
      newsp := MachineThread.InitialSP(thread.stack);
      thread.func := continuation;
      thread.arg := arg;
      MachineThread.CallContinuation(thread, ThreadPrivate.KThreadBody, newsp);
      (* not reached *)
    END;
  END BlockWithContinuation;

PROCEDURE Sleep() =
  VAR thread: Thread.T;
  BEGIN
    thread := NARROW(Strand.GetCurrent(), Thread.T);
    Sema.P(thread.wait_event);
    (* FIXME *)    (* FIXME *)    (* FIXME *)
    EVAL Clock.CancelAlarm(Timeout, thread);
  END Sleep;

PROCEDURE Timeout(arg: REFANY) = 
  VAR s: Thread.T;
  BEGIN 
    s := NARROW(arg, Thread.T);
    ClearWait(s);
  END Timeout;

(*
 * Assumes that AssertWait has been called already
 *)
PROCEDURE SetTimeout(thread: Thread.T; timeout: Word.T) =
  BEGIN
    Clock.SetAlarm(timeout, Timeout, thread);
  END SetTimeout;


PROCEDURE AssertWait(eventhandle: EventT) =
  VAR
    thread := NARROW(Strand.GetCurrent(), Thread.T);
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
  BEGIN
    (* no need to lock the thread because we can only operate 
       on ourselves *)

    IF eventhandle # 0 THEN
      IF cache[0].event = eventhandle THEN
	thread.wait_event := cache[0].sema;
      ELSIF cache[1].event = eventhandle THEN
	thread.wait_event := cache[1].sema;
      ELSE
	IF NOT eventTable.get(eventhandle, thread.wait_event) THEN
	  thread.wait_event := Sema.Alloc(0);
	  EVAL eventTable.put(eventhandle, thread.wait_event);
	END;
	WITH idx = Word.And(Word.RightShift(eventhandle, 4), 1) DO
	  cache[idx].event := eventhandle;
	  cache[idx].sema := thread.wait_event;
	END;
      END;
    ELSE
      IF thread.wait_eventcache # NIL THEN
	thread.wait_event := thread.wait_eventcache;
      ELSE
	thread.wait_event := Sema.Alloc(0);
	thread.wait_eventcache := thread.wait_event;
      END;
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END AssertWait;

PROCEDURE FindEventSema(eventhandle: EventT): Sema.T =
  VAR
    sema: Sema.T := NIL;
  BEGIN
    IF cache[0].event = eventhandle THEN
      RETURN cache[0].sema;
    ELSIF cache[1].event = eventhandle THEN
      RETURN cache[1].sema;
    ELSE
      IF eventhandle # 0 THEN
	EVAL eventTable.get(eventhandle, sema);
	(*
	   It is not an error to attempt to wakeup a
	   non existent sleeper.
	*)
      END;
      RETURN sema;
    END;
  END FindEventSema; 

(*
 * Argh, threadwakeup/assertwait/threadblock are the crappiest
 * synchronization primitives designed by apes. ThreadWakeup is
 * supposed to implicitly clear the wait_event that was asserted
 * by AssertWait. This means we cannot opaquely use semaphores
 * to implement it, and must instead go munging through a semaphore's
 * insides to get to the waiting thread list.
 *)
PROCEDURE ThreadWakeup(eventhandle: EventT) =
  VAR
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    sema := FindEventSema(eventhandle);
    th: Thread.T;
  BEGIN
    IF sema # NIL THEN
      th := sema.list;
      IF th # NIL THEN
        WHILE th # NIL DO
          th.wait_event := NIL;
          th := th.syncnext;
        END;
        Sema.Broadcast(sema); (* set sema.val to zero *)
      ELSE
        Sema.V(sema); (* incs sema.val *)
      END;
      CPUPrivate.RestoreInterruptMask(spl);
    ELSE
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END ThreadWakeup;

PROCEDURE ThreadWakeupOne(eventhandle: EventT) =
  VAR
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    sema := FindEventSema(eventhandle);
  BEGIN
    IF sema # NIL THEN
      IF sema.list # NIL THEN
        NARROW(sema.list, Thread.T).wait_event := NIL;
      END;
      Sema.V(sema);
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END ThreadWakeupOne;

PROCEDURE ClearWait(thread: Thread.T) =
  VAR sema: Sema.T;
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
  BEGIN
    IF thread.wait_event # NIL THEN
      sema := thread.wait_event;
      thread.wait_event := NIL;
      Sema.VThread(sema, thread);
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END ClearWait;

(* interfacing for C clients that need mutexes *)
PROCEDURE AllocateMutex() : CSideMutex =
  VAR
    mu: CSideMutex;
  BEGIN
    mu := NEW(CSideMutex);
    mu.mutex := NEW(MUTEX);
    StrongRef.Add(mu);
    RETURN mu;
  END AllocateMutex;

(* interfacing for C clients that need mutexes *)
PROCEDURE DeallocateMutex(mu : CSideMutex) =
  BEGIN
    StrongRef.Remove(mu);
  END DeallocateMutex; 

PROCEDURE AcquireMutex(mu: CSideMutex) =
  BEGIN
    Thread.Acquire(mu.mutex);
  END AcquireMutex;

PROCEDURE ReleaseMutex(mu: CSideMutex) =
  BEGIN
    Thread.Release(mu.mutex);
  END ReleaseMutex;

PROCEDURE Init(verbose: BOOLEAN) =
  BEGIN
    (*
     * no need to lock here because we are single threaded.
     *)
    eventTable := NEW(IntSemaTbl.Default).init();

    IF verbose THEN
       IO.Put("Thread Mach Support initialized...\n");
    END;
  END Init;

BEGIN
END SalSync.

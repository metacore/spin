(*
 * NOTE:
 *	THIS IS AN OLD VERSION OF THE FILE WHICH USES SPL-S FOR SYNCHRONIZATION
 *	IT HAS BEEN REVIVED BECAUSE THE NEW AND BETTER VERSION WHICH USES
 *	MUTEX-ES TRIGGERS SYNCHRONIZATION BUGS IN I/O (CAM AND SOCKETS).
 *	THIS FILE SHOULD BE REPLACED WITH THE NEW VERSION ONCE THE BUGS
 *	ARE FIXED.  THIS CAN BE DONE BY CHECKING OUT THE spin-30 VERSION
 *	OF THE FILE.
 *)

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Consolidated various TimeVal types and procs here.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Change to TrackStrand from Spy
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt handling.
 *
 * 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed locking to use SPL instead of global alarmlock.
 *
 * 27-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	XXX ADDED UGLY HACK TO SETTRUSTEDALARMS TO PREVENT US FROM BEING
 *	PREEMPTED. THIS NEEDS TO BE FIXED AND THIS WHIST ENTRY REMOVED.
 *
 * 03-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed minor buglet in SetTrustedAlarm when inserting an alarm in
 *	the middle of the alarm queue.  Just needed to decrement the
 *	delta of the next alarm in line.
 *
 * 27-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added a return value to cancelalarm to support sal.
 *
 * 16-Apr-96  Charles Garrett (garrett) at the University of Washington
 *	Initialize pcForDelayedTicks.
 *
 * 16-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Special case a delta of zero or less to execute immediately.
 *
 * 24-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added a new interface so C clients that take untraced arguments
 *      can use the clock service.
 *
 * 28-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Made it possible to specify alarms at absolute times in the future
 *	so clients do not have to spl.
 *
 * 27-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to execute callouts at low spl from a thread.
 *      Added interrupt level alarm service for trusted clients.
 *
 * 06-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed alarms list update after firing to check for no more alarms.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Alarm clock service.
 *
 *)

UNSAFE (* import CPUPrivate, an unsafe module *)
MODULE Clock EXPORTS Clock, ClockPrivate;
IMPORT Thread, ThreadExtra, SchedPrivate, Sema;
IMPORT CPU;
IMPORT Log, IO, TrackStrand;
IMPORT CPUPrivate;
IMPORT Word;

(* Alarms is a time sorted queue that holds the functions in the order
   they need to fire. The delta field holds the number of ticks after
   the previous alarms structure the given alarm should fire.  A set
   of fired alarms is executed without ordering contraints.

   There are two interfaces: the private interface supports trusted
   clients who want to run the alarms functions at high spl from clock
   interrupt.  The public interface is more generalized and is
   executed at low spl by the alarm thread. In all cases, application
   supplied functions need to be bounded. The compiler will check for
   this in the future. *)

TYPE Alarm = REF RECORD 
  delta   : INTEGER;
  trusted : BOOLEAN := FALSE; (* should the func execute w/ high spl ? *)
  func    : TimeoutFunc;      (* function to fire *)
  arg     : REFANY;           (* closure *)
  cfunc   : CTimeoutFunc;     (* C function to fire *)
  carg    : ADDRESS;          (* untraced argument *)
  next    : Alarm;
END;

VAR 
    alarms: Alarm := NIL;        (* Alarm queue of events in the future *)
    expiredalarms: Alarm := NIL; (* Alarms that have expired, but not yet *)
                                 (* executed by the alarm thread. *)
    ExpiredSema: Sema.T;         (* Sema that indicates if alarms have fired *)
    freealarms: Alarm := NIL;    (* Freelist to avoid allocation. *)
    AlarmLock: MUTEX;
    boundedTicks: INTEGER := 0;

CONST
  MaxPreallocatedAlarms = 20;
    
PROCEDURE ReadTicks() : INTEGER =
  BEGIN
    RETURN SchedPrivate.gticks;
  END ReadTicks;

(* XXX obsolete when we switch to using SPL protection
PROCEDURE TryLock(VAR spl: CPU.InterruptLevel):BOOLEAN = 
  BEGIN
    spl := CPU.GetInterruptLevel();
    RETURN Mutex.TryLock(AlarmLock);
  END TryLock;
 *)

PROCEDURE Lock(VAR spl: CPU.InterruptLevel) = 
  BEGIN
    (* XXX test case
    spl := CPU.GetInterruptLevel();
    IF ORD(spl) > 0 THEN
      IF NOT Mutex.TryLock(AlarmLock) THEN
        Log.Log("{spl="); Log.Logi(ORD(spl));
        Log.Log(" yielding}");
        Thread.Acquire(AlarmLock);
      END;
    ELSE
        Thread.Acquire(AlarmLock);
    END;
    *)
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
  END Lock;

PROCEDURE Unlock(spl: CPU.InterruptLevel) = 
  BEGIN
    (* XXX test case Thread.Release(AlarmLock); *)
    CPUPrivate.RestoreInterruptMask(spl);
  END Unlock; 

(*
 * Register a handler to go off delta ticks from now.
 *)
PROCEDURE SetTrustedAlarm(
    time     : INTEGER;
    func     : TimeoutFunc;
    arg      : REFANY;
    cfunc    : CTimeoutFunc;
    carg     : ADDRESS;
    trusted  : BOOLEAN := TRUE;
    absolute : BOOLEAN := FALSE;
    period   : INTEGER := 0) =
  VAR
    newalarm, cur: Alarm;
    delta: INTEGER;
  BEGIN
    <* ASSERT period = 0 *>
    IF absolute THEN
      delta := time - ReadTicks();
    ELSE
      delta := time;
    END;
    IF delta <= 0 AND NOT absolute THEN
      func(arg);
      RETURN;
    END;

    VAR
      spl: CPU.InterruptLevel;
    BEGIN
      TRY
        Lock(spl);

        IF freealarms # NIL THEN
          newalarm := freealarms;
          freealarms := freealarms.next;
        ELSE
          newalarm := NEW(Alarm);
        END;

        newalarm.trusted := trusted;
        newalarm.func := func;
        newalarm.arg := arg;
        newalarm.cfunc := cfunc;
        newalarm.carg := carg;

        IF alarms = NIL THEN  (* no one else is waiting, first in line *)
          newalarm.delta := delta;
          newalarm.next := alarms;
          alarms := newalarm;
        ELSIF alarms.delta >= delta THEN  (* first in line, others waiting *)
          alarms.delta := alarms.delta - delta;
          newalarm.delta := delta;
          newalarm.next := alarms;
          alarms := newalarm;
        ELSE                       (* in the middle or end of the queue *)
          cur := alarms;
          WHILE cur.next # NIL DO
            delta := delta - cur.delta;
            IF cur.next.delta > delta THEN EXIT END;
            cur := cur.next;
          END;
          IF cur.next # NIL THEN (* decrement delta of next in line *)
            cur.next.delta := cur.next.delta - delta;
          END;
          newalarm.delta := delta;
          newalarm.next := cur.next;
          cur.next := newalarm;
        END;
      FINALLY
        Unlock(spl);
      END;
    END;
  END SetTrustedAlarm;

PROCEDURE SetAlarm(
    time     : INTEGER; 
    func     : TimeoutFunc;
    arg      : REFANY;
    absolute : BOOLEAN := FALSE;
    period: INTEGER := 0) =
  BEGIN
    <* ASSERT period = 0 *>
    SetTrustedAlarm(time, func, arg, NIL, NIL, FALSE, absolute);
  END SetAlarm; 

PROCEDURE SetCAlarm(
    time     : INTEGER; 
    cfunc    : CTimeoutFunc;
    carg     : ADDRESS;
    absolute : BOOLEAN := FALSE;
    period: INTEGER := 0) =
  BEGIN
    <* ASSERT period = 0 *>
    SetTrustedAlarm(time, NIL, NIL, cfunc, carg, FALSE, absolute);
  END SetCAlarm; 

(*
 * Deregister an alarm, i.e untimeout.
 *)
PROCEDURE CancelAlarmImplementation(
    func  : TimeoutFunc;
    arg   : REFANY;
    cfunc : CTimeoutFunc;
    carg  : ADDRESS) : BOOLEAN =
  VAR prev, cur: Alarm;
  BEGIN
    VAR
      spl: CPU.InterruptLevel;
    BEGIN
      TRY
        Lock(spl);
        
        cur := alarms;
        prev := alarms;
        WHILE cur # NIL DO
          IF (cur.func = func) AND (cur.arg = arg) AND
            (cur.cfunc = cfunc) AND (cur.carg = carg)  THEN 
            BEGIN 
              IF cur = alarms THEN
                alarms := cur.next;
              ELSE
                prev.next := cur.next;
              END;
              cur.next := freealarms;
              freealarms := cur;
              RETURN TRUE;
            END;
          ELSE 
            prev := cur;
            cur := cur.next;
          END;
        END;
        RETURN FALSE;
      FINALLY
        Unlock(spl);
      END;
    END;
  END CancelAlarmImplementation;
  
PROCEDURE CancelAlarm(func: TimeoutFunc;
		      arg: REFANY) : BOOLEAN =
  BEGIN
    RETURN CancelAlarmImplementation(func, arg, NIL, NIL);
  END CancelAlarm;

PROCEDURE CancelCAlarm(cfunc: CTimeoutFunc;
		      carg: ADDRESS) : BOOLEAN =
  BEGIN
    RETURN CancelAlarmImplementation(NIL, NIL, cfunc, carg);
  END CancelCAlarm;


(*
 * Gets called on every clock tick at high spl.
 * Performs the registered and expired trusted alarms, if any.
 * It queues untrusted alarms for execution by the alarm thread
 * and signals the alarm thread, if necessary.
 *)
PROCEDURE ClockTick(<*UNUSED*>VAR ss: CPU.SavedState) = 
  VAR cur, todo: Alarm;
      signalthread: BOOLEAN := FALSE;
      func: TimeoutFunc;
      arg: REFANY;
      cfunc: CTimeoutFunc;
      carg: ADDRESS;
  BEGIN
    todo := NIL;

    (* XXX not needed
    VAR
      spl: CPU.InterruptLevel;
    BEGIN
    *)
      IF alarms # NIL (* XXX not anymore AND TryLock(spl) *) THEN
        alarms.delta := alarms.delta - 1;
        WHILE alarms # NIL AND alarms.delta <= 0 DO
          cur := alarms;
          alarms := alarms.next;
          IF cur.trusted THEN
            (* queue onto list of alarms to execute now *)
            cur.next := todo;
            todo := cur;
          ELSE
            cur.next := expiredalarms;
            expiredalarms := cur;
            signalthread := TRUE;
          END;
        END;
        (* XXX not anymore Unlock(spl);  *) (* matches TryLock above *)
        IF signalthread THEN
          Sema.V(ExpiredSema);
        END;
        WHILE todo # NIL DO
          cur := todo;
          todo := todo.next;
          (* save the relevant info in locals *)
          func := cur.func;
          arg  := cur.arg;
          cfunc := cur.cfunc;
          carg  := cur.carg;
          (* free the alarm struct *)
          cur.next := freealarms;
          freealarms := cur;
          (* call the alarm function *)
          IF  func # NIL THEN  func(arg); END;
          IF cfunc # NIL THEN cfunc(carg); END;
        END;
      END;
    (* XXX not anymore
    END;
    *)
    DEC(boundedTicks);
    IF boundedTicks = 0 THEN
      (* catastrophe. kill the thread that exceeded the time limit in *)
      (* handling this event. *)
      Log.Log("Bounded event expired.\n");
      (* ThreadPrivate.Kill(); *)
    END;
  END ClockTick; 

CONST DEBUG = FALSE;

(*
 * High priority thread that executes expired alarms at low spl.
 *)
PROCEDURE AlarmThread(<*UNUSED*>dummy: REFANY) : ThreadExtra.ResultT =
  VAR
    cur, todo: Alarm;
    func: TimeoutFunc;
    arg: REFANY;
    cfunc: CTimeoutFunc;
    carg: ADDRESS;
  BEGIN
    LOOP
      Sema.P(ExpiredSema);
      VAR
        spl: CPU.InterruptLevel;
      BEGIN
        TRY
          Lock(spl);

          todo := expiredalarms;
          expiredalarms := NIL;
        FINALLY
          Unlock(spl);
        END;
      END;

      WHILE todo # NIL DO
        cur := todo;
        todo := todo.next;
        (* save the relevant info in locals *)
        func := cur.func;
        arg  := cur.arg;
        cfunc:= cur.cfunc;
        carg := cur.carg;
        (* free the alarm struct *)
        cur.next := freealarms;
        freealarms := cur;
        (* call the alarm function *)
        (* we should fork this function off if we cannot be sure that *)
        (* it is bounded. For now, assume that it is and rely on the *)
        (* compiler to check it in the future. *)
        
        IF  func # NIL THEN
          IF DEBUG THEN 
            Log.Log("ATM3 ");
            Log.Logx(LOOPHOLE(func,Word.T));
            Log.Log("(");
          END;
          func(arg); 
          IF DEBUG THEN 
            Log.Log(")\n");
          END
        END;
        IF cfunc # NIL THEN 
          IF DEBUG THEN 
            Log.Log("ATC ");
            Log.Logx(LOOPHOLE(func,Word.T));
            Log.Log("(");
          END;
          cfunc(carg); 
          IF DEBUG THEN 
            Log.Log(")\n");
          END;
        END;
      END;
    END;
  END AlarmThread;

PROCEDURE BoundedBegin(nTicks: INTEGER) =
  BEGIN
    boundedTicks := nTicks;
  END BoundedBegin;

PROCEDURE BoundedEnd() =
  BEGIN
    boundedTicks := 0;
  END BoundedEnd;


PROCEDURE Init(verbose: BOOLEAN) =
  VAR
    a: Alarm;
    t : Thread.T;
  BEGIN
    pcForDelayedTicks := LOOPHOLE(CPUPrivate.RestoreInterruptMask, 
                                  INTEGER) + 4;

    AlarmLock := NEW(MUTEX);
    ExpiredSema := Sema.Alloc(0);
    (*
     * Preallocate a few alarm structs so CAM doesn't have
     * to call NEW at high spl.
     *)
    FOR i := 1 TO MaxPreallocatedAlarms DO
      a := NEW(Alarm);
      a.next := freealarms;
      freealarms := a;
    END;
    t:= ThreadExtra.PFork(AlarmThread, NIL, ThreadExtra.defaultPriority+1);
    TrackStrand.SetName(ThreadExtra.GetTracker(t),"AlarmThread");
    IF verbose THEN IO.Put("Alarm service initialized...\n"); END;
  END Init;

BEGIN
END Clock.

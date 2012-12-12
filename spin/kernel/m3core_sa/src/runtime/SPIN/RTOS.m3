(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Nov 18 16:00:55 PST 1994 by kalsow     *)
(*      modified on Wed Jan 27 22:49:37 PST 1993 by mjordan    *)
(*
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added a spy to measure the pause time.
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Changed to new interrupt scheme including both classes and levels.
 *
 * 17-Feb-96 Przemek Pardyak (pardy) at the University of Washington
 *	Measurements by Spy used only if RTCollectorSRC.DoTimings is on.
 *	Until our memory problems are fixed, locking of the traced heap
 *	disables all interrupts.
 *
 * 27-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Removed dependency on Clib. RTOS now provides all of its own
 *	 output functions.  The primitives for these functions (eg,
 *	 Print) are intended to be provided by the operating system
 *	 itself.
 *
 * 14-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Added GC pause timer
 *
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Support for crash closures.
 *
 * This module is unsafe because it imports RTOSMachine.
 *)

UNSAFE MODULE RTOS;

(*IMPORT Unix, Uuio, Cstdlib, RT0u;*)
IMPORT RT0u, RTMem, RTOSMachine, Spy;
FROM RTCollectorSRC IMPORT DoTimings, pause_timer;

(*--------------------------------------------------- process termination ---*)

PROCEDURE Exit (<* UNUSED *>n: INTEGER) =
  BEGIN
    Print("RTOS.Exit!!! (hang in an infinite loop)\n");
    LOOP END;
  END Exit;

PROCEDURE Crash () =
  BEGIN
    Print("RTOS.Crash!!! \n");
    RTOSMachine.Debugger();
    Print("RTOS.Crash!!! (passed the debugger, hand in an infinite loop)\n");
    LOOP END; (* loop in case the debugger is broken *)
  END Crash;

(*------------------------------------------------------------- allocator ---*)

PROCEDURE GetMemory (size: INTEGER): ADDRESS =
  (* Return the address of "size" bytes of unused storage *)
  BEGIN
    RETURN RTMem.AllocGC(size);
  END GetMemory;

(*------------------------------------------------------------- collector ---*)
(* These procedures provide synchronization primitives for the allocator
   and collector.  This is the Ultrix version, and depends on the Ultrix
   user-level thread implementation. *)

(* LockHeap() enters a critical section; the same thread may enter the
   critical section multiple times.  It could be written at user level
   as:

| VAR
|   mutex    : MUTEX            := NEW(MUTEX);
|   condition: Thread.Condition := NEW(Thread.Condition);
|   thread   : Thread.T         := NIL;
|   count    : CARDINAL         := 0;

| PROCEDURE LockHeap () =
|   BEGIN
|     LOCK mutex DO
|       IF count = 0 THEN
|         thread := Thread.Self();
|         INC(count);
|       ELSIF thread = Thread.Self() THEN
|         INC(count);
|       ELSE
|         Thread.Wait(mutex, condition);
|       END;
|     END;
|   END LockHeap;

   However, it must be possible to call it from anywhere in the
   collector. *)

VAR 
  heap_spl: RTOSMachine.InterruptLevel;

PROCEDURE LockHeap () =
  VAR spl: RTOSMachine.InterruptLevel;
  BEGIN
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);
    INC(RT0u.inCritical);
    (* EVAL RTOSMachine.RestoreInterruptMask(spl);*)
    IF RT0u.inCritical = 1 THEN
      IF DoTimings AND pause_timer # NIL THEN Spy.Enter(pause_timer) END;
      heap_spl := spl;
    END;
  END LockHeap;

(* UnlockHeap() leaves the critical section.  It could be written at user
   level as:

| PROCEDURE UnlockHeap () =
|   BEGIN
|     LOCK mutex DO DEC(count); END;
|     IF count = 0 THEN Thread.Signal(condition); END;
|   END UnlockHeap;

   However, it must be possible to call it from anywhere inside the
   collector. *)

PROCEDURE UnlockHeap () =
  (* VAR spl: RTOSMachine.InterruptLevel;*)
  BEGIN
    (* spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);*)
    DEC(RT0u.inCritical);
    IF RT0u.inCritical = 0 THEN
      IF DoTimings AND pause_timer # NIL THEN Spy.Exit(pause_timer) END;
      RTOSMachine.RestoreInterruptMask(heap_spl);
    END;
    (* EVAL RTOSMachine.RestoreInterruptMask(spl);*)
  END UnlockHeap;


(*------------------------------------------------------------------- I/O ---*)

PROCEDURE Write (a: ADDRESS;  <* UNUSED *>n: INTEGER) =
  BEGIN
    (* EVAL Uuio.write (2, a, n);*)
    Print("RTOS.Write called\n");
    Prints(a);
  END Write;

BEGIN
END RTOS.

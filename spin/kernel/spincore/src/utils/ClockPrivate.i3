(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup.
 *
 * 16-Apr-96  Charles Garrett (garrett) at the University of Washington
 *	Export pcForDelayedTicks which is the PC value at which delayed
 *	ClockTick events are delivered.
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
 *	Added interrupt level alarm service for trusted clients.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created.
 *)

(* "ClockPrivate" provides interface to Alarm clock service for
   trusted clients. *)

INTERFACE ClockPrivate;
IMPORT Clock, CPU;

VAR pcForDelayedTicks: INTEGER;
    (* The PC that will be stored in the machine state when we receive a 
       clock interrupt that was delayed because interrupt levels were
       too high. Used in profiling to signal that a clock tick occured
       in an uninterruptible region. *)

PROCEDURE ClockTick(VAR ss: CPU.SavedState);
  (* "ClockTick" is called on every clock tick at
     CPU.InterruptLevel.Clock.  Performs the registered and
     expired alarms, if any. *)

PROCEDURE SetTrustedAlarm(
    time     : INTEGER;
    func     : Clock.TimeoutFunc;
    arg      : REFANY;
    cfunc    : Clock.CTimeoutFunc;
    carg     : ADDRESS;
    trusted  : BOOLEAN := TRUE;
    absolute : BOOLEAN := FALSE;
    period   : INTEGER := 0);
  (* "SetTrustedAlarm" registers a handler to go off delta ticks from
     now.  The handler is executed at spl high from clock interrupt.
     This interface is meant to be used by privileged clients. Do not
     call SetTrustedAlarm from within an interrupt handler. *)

(* Support for EPHEMERAL procedure *)

PROCEDURE BoundedBegin(nTicks: INTEGER); 
  (* "BoundedBegin" starts ticking away for a bounded region.  Kill
     the thread when the tick expires. *)

PROCEDURE BoundedEnd();
  (* "BoundedEnd" stops ticking away for a bounded region, the handler
     has completed in time. *)

PROCEDURE Init(verbose: BOOLEAN);
  (* "Init" is called at system Boot. *)

END ClockPrivate.

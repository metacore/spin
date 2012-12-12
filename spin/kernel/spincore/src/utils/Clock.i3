(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Consolidated various TimeVal types and procs here.
 *
 * 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup.
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
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Added CancelAlarm
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Alarm clock service.
 *)
INTERFACE Clock;
IMPORT Ctypes;

TYPE CTimeoutFunc = (* EPHEMERAL *) PROCEDURE (arg: ADDRESS);
     TimeoutFunc  = (* EPHEMERAL *) PROCEDURE (arg: REFANY);

TYPE
  TimeVal = RECORD
       tv_sec: Ctypes.int;
       tv_usec: Ctypes.int;
     END;

PROCEDURE TimeOfDay((*OUT*) VAR t: TimeVal);
PROCEDURE InterruptRate(): CARDINAL;  (* Clock chip ticks per second *)

(*
 * Return the current value of ticks.
 *)
PROCEDURE ReadTicks() : INTEGER;

(*
 * Register a handler to go off delta ticks from now.
 *)
PROCEDURE SetAlarm(time: INTEGER; 
                   func: TimeoutFunc;
                   arg: REFANY;
                   absolute: BOOLEAN := FALSE;
                   period: INTEGER := 0);
(*
 * Same as Set alarm, but for c functions that take untraced pointers.
 *)
PROCEDURE SetCAlarm(time: INTEGER; 
                   cfunc: CTimeoutFunc := NIL;
                   carg: ADDRESS := NIL;
                   absolute: BOOLEAN := FALSE;
                   period: INTEGER := 0);

(*
 * And a routine to deregister same.
 *)
PROCEDURE CancelAlarm(func: TimeoutFunc;
		      arg: REFANY) : BOOLEAN;

PROCEDURE CancelCAlarm (cfunc: CTimeoutFunc;
                        carg : ADDRESS) : BOOLEAN;


END Clock.


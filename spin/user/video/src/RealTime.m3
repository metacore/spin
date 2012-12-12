(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Routines to make fli realtime.
 *
 *)
MODULE RealTime; 
IMPORT Sema, Clock, Fli, Clib;

(*
 * Something to sleep on when we are done before our deadline.
 *)
VAR
  waitsema: Sema.T;

PROCEDURE ClockTick(<*UNUSED*>arg: REFANY) =
  BEGIN
    Sema.V(waitsema);
  END ClockTick;

(*
 * Initialize the periodicity of the thread
 *)
PROCEDURE SetStartTime(starttime: INTEGER) =
  BEGIN
    Clock.SetAlarm(starttime, ClockTick, NIL);
  END SetStartTime;

(*
 * Stall between deadlines.
 *)
PROCEDURE WaitUntilStartTime() =
  BEGIN
    Sema.P(waitsema);
  END WaitUntilStartTime;

BEGIN
  Clib.Print("Realtime fli getting initialized...\n");
  waitsema := Sema.Alloc(0);
  Clib.Print("Realtime fli initialized...doing demo...\n");
  Fli.fli_do_demo();
END RealTime. 

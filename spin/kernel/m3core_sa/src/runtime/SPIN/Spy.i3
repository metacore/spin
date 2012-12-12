(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
  	Spy.i3
  
   Spin system monitor
 *)
(*
 * HISTORY
 * 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Added GetElapsed
 *
 * 31-May-97  David Becker at the University of Washington
 *	Moved cycle counter functions to MachineCPU interface
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made Spy-s untraced to be able to measure the collector.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	The current interface is too rigid.  Added alwaysActive and
 *	nested flags, and Move() and Reset() operations which where needed
 *	for measuring the collector. More flexibility and clean-up to come.
 *
 * 27-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Fixed Up.
 *
 *	Author: David Becker
 *	Created: Thu Mar  2 17:14:22 PST 1995
 *
 *)

INTERFACE Spy;

TYPE T <: ADDRESS;
  Info = RECORD
    label: TEXT;
    hits, total, min, max: INTEGER; (* all values are cycles. *)
    samples: REF ARRAY OF INTEGER;
  END;
  

PROCEDURE Create (label:TEXT; nested: BOOLEAN := FALSE; nsamples := 0): T;
(* Create a new spy. If "nested" is TRUE, then nested entry to the spy is
   allowed. if "nsamples > 0", spy records the duration of each interval
   up to "nsamples". *)
PROCEDURE Delete(timer: T);
  
<*OBSOLETE*>PROCEDURE SetName(timer:T; label: TEXT);
<*OBSOLETE*>PROCEDURE Lookup(label: TEXT): T;

PROCEDURE Enter(timer:T);
(* Start timing the "timer". *)  
  
PROCEDURE Exit(timer:T);
(* Stop timing the "timer", and compute the duration of the interval.
   Min, max, samples are also updated. *)

<*OBSOLETE*>PROCEDURE On(); (* resets stats to zero *)
<*OBSOLETE*>PROCEDURE ContextSwitch(nextTimer:T);
<*OBSOLETE*>PROCEDURE ExitInterrupt();
PROCEDURE Move(thisTimer, newTimer: T);
(* Ask pardy. *)
  
<*OBSOLETE*>PROCEDURE Off();

PROCEDURE GetInfo(timer:T; VAR (*OUT*)info: Info);
(* Get current status of the spy "timer". *)

PROCEDURE GetElapsed() : INTEGER;
(* Get elapsed time since last Spy.Reset *)
  
PROCEDURE Activate(timer: T; onoff := TRUE);
(* Turn on or off the timer. *)
  
PROCEDURE Clear(timer: T);
(* Clear all the stats of the timer. *)
  
PROCEDURE Dump();
PROCEDURE DumpTime(timer:T);
PROCEDURE Hit(timer:T; start, stop: INTEGER);

<*OBSOLETE*>PROCEDURE Limits();

PROCEDURE Reset(); (* resets all the stats to zero *)
  
PROCEDURE Init(); (* Init() run directly by RTLinker before main body *)

END Spy.

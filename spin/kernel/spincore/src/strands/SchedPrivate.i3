(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 01-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Private interface to the scheduler.
 *)
INTERFACE SchedPrivate;
IMPORT CPU, Strand;

VAR
  userPreemptive: BOOLEAN := TRUE;
  kernelPreemptive: BOOLEAN := TRUE;
  gticks: INTEGER := 0; (* global ticks, continually incrementing counter *)
                        (* overflows in 500 Million years at 1024 ticks/sec *)
                        (* that's a little longer than we expect to be *)
                        (* around. *)

(*
 * Called from interrupt level to check for preemption, adjust
 * interrupt state, and preempt if necessary.
 *)
PROCEDURE PreemptIfNeeded(VAR ss: CPU.SavedState);

(*
 * Scheduler upcall that registers a clocktick.
 *)
PROCEDURE ClockTick(VAR ss: CPU.SavedState);

(*
 * Get the current strand scheduled by the global scheduler
 *)
PROCEDURE GetRunqHead() : Strand.T;

(*
 * Print out the run queue for debugging
 *)
PROCEDURE Dump();

(*
 * Start scheduling.
 *)
PROCEDURE Start();

(*
 * Initialize internal scheduler data structures.
 *)
PROCEDURE Init(verbose: BOOLEAN);

END SchedPrivate.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 14-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added SignalThread for Mach thread support.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Condition variables.
 *)
INTERFACE Condition;
IMPORT Strand, Thread;

(* Condition variables can be signaled or broadcast to from interrupt level *)
(* Use Thread.Signal, Thread.Wait, and Thread.Broadcast *)

(* support for signalling a particular thread waiting on a condition variable*)
PROCEDURE SignalThread(c: Thread.Condition; s: Strand.T);

END Condition.

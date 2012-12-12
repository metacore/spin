(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Charles Garrett (garrett) at the University of Washington
 *	Added SetProfiled which enables/disables profiling of an
 *	 individual thread.
 *
 * 10-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 22-Nov-96  becker at the University of Washington
 *	Change to TrackStrand from Spy
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. In-kernel thread interface for untrusted clients.
 *      This is an extension to the M3 Thread interface offered by
 *      the runtime and offers functionality not provided by the
 *      standard M3 Thread interface.
 *)
INTERFACE ThreadExtra;
IMPORT Thread, Strand, TrackStrand, Rd, Wr;

TYPE ResultT = REFANY;
TYPE ArgT = REFANY;
TYPE FuncT = PROCEDURE(arg: ArgT) : ResultT;

CONST defaultPriority = Strand.defPriority;

(*
 * Execute a given function in a new thread of control.
 * Much faster than a closure.
 *)
PROCEDURE PFork(func: FuncT;
               arg: ArgT;
               pri: Strand.PriorityT := defaultPriority) : Thread.T;

(*
 * Terminate the current thread with result.
 *)
PROCEDURE Exit(result: ResultT);

PROCEDURE Yield();

(*
 * Return the name of the strand on whose behalf we are doing work
 * in the kernel.
 *)

(* WARNING THIS FUNCTION IS NOT SAFE.  IT ALLOWS ADDRESS SPACE INFORMATION TO LEAK *)
PROCEDURE GetBoundUserStrand(th: Thread.T): Strand.T;
(* If "th" is not bound to a user thread, the proc returns NIL.*)
  
(*
 * Change the default input reader or output writer. Returns old value for
 * stacking.  Should go to stack interface directly.
 *)
PROCEDURE SetRdSelf(rd: Rd.T) : Rd.T;
PROCEDURE GetRdSelf() : Rd.T;
PROCEDURE SetWrSelf(wr: Wr.T) : Wr.T;
PROCEDURE GetWrSelf() : Wr.T;

PROCEDURE GetTracker(th: Thread.T): TrackStrand.T;

FUNCTIONAL PROCEDURE GetId(th: Thread.T): INTEGER; (* XXX temporary *)

PROCEDURE SetProfiled(th: Thread.T; b: BOOLEAN);

END ThreadExtra.



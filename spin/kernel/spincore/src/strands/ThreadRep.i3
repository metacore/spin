(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Dec-96  Charles Garrett (garrett) at the University of Washington
 *	Combined the profiled version and the unprofiled version of Thread.T.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Removed Spy
 *
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use semaphores for events instead of cond. variables.
 *
 * 20-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Switch to per thread reader and writer.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Representation of kernel threads.
 *
 *)
INTERFACE ThreadRep;
IMPORT ThreadExtra, ThreadPrivate;
IMPORT CPU, Strand;
IMPORT SpinException;
IMPORT Thread, Sema;
IMPORT Rd, Wr;

REVEAL
  Thread.T = Strand.T BRANDED OBJECT
        ms    : UNTRACED REF CPU.GeneralRegs;
        lock  : MUTEX;
        stack : ThreadPrivate.StackT;
        func  : ThreadExtra.FuncT;  (* func to call for startup/continuation *)
        arg   : ThreadExtra.ArgT;   (* argument to call *)
        res   : ThreadExtra.ResultT;(* thread return value *)
        done  : Thread.Condition;   (* waiting place until thread exits *)
        state : ThreadPrivate.State; (* nascent/active/dead etc. *)
        exception: SpinException.ExceptionInfo; (* any exception outstanding *)
        handlerStack: ADDRESS;         (* per-thread exception handler stack *)
        profileStack: ThreadPrivate.ProfileData; (* used to patch return addr *)
        gcnext      : Thread.T;        (* used for GC ambiguous root set *) 
   
        (* per thread I/O *)
        reader      : Rd.T;            (* where to get input *)
        writer      : Wr.T;            (* where to send output *)

        (* support for Mach synch primitives *)
        wait_event     : Sema.T;       (* current event sema to wait on *)
        wait_eventcache: Sema.T;       (* last event sema waited on *)

        intid       : INTEGER;         (* XXX temporary!!! for debug *)
        waitResult: WaitResult; (* used only by Alert*** *)
        condition: Thread.Condition; (* for Alert{Join,Wait}.
					This is always NIL
					EXCEPTION INTEGER Alert* procedures. *)
 END;
TYPE WaitResult = {TimedOut, Awakened, Interrupted, ShouldTerminate};

END ThreadRep.

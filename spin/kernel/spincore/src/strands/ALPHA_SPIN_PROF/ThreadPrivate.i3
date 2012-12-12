(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cleanup for machine-independence.  Moved debugger support functions to
 *	MachineDebugger.
 *
 * 18-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Made stacks traced, in the current dash to make everything traced.
 *	The stacks should be allocated from the bottom of the heap so they
 *	don't fragment the traced memory heap.
 *
 * 22-Dec-95  Charles Garrett (garrett) at the University of Washington
 *	Removed SetStandardInput and Output.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Moved KThread_Startup to KThreadPrivateExtern
 *
 * 21-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Moved Thread.T revelation to KThreadRep.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Added fields to maintain exception and execution state.
 *
 * 23-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added preallocate to shield fast spawning clients from
 *	the M3 heap allocator.
 *
 * 15-Aug-95  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the signature of KThreadPrivate.StopHandler to pass 
 *	callee-saved registers directly and not as a pointer to structure.
 *
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Added sleep.
 *
 * 13-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added support for Mach/OSF compatibility layer.
 *
 * 07-Apr-95  Marc Fiuczynski (mef) at the University of Washington
 *	Dispatcher nows saves registers in SavedRegs vs. SavedArgs.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. In-kernel thread interface for trusted clients.
 *)
INTERFACE ThreadPrivate;

IMPORT Thread, ThreadExtra, Strand, CPU;
IMPORT FastList;

CONST
  DefaultStackSize = 8 * 8192;
  StackSize = DefaultStackSize;
  Slack = 512;

TYPE StackData = UNTRACED REF ARRAY [0..DefaultStackSize-1] 
                 OF CHAR;
TYPE StackT = FastList.T OBJECT
  base: StackData;
END;

TYPE
ProfileData = UNTRACED REF RECORD 
        pointer : ADDRESS;                 (* Points to a profiling record *)
        data    : ARRAY [0..4087] OF CHAR; (* in this array *)
      END;

TYPE State = {Nascent,   (* preallocated, unused *)
	      Active,    (* running or runnable  *)
              ResultAvailable, (* terminated, a return result is available *)
	      ReadyToDie, (* terminated, but the stack is still in use *)
              Dead,      (* terminated cleanly, stack can now be collected *)
              ExceptionPosted  (* outstanding exception *)
             };

CONST StateName = ARRAY State OF TEXT {
             "Nascent",
             "Active",
             "ResultAvailable",
             "ReadyToDie",
             "Dead",
             "ExceptionPosted"
};

(*
 * Create a suspended kernel thread
 *)
PROCEDURE Create(func: ThreadExtra.FuncT;
                 arg: ThreadExtra.ArgT;
                 pri: Strand.PriorityT := ThreadExtra.defaultPriority) : Thread.T;

(*
 * Advise the thread module that we will allocate n threads.
 * XXX Move this to unprivileged interface when admission control
 * is implemented.
 *)
PROCEDURE Preallocate(n: CARDINAL);

(*
 * Kill another thread. Joiners see the result.
 *)
PROCEDURE Kill(th: Thread.T; result: ThreadExtra.ResultT);

(*
 * Create a trap handler thread.
 *)
PROCEDURE CreateTrapHndlr() : Thread.T;

(*
 * Kill a trap handler thread.
 *)
PROCEDURE KillTrapHndlr(th: Thread.T);

(*
 * Exit oneself with result res.
 *)
PROCEDURE Exit_main(th: Thread.T; result: ThreadExtra.ResultT);

(*
 * See if this thread is in a RAS region, update pc if so.
 *)
PROCEDURE DoRASCheck(th: Thread.T);

(*
 * Stack red zone
 *)  
PROCEDURE CheckRedZone(th: Thread.T);

(*
 * KThread stop and run handlers
 *)
PROCEDURE StopHandler(s: Strand.T;
                      regs: UNTRACED REF CPU.CalleeSavedRegs) : BOOLEAN;

PROCEDURE RunHandler(s: Strand.T);

PROCEDURE KThreadBody(th: Thread.T);

(* 
 * Apply through all thread contexts. Stop iterating when apply returns FALSE.
 * Returns result of last iterProc.  The iteration set is only guaranteed to
 * be consistent with a snapshot of the system's threads at some point in the
 * past. It may not reflect current reality.  iterProc is called without any
 * locks held.
 *)

TYPE ThreadApplyClosure = OBJECT
  METHODS
      apply(t: Thread.T): BOOLEAN;
  END;

PROCEDURE Apply(cl: ThreadApplyClosure): BOOLEAN;

(*
 * Initialize kernel threads.
 *)
PROCEDURE Init(verbose: BOOLEAN);

END ThreadPrivate.

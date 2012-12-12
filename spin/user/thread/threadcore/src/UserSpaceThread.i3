(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Deleted externalize and internalize. Use ones in AddressSpace.
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. The Mach Thread interface that OSF wants to see.
 *) 
INTERFACE UserSpaceThread;
IMPORT Strand, Space, CPU, Word;

CONST Brand = "UserSpaceThread"; (* Use this name to link with this extension *)

TYPE T <: Strand.T;
TYPE State = REF RECORD
     fpustate: REF CPU.FloatRegs;
     cpustate: REF CPU.GeneralRegs;
END;
TYPE ExternRef = Word.T;

(* Create a new UserSpacethread, with space as its address map *)
(* The returned thread is not runnable. *)
PROCEDURE Create(space: Space.T) : T;

(* Destroy the named thread after stopping it safely *)
PROCEDURE Destroy(Uthread: T);

(* Stop the thread. [GS]etState will only be coherent *)
(* with respect to the process after a call to Suspend returns *)
PROCEDURE Suspend(Uthread: T);

(* Make the process runnable. *)
PROCEDURE Resume(Uthread: T);

(* Suspend and Resume keep a suspend count and the thread is suspended *)
(* on the 0->1 transition and resumed on the 1->0 transition. That is, *)
(* nested suspends and resumes work intuitively. *)

(* Get and Set execution state respectively *)
PROCEDURE GetState(Uthread: T; VAR state: State);

(*
 * Setting FPU state causes the thread's floating point
 * state to be enabled.
 *)
PROCEDURE SetState(Uthread: T; state: State);

(*
 * Stop saving/restoring floating point state for this
 * thread.
 *)
PROCEDURE DisableFloatingPointState(Uthread: T);

(*
 * Allocate a floating point state save area, and
 * start saving and restoring FPU state.
 *)
PROCEDURE EnableFloatingPointState(Uthread: T);

PROCEDURE GetSpace(Uthread: T) : Space.T;
(* Get the space "Uthread" is running on. *)
  
(*
 * Return the user space space thread on whose behalf this kernel thread 
 * is running.
 *)

PROCEDURE Self(): T;


END UserSpaceThread.

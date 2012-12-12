(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Pass through Functions are now bypassed using the dispatcher.
 *	That is, these procedure calls go directly through to their
 *	EXTERNAL.
 *
 * 10-Dec-94  Emin Gun Sirer (egs) at the University Washington
 *	Created. CPU state manipulation for trusted clients.
 *)
UNSAFE (* for externs *)
MODULE MachineThread EXPORTS MachineThread, Thread;
IMPORT CPU, Thread, ThreadRep, ThreadPrivate, Word, IO;

(*  These PROCs are temporariliy implemented in machine/MachineThread.i3
    Look there for details


TYPE StartupT = PROCEDURE(t: REFANY);
PROCEDURE Startup(t: REFANY) =
  BEGIN
    MachineThreadExtern.Startup(t);
  END Startup;

TYPE CallContinuationT =PROCEDURE(thread: Thread.T; 
     bodystarter: PROCEDURE(a:Thread.T); newsp: Word.T);
PROCEDURE CallContinuation(thread: Thread.T;
  bodystarter: PROCEDURE(a:Thread.T); newsp: Word.T) =
  BEGIN
    MachineThreadExtern.CallContinuation(thread, bodystarter, newsp);
  END CallContinuation;
*)

PROCEDURE Setup(state: UNTRACED REF CPU.GeneralRegs; 
                stackTop: Word.T; 
                initialFunction: PROCANY; 
                arg: Word.T) = 
  BEGIN
    state.ksp := LOOPHOLE(stackTop, Word.T);
    state.pc  := LOOPHOLE(Startup, CPU.GeneralRegister);
    state.s0 := LOOPHOLE(arg, CPU.GeneralRegister);
    state.s1 := LOOPHOLE(initialFunction, CPU.GeneralRegister);
  END Setup;

PROCEDURE InitialSP(READONLY stack: ThreadPrivate.StackT) : Word.T =
  BEGIN
    RETURN Word.And(Word.Plus(LOOPHOLE(ADR(stack.base[0]), Word.T),
                              ThreadPrivate.StackSize - ThreadPrivate.Slack),
                    16_ffffffffffffffff0);
  END InitialSP;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    IF verbose THEN
      IO.Put("MachineThread started...\n");
    END;
  END Init;

(*
 * Debugger support.
 *)

(* Return the saved registers of a stopped thread. *)
PROCEDURE GetSavedRegs(kthread: Thread.T; 
                       VAR state: CPU.MachineState) =
  BEGIN
    state.s0 := kthread.ms.s0;
    state.s1 := kthread.ms.s1;
    state.s2 := kthread.ms.s2;
    state.s3 := kthread.ms.s3;
    state.s4 := kthread.ms.s4;
    state.s5 := kthread.ms.s5;
    state.fp := kthread.ms.s6;
    state.pc := kthread.ms.pc;
    state.sp := kthread.ms.ksp;
  END GetSavedRegs;

(* Set the saved registers of a stopped thread. *)
PROCEDURE SetSavedRegs(kthread: Thread.T; 
                       READONLY state: CPU.MachineState) =
  BEGIN
    kthread.ms.s0 := state.s0;
    kthread.ms.s1 := state.s1;
    kthread.ms.s2 := state.s2;
    kthread.ms.s3 := state.s3;
    kthread.ms.s4 := state.s4;
    kthread.ms.s5 := state.s5;
    kthread.ms.s6 := state.fp;
    kthread.ms.pc := state.pc;
    kthread.ms.ksp := state.sp;
  END SetSavedRegs;

PROCEDURE ReturnSP(thread: T) : Word.T =
  BEGIN
    LOCK thread.lock DO
      RETURN thread.ms.ksp;
    END;
  END ReturnSP;

BEGIN
END MachineThread.

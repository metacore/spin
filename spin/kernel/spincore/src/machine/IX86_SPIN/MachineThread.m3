(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
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
    state.edi := LOOPHOLE(arg, CPU.GeneralRegister);
    state.esi := LOOPHOLE(initialFunction, CPU.GeneralRegister);
  END Setup;

PROCEDURE InitialSP(READONLY stack: ThreadPrivate.StackT) : Word.T =
  BEGIN
    RETURN Word.And(Word.Plus(LOOPHOLE(ADR(stack.base[0]), Word.T),
                              ThreadPrivate.StackSize - ThreadPrivate.Slack),
                    16_fffffff0);
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
    state.ebx := kthread.ms.ebx;
    state.ebp := kthread.ms.ebp;
    state.edi := kthread.ms.edi;
    state.esi := kthread.ms.esi;
    state.eip := kthread.ms.pc;
    state.esp := kthread.ms.ksp;
  END GetSavedRegs;

(* Set the saved registers of a stopped thread. *)
PROCEDURE SetSavedRegs(kthread: Thread.T; 
                       READONLY state: CPU.MachineState) =
  BEGIN
    kthread.ms.ebx := state.ebx;
    kthread.ms.ebp := state.ebp;
    kthread.ms.edi := state.edi;
    kthread.ms.esi := state.esi;
    kthread.ms.pc := state.eip;
    kthread.ms.ksp := state.esp;
  END SetSavedRegs;

PROCEDURE ReturnSP(thread: T) : Word.T =
  BEGIN
    LOCK thread.lock DO
      RETURN thread.ms.ksp;
    END;
  END ReturnSP;

BEGIN
END MachineThread.


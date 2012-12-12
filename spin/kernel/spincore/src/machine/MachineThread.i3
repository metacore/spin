(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Thread bootstrap functions.
 *)
UNSAFE (* misuse of startup can cause unchecked runtime faults *)
INTERFACE MachineThread;
IMPORT Thread, ThreadPrivate, CPU, Word;

(*
 * The routine that starts up a thread. It takes its arguments in 
 * callee-save registers, and calls an M3 procedure that can then
 * use the regular calling convention.
 *)
<* EXTERNAL *> PROCEDURE Startup();

<* EXTERNAL *> PROCEDURE CallContinuation(thread: Thread.T;
                           bodystarter: PROCEDURE(a:Thread.T);
                           newsp: Word.T);

PROCEDURE Setup(state: UNTRACED REF CPU.GeneralRegs; 
                stackTop: Word.T; 
                initialFunction: PROCANY;
                arg: Word.T);

PROCEDURE Init(verbose:BOOLEAN);

PROCEDURE InitialSP(READONLY stack: ThreadPrivate.StackT) : Word.T;

PROCEDURE GetSavedRegs(thread: Thread.T; 
                       VAR state: CPU.MachineState);

PROCEDURE SetSavedRegs(kthread: Thread.T; 
                       READONLY state: CPU.MachineState);

PROCEDURE ReturnSP(thread: Thread.T) : Word.T;

END MachineThread.

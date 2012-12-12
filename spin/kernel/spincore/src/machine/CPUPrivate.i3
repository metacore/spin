(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Took out duplicate code(!).
 *
 * 10-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Interrupt class and level are both arch dependent.
 *
 * 16-Dec-96  Charles Garrett (garrett) at the University of Washington
 *	Added procedure that returns the PC where the interrupt level is
 *	 lowered. Profiling wants to know this.
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	The SetInterruptLevel procedure is made EXTERNAL so that
 *	profiling code can tell who lowered the interrupt level.
 *
 * 29-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Pass through Functions are now bypassed using the dispatcher.
 *	That is, these procedure calls go directly through to their
 *	EXTERNAL.
 *
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. CPU state manipulation for trusted clients.
 *)
UNSAFE (* misuse may cause unchecked runtime error *)
INTERFACE CPUPrivate;
IMPORT CPUDep, CPU;

TYPE
  (* The difference between interrupt class and level is that class denotes
     the particular type of interrpt that should be masked. The level is
     an opaque value that hold the value of current interrupt mask. On
     machines that completely serializes the interrupt priority(eg Alpha),
     they can be same, but on machines that each device can individually
     turned on and off, interrupt mask is conceptually a set of interrupt
     class values. *)
  InterruptClass = CPUDep.InterruptClass;
  
PROCEDURE SaveAllGeneralRegs(VAR general: CPU.GeneralRegs): BOOLEAN;
PROCEDURE SaveCalleeSaveGeneralRegs(general: UNTRACED REF CPU.GeneralRegs) : BOOLEAN;
PROCEDURE RestoreCalleeSaveGeneralRegs(general: UNTRACED REF CPU.GeneralRegs);
PROCEDURE RestoreUserGeneralRegs(READONLY general: CPU.GeneralRegs);

<*EXTERNAL*> PROCEDURE IsLow(mask: CPU.InterruptLevel) : BOOLEAN;
<*EXTERNAL*> PROCEDURE IsHigh(mask: CPU.InterruptLevel) : BOOLEAN;

<*EXTERNAL*> PROCEDURE SetInterruptMask(mask: InterruptClass): CPU.InterruptLevel;
<*EXTERNAL*> PROCEDURE RestoreInterruptMask(mask: CPU.InterruptLevel);

PROCEDURE InstallTrapHandler(s: CPU.TrapType; prot: PROCANY);

PROCEDURE Breakpoint(); (* Gives you a breakpoint trap *)

PROCEDURE GetDebuggerRegs(VAR state: CPU.MachineState);
PROCEDURE SetDebuggerRegs(READONLY state: CPU.MachineState);

PROCEDURE DumpState(READONLY ss: CPU.SavedState);

PROCEDURE Init(verbose:BOOLEAN);

END CPUPrivate.

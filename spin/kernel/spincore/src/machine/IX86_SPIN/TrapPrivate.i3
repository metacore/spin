(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *) 
UNSAFE INTERFACE TrapPrivate;
IMPORT CPU;

PROCEDURE SyscallWrap(VAR ss: CPU.SavedState);
PROCEDURE PrivilegedInstructionFaultWrap(VAR ss: CPU.SavedState);
PROCEDURE TraceTrapWrap(VAR ss: CPU.SavedState);
PROCEDURE BreakpointWrap(VAR ss: CPU.SavedState);
PROCEDURE ArithmeticTrapWrap(VAR ss: CPU.SavedState);
PROCEDURE DivideFaultWrap(VAR ss: CPU.SavedState);
PROCEDURE OverflowFaultWrap(VAR ss: CPU.SavedState);
PROCEDURE BoundsCheckFaultWrap(VAR ss: CPU.SavedState);
PROCEDURE ProtectionFaultWrap(VAR ss: CPU.SavedState);
PROCEDURE PageFaultWrap(VAR ss: CPU.SavedState);
PROCEDURE FPFaultWrap(VAR ss: CPU.SavedState);
PROCEDURE FPOperandFaultWrap(VAR ss: CPU.SavedState);
PROCEDURE ClockTickWrap(VAR ss: CPU.SavedState);


PROCEDURE Init(verbose: BOOLEAN);

<*EXTERNAL note*> PROCEDURE GprofNote();

END TrapPrivate.

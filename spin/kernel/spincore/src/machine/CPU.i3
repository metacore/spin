 (*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 05-Jan-98  Tsutomu Owa (owa) at the University of Washington
 *	Added Save/RestoreESP.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Placed cycle counter procs here.
 *
 * 27-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added ExtractReturnAddress.
 *
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Added TextifyRegs.
 *
 * 17-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Merge in FloatUState, CPUState, Cache, and part of MachineMem.
 *	These interfaces, variables and types are suppose to be the
 *	machine independent view of the CPU.
 *
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Machine-independent structure definitions.
 *)
INTERFACE CPU;
IMPORT CPUDep, Word;

CONST
 (* Number of bits to shift for pages *)
  PGSHIFT: Word.T	 = CPUDep.PGSHIFT;
  PAGESIZE: Word.T	 = CPUDep.PAGESIZE;  (* Size of a page *)
  PAGEMASK: Word.T	 = CPUDep.PAGEMASK;

TYPE
  GeneralRegs = CPUDep.GeneralRegs;
  FloatRegs = CPUDep.FloatRegs;
  GeneralRegister = CPUDep.GeneralRegister;
  FloatRegister = CPUDep.FloatRegister;
  InterruptLevel = CPUDep.InterruptLevel;
  TrapType = CPUDep.TrapType; 
  SavedState = CPUDep.SavedState; 
  MachineState = CPUDep.MachineState;
  VirtAddress = CPUDep.VirtAddress;
  PhysAddress = CPUDep.PhysAddress;
  CalleeSavedRegs = CPUDep.CalleeSavedRegs;

PROCEDURE FlushInstructionCache();
PROCEDURE FlushDataCache();
PROCEDURE FlushTLB();
  
PROCEDURE SetUserGeneralRegs(VAR gp: GeneralRegs);
PROCEDURE DumpGeneralRegs(READONLY gp: GeneralRegs);
PROCEDURE TextifyRegs(READONLY ss: SavedState) : TEXT;

PROCEDURE ExtractInterruptLevel(READONLY ss: SavedState) : InterruptLevel;

PROCEDURE ExtractReturnAddress(READONLY ss: SavedState) : GeneralRegister;

PROCEDURE SetUserFloatRegs(READONLY fp: FloatRegs);
PROCEDURE GetUserFloatRegs(VAR fp: FloatRegs);
PROCEDURE EnableUserFloatOps(on: BOOLEAN);

PROCEDURE CopyCalleeSavedRegs(from: UNTRACED REF CalleeSavedRegs; 
                                to: UNTRACED REF GeneralRegs);

PROCEDURE InterruptDropPC(): INTEGER;

PROCEDURE CurrentStackPointer(): GeneralRegister;

PROCEDURE CycleCounter(): Word.T;
PROCEDURE Hertz(): Word.T;
PROCEDURE CycleMinus(stop,start: Word.T): Word.T; (* stop - start *)
PROCEDURE CycleToMicrosec(cycles: Word.T) : Word.T;

PROCEDURE SaveESP(VAR s: GeneralRegister);
PROCEDURE RestoreESP(READONLY s: GeneralRegister);

END CPU.

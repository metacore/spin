(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 05-Jan-98  Tsutomu Owa (owa) at the University of Washington
 *      Added Save/RestoreESP
 *
 * 31-May-97  David Becker at the University of Washington
 *	Added cycle counter procs
 *
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)
UNSAFE (* for externals *)
INTERFACE CPUExtern;
IMPORT CPU, Word;

<*EXTERNAL *> PROCEDURE FlushInstructionCache();
<*EXTERNAL *> PROCEDURE FlushDataCache();

<*EXTERNAL *> PROCEDURE RdtscLow() : Word.T;

<*EXTERNAL*> PROCEDURE SaveAllGeneralRegs(VAR ms: CPU.SavedState): BOOLEAN;
<*EXTERNAL*> PROCEDURE SaveCalleeSaveGeneralRegs(general: UNTRACED REF CPU.GeneralRegs) : BOOLEAN;
<*EXTERNAL*> PROCEDURE RestoreCalleeSaveGeneralRegs(general: UNTRACED REF CPU.GeneralRegs);
<*EXTERNAL*> PROCEDURE RestoreUserGeneralRegs(READONLY ms: CPU.SavedState); 
<*EXTERNAL*> PROCEDURE SetUserFloatRegs(READONLY s: CPU.FloatRegs);
<*EXTERNAL*> PROCEDURE GetUserFloatRegs(VAR s: CPU.FloatRegs);
<*EXTERNAL*> PROCEDURE EnableUserFloatOps(on: BOOLEAN);
<* EXTERNAL *> PROCEDURE SaveGeneralRegs(ms: UNTRACED REF CPU.SavedState) : BOOLEAN;
<* EXTERNAL *> PROCEDURE RestoreGeneralRegs(ms: UNTRACED REF CPU.SavedState);

<* EXTERNAL *> PROCEDURE ChangeSoftSCB(s: CPU.TrapType; proc: PROCANY);

(*
 * Implemented in: ttd/alpha/kttd_interface.c
 * Function: Manipulates debugger register state
 *)
<*EXTERNAL ttd_recover_esp*>PROCEDURE GetDebuggerRegs(
                                VAR state: CPU.MachineState);

<*EXTERNAL ttd_overwrite_esp*>PROCEDURE SetDebuggerRegs(
                                   READONLY state: CPU.MachineState);

<*EXTERNAL *>PROCEDURE get_cr2(): Word.T;
<*EXTERNAL *>PROCEDURE mvesp(): Word.T;
<*EXTERNAL *>PROCEDURE hertz(): Word.T;
<*EXTERNAL *>PROCEDURE cyclecounter(): Word.T;
<*EXTERNAL *>PROCEDURE cycleminus(a,b:Word.T): Word.T;
<*EXTERNAL *>PROCEDURE cycle_to_microsec(cycles:Word.T): Word.T;

<* EXTERNAL *> PROCEDURE SaveESP(VAR reg: CPU.GeneralRegister);
<* EXTERNAL *> PROCEDURE RestoreESP(READONLY reg: CPU.GeneralRegister);

END CPUExtern.

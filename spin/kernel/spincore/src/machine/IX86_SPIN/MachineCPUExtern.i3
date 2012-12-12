(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Added cycle counter procs
 *
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)
UNSAFE (* for externals *)
INTERFACE MachineCPUExtern;
IMPORT MachineCPU, Word;

<*EXTERNAL *> PROCEDURE FlushInstructionCache();
<*EXTERNAL *> PROCEDURE FlushDataCache();

<*EXTERNAL *> PROCEDURE RdtscLow() : Word.T;

<*EXTERNAL*> PROCEDURE SaveAllGeneralRegs(VAR ms: MachineCPU.SavedState): BOOLEAN;
<*EXTERNAL*> PROCEDURE SaveCalleeSaveGeneralRegs(general: UNTRACED REF MachineCPU.GeneralRegs) : BOOLEAN;
<*EXTERNAL*> PROCEDURE RestoreCalleeSaveGeneralRegs(general: UNTRACED REF MachineCPU.GeneralRegs);
<*EXTERNAL*> PROCEDURE RestoreUserGeneralRegs(READONLY ms: MachineCPU.SavedState); 
<*EXTERNAL*> PROCEDURE SetUserFloatRegs(READONLY s: MachineCPU.FloatRegs);
<*EXTERNAL*> PROCEDURE GetUserFloatRegs(VAR s: MachineCPU.FloatRegs);
<*EXTERNAL*> PROCEDURE EnableUserFloatOps(on: BOOLEAN);
<* EXTERNAL *> PROCEDURE SaveGeneralRegs(ms: UNTRACED REF MachineCPU.SavedState) : BOOLEAN;
<* EXTERNAL *> PROCEDURE RestoreGeneralRegs(ms: UNTRACED REF MachineCPU.SavedState);

<* EXTERNAL *> PROCEDURE ChangeSoftSCB(s: MachineCPU.TrapType; proc: PROCANY);

(*
 * Implemented in: ttd/alpha/kttd_interface.c
 * Function: Manipulates debugger register state
 *)
<*EXTERNAL ttd_recover_esp*>PROCEDURE GetDebuggerRegs(
                                VAR state: MachineCPU.MachineState);

<*EXTERNAL ttd_overwrite_esp*>PROCEDURE SetDebuggerRegs(
                                   READONLY state: MachineCPU.MachineState);

<*EXTERNAL *>PROCEDURE get_cr2(): Word.T;
<*EXTERNAL *>PROCEDURE mvesp(): Word.T;
<*EXTERNAL *>PROCEDURE hertz(): Word.T;
<*EXTERNAL *>PROCEDURE cyclecounter(): Word.T;
<*EXTERNAL *>PROCEDURE cycleminus(a,b:Word.T): Word.T;
<*EXTERNAL *>PROCEDURE cycle_to_microsec(cycles:Word.T): Word.T;

END MachineCPUExtern.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Made unsafe. Eliminated cache flush ops, and PatchOffset (unused)
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 * 
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *
 * This interface is unsafe because it declares externals.
 *
 *)
UNSAFE INTERFACE MachineStitcherExtern;
IMPORT AliasDesc;

<* EXTERNAL *>
PROCEDURE CloneTrampolineStub(func: AliasDesc.T; 
                              offset: INTEGER;
                              saveRegs: BOOLEAN): PROCANY;

<* EXTERNAL *>
PROCEDURE CloneDefaultStub(desc: AliasDesc.T; 
                           nArgs: INTEGER; 
                           saveRegs: BOOLEAN): PROCANY;

<* EXTERNAL *>
PROCEDURE CloneStub(desc: AliasDesc.T; 
                    nArgs: INTEGER; 
                    nHandlers: INTEGER; 
                    saveRegs: BOOLEAN): PROCANY;

<* EXTERNAL *>
PROCEDURE CloneUnrolledStub(desc: AliasDesc.T; 
                            handlers: ADDRESS;
                            nArgs: INTEGER; 
                            res: BOOLEAN; 
                            nHandlers: INTEGER; 
                            saveRegs: BOOLEAN): PROCANY;

<* EXTERNAL *>
PROCEDURE CloneInlinedStub(desc: AliasDesc.T; 
                           handlers: ADDRESS;
                           nArgs: INTEGER; 
                           res: BOOLEAN; 
                           nHandlers: INTEGER; 
                           saveRegs: BOOLEAN;
                           doInline: BOOLEAN): PROCANY;


<* EXTERNAL StitcherInitialize *>
PROCEDURE Init();

<* EXTERNAL *>
PROCEDURE trace_counts_dump();

<* EXTERNAL *>
PROCEDURE trace_counts_reset();

<* EXTERNAL *>
PROCEDURE CloneArgumentStub(proc: PROCANY;
                            nArgs: INTEGER;
                            useClosure: BOOLEAN;
                            closure: REFANY): PROCANY;

END MachineStitcherExtern.

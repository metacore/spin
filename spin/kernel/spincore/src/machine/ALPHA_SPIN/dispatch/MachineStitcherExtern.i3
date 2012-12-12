(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	All stub cloning procedures have been changed to take the
 *	 saveRegs argument, because the profiled versions all need to
 *	 use it.
 *
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
IMPORT AliasDesc, Word;

<* EXTERNAL *>
PROCEDURE CloneTrampolineStub(func: AliasDesc.T; 
                              offset: INTEGER;
                              saveRegs: BOOLEAN): PROCANY;

<* EXTERNAL *>
PROCEDURE CloneDefaultStub(desc: AliasDesc.T; 
                           nArgs: INTEGER; 
                           res: BOOLEAN; 
                           saveRegs: BOOLEAN): PROCANY;

<* EXTERNAL *>
PROCEDURE CloneStub(desc: AliasDesc.T; 
                    nArgs: INTEGER; 
                    res: BOOLEAN; 
                    nHandlers: INTEGER; 
                    saveRegs: BOOLEAN): PROCANY;

<* EXTERNAL *>
PROCEDURE CloneUnrolledStub(desc: AliasDesc.T; 
                            handlers: ADDRESS;
                            nArgs: INTEGER; 
                            res: BOOLEAN; 
                            nHandlers: INTEGER; 
                            saveRegs: BOOLEAN;
                            doGuards: BOOLEAN;
                            guardList: UNTRACED REF Word.T;
                            guardCnt: INTEGER): PROCANY;

<* EXTERNAL *>
PROCEDURE CloneInlinedStub(desc: AliasDesc.T; 
                           handlers: ADDRESS;
                           nArgs: INTEGER; 
                           res: BOOLEAN; 
                           nHandlers: INTEGER; 
                           saveRegs: BOOLEAN;
                           doInline: BOOLEAN;
                           trace: BOOLEAN): PROCANY;

<* EXTERNAL *>
PROCEDURE CloneFromList(list: UNTRACED REF Word.T; cnt: INTEGER;
                        nArgs: INTEGER; res: BOOLEAN; 
                        saveRegs: BOOLEAN): PROCANY;

<* EXTERNAL *>
PROCEDURE PatchCall(ptr: Word.T; proc: PROCANY);

<* EXTERNAL StitcherInitialize *>
PROCEDURE Init();

<* EXTERNAL *>
PROCEDURE AsmTest(proc: PROCANY; name: ADDRESS);

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

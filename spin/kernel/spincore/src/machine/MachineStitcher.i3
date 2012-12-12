(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	All stub clone routines now take the saveRegs argument since the
 *	 profiled stubs depend on it.
 *
 * 20-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 16-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Interface to machine dependent stitcher functions.
 *)
INTERFACE MachineStitcher;
IMPORT AliasDesc, Word;

PROCEDURE CloneTrampolineStub(func: AliasDesc.T; 
                              offset: INTEGER;
                              saveRegs: BOOLEAN): PROCANY;

PROCEDURE CloneDefaultStub(desc: AliasDesc.T;
                           nArgs: INTEGER;
                           res: BOOLEAN;
                           saveRegs: BOOLEAN): PROCANY;

PROCEDURE CloneStub(desc: AliasDesc.T; 
                    nArgs: INTEGER; 
                    res: BOOLEAN; 
                    nHandlers: INTEGER; 
                    saveRegs: BOOLEAN): PROCANY;

PROCEDURE CloneUnrolledStub(desc: AliasDesc.T; 
                            handlers: ADDRESS;
                            nArgs: INTEGER; 
                            res: BOOLEAN; 
                            nHandlers: INTEGER; 
                            saveRegs: BOOLEAN;
                            doGuards: BOOLEAN;
                            guardList: UNTRACED REF Word.T;
                            guardCnt: INTEGER): PROCANY;

PROCEDURE CloneInlinedStub(desc: AliasDesc.T; 
                           handlers: ADDRESS;
                           nArgs: INTEGER; 
                           res: BOOLEAN; 
                           nHandlers: INTEGER; 
                           saveRegs: BOOLEAN;
                           doInline: BOOLEAN;
                           trace: BOOLEAN): PROCANY;

PROCEDURE CloneFromList(list: UNTRACED REF Word.T; cnt: INTEGER;
                        nArgs: INTEGER; res: BOOLEAN; 
                        saveRegs: BOOLEAN): PROCANY;

PROCEDURE PatchCall(ptr: Word.T; proc: PROCANY);

PROCEDURE GetLinear (proc: PROCANY;
                     nArgs: INTEGER;
                     useClosure: BOOLEAN;
                     closure: REFANY): PROCANY;

PROCEDURE Init();

END MachineStitcher.



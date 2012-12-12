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
 * 16-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Wrapper for machine dependent stitcher functions.
 *)
UNSAFE MODULE MachineStitcher;
IMPORT AliasDesc, MachineStitcherExtern, Word;

PROCEDURE CloneTrampolineStub(func: AliasDesc.T; 
                              offset: INTEGER;
                              saveRegs: BOOLEAN): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneTrampolineStub(func, offset, saveRegs);
  END CloneTrampolineStub;

PROCEDURE CloneDefaultStub(desc: AliasDesc.T; 
                           nArgs: INTEGER; 
                           res: BOOLEAN; 
                           saveRegs: BOOLEAN): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneDefaultStub(desc, nArgs, res, saveRegs);
  END CloneDefaultStub; 


PROCEDURE CloneStub(desc: AliasDesc.T; 
                    nArgs: INTEGER; 
                    res: BOOLEAN; 
                    nHandlers: INTEGER; 
                    saveRegs: BOOLEAN): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneStub(desc, nArgs, res, 
                                           nHandlers, saveRegs);
  END CloneStub; 

PROCEDURE CloneUnrolledStub(desc: AliasDesc.T; 
                            handlers: ADDRESS;
                            nArgs: INTEGER; 
                            res: BOOLEAN; 
                            nHandlers: INTEGER; 
                            saveRegs: BOOLEAN;
                            doGuards: BOOLEAN;
                            guardList: UNTRACED REF Word.T;
                            guardCnt: INTEGER): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneUnrolledStub(
               desc, handlers, nArgs, res, nHandlers, saveRegs, 
               doGuards, guardList, guardCnt);
  END CloneUnrolledStub; 

PROCEDURE CloneInlinedStub(desc: AliasDesc.T; 
                            handlers: ADDRESS;
                            nArgs: INTEGER; 
                            res: BOOLEAN; 
                            nHandlers: INTEGER; 
                            saveRegs: BOOLEAN;
                            doInline: BOOLEAN;
                            trace: BOOLEAN): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneInlinedStub(desc, handlers, nArgs, 
                                                  res, nHandlers, saveRegs,
                                                  doInline, trace);
  END CloneInlinedStub; 

PROCEDURE Init() =
  BEGIN
    MachineStitcherExtern.Init();
  END Init; 

PROCEDURE CloneFromList(list: UNTRACED REF Word.T; cnt: INTEGER;
                        nArgs: INTEGER; res: BOOLEAN; 
                        saveRegs: BOOLEAN): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneFromList(list, cnt, 
                                               nArgs, res, saveRegs);
  END CloneFromList;

PROCEDURE PatchCall(ptr: Word.T; proc: PROCANY) =
  BEGIN
    MachineStitcherExtern.PatchCall(ptr, proc);
  END PatchCall;

PROCEDURE GetLinear (proc: PROCANY;
                     nArgs: INTEGER;
                     useClosure: BOOLEAN;
                     closure: REFANY): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneArgumentStub(proc, 
                                                   nArgs,
                                                   useClosure,
                                                   closure);
  END GetLinear;

BEGIN
END MachineStitcher.

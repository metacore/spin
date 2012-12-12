(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Dec-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added empty PatchCall() and CloneFromList(), and removed Test().
 *
 * 27-Aug-96  Przemek Pardyak (pardy) at the University of Washington
 * 	Until optimizations are written, generate unoptimized stubs
 *	for all optimization levels.
 *
 * 20-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 16-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Wrapper for machine dependent stitcher functions.
 *)
UNSAFE MODULE MachineStitcher;
IMPORT AliasDesc, MachineStitcherExtern, MachineDispatcher, Word, IO;

PROCEDURE CloneTrampolineStub(desc: AliasDesc.T; 
                              offset: INTEGER;
                              saveRegs: BOOLEAN): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneTrampolineStub(desc, offset, saveRegs);
  END CloneTrampolineStub;

PROCEDURE CloneDefaultStub(desc: AliasDesc.T; 
                           <* UNUSED *> nArgs: INTEGER; 
                           <* UNUSED *> res: BOOLEAN;
                           saveRegs: BOOLEAN): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneTrampolineStub(
               desc, MachineDispatcher.DebugDispatchOffset, saveRegs);
  END CloneDefaultStub; 



PROCEDURE CloneStub(desc: AliasDesc.T; 
                    <* UNUSED *> nArgs: INTEGER; 
                    <* UNUSED *> res: BOOLEAN; 
                    <* UNUSED *> nHandlers: INTEGER; 
                    saveRegs: BOOLEAN): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneTrampolineStub(
               desc, MachineDispatcher.DebugDispatchOffset, saveRegs);
  END CloneStub; 

PROCEDURE CloneUnrolledStub(desc: AliasDesc.T; 
                            <* UNUSED *> handlers: ADDRESS;
                            <* UNUSED *> nArgs: INTEGER; 
                            <* UNUSED *> res: BOOLEAN; 
                            <* UNUSED *> nHandlers: INTEGER; 
                            saveRegs: BOOLEAN;
                            <* UNUSED *> doGuards: BOOLEAN;
                            <* UNUSED *> guardList: UNTRACED REF Word.T;
                            <* UNUSED *> guardCnt: INTEGER): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneTrampolineStub(
               desc, MachineDispatcher.DebugDispatchOffset, saveRegs);
  END CloneUnrolledStub; 

PROCEDURE CloneInlinedStub(desc: AliasDesc.T; 
                           <* UNUSED *> handlers: ADDRESS;
                           <* UNUSED *> nArgs: INTEGER; 
                           <* UNUSED *> res: BOOLEAN; 
                           <* UNUSED *> nHandlers: INTEGER; 
                           saveRegs: BOOLEAN;
                           <* UNUSED *> doInline: BOOLEAN;
                           <* UNUSED *> trace: BOOLEAN): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneTrampolineStub(
               desc, MachineDispatcher.DebugDispatchOffset, saveRegs);
  END CloneInlinedStub; 

PROCEDURE Init() =
  BEGIN
    MachineStitcherExtern.Init();
  END Init; 

PROCEDURE PatchCall(<* UNUSED *>ptr: Word.T;<* UNUSED *> proc: PROCANY) =
  BEGIN
    <* ASSERT FALSE *>
  END PatchCall;

PROCEDURE CloneFromList(<* UNUSED *>list: UNTRACED REF Word.T; 
                        <* UNUSED *>cnt: INTEGER;
                        <* UNUSED *>nArgs: INTEGER; <* UNUSED *>res: BOOLEAN; 
                        <* UNUSED *>saveRegs: BOOLEAN): PROCANY =
  BEGIN
    <* ASSERT FALSE *>
  END CloneFromList;

PROCEDURE GetLinear (<*UNUSED*>proc: PROCANY;
                     <*UNUSED*>nArgs: INTEGER;
                     <*UNUSED*>useClosure: BOOLEAN;
                     <*UNUSED*>closure: REFANY): PROCANY =
  BEGIN
    RETURN MachineStitcherExtern.CloneArgumentStub(proc, 
                                                   nArgs,
                                                   useClosure,
                                                   closure);
  END GetLinear;

BEGIN
END MachineStitcher.


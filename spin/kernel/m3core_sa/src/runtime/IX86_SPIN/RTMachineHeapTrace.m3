(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Dec-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added GetPC.
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created.
 *
 *)


UNSAFE MODULE RTMachineHeapTrace;
IMPORT Word, RTIO, RTOSMachine;

CONST
  PCMask = 16_ffffffff;
  PCVal  = 16_00000000;
  RefMask = 16_ffffffff;
  RefVal  = 16_00000000;
  HashMask     = 16_7ffff8;  (* Word.ShiftLeft(MaxObjDesc, 3) *)

PROCEDURE RefAnyToRefT(r: REFANY): RefT =
  BEGIN
    RETURN WordToRefT(LOOPHOLE(r, Word.T));
  END RefAnyToRefT;

PROCEDURE AddrToPCT(pc: ADDRESS): PCT =
  VAR
    pcRef := LOOPHOLE(pc, Word.T);
  BEGIN
    IF Word.And(Word.Not(PCMask), pcRef) # PCVal THEN
      RTIO.PutText("ILLEGAL PC VALUE: "); 
      RTIO.PutHex(pcRef); RTIO.PutText("\n"); 
      RTOSMachine.Debugger();
    END;
    RETURN Word.And(pcRef, PCMask);
  END AddrToPCT; 

PROCEDURE WordToRefT(ref: Word.T): RefT =
  BEGIN
    IF Word.And(Word.Not(RefMask), ref) # RefVal THEN
      RTIO.PutText("ILLEGAL POINTER: "); 
      RTIO.PutHex(ref); RTIO.PutText("\n"); 
      RTOSMachine.Debugger();
    END;
    RETURN Word.And(ref, RefMask);
  END WordToRefT;

PROCEDURE GetRef(ref: RefT): REFANY =
  BEGIN
    RETURN LOOPHOLE(Word.Or(ref, RefVal), REFANY);
  END GetRef;

PROCEDURE GetPC(ref: RefT): ADDRESS =
  BEGIN
    RETURN LOOPHOLE(Word.Or(ref, PCVal), ADDRESS);
  END GetPC;

PROCEDURE Hash(ref: RefT): ObjDesc =
  VAR hash := Word.RightShift(Word.And(ref, HashMask), 3);
  BEGIN
    IF hash = 0 THEN hash := 1; END;
    RETURN hash;
  END Hash;

BEGIN
END RTMachineHeapTrace.

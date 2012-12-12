(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Dec-96  Przemek Pardyak (pardy) at the University of Washington
 *	Whisted. Added GetPC.
 *)

INTERFACE RTMachineHeapTrace;
IMPORT Word;

CONST
(*  MaxObjDesc   = 16_fffff;*)
  MaxObjDesc   = 16_1;  (* Make this 16_1 if your BSS is too big. - savage *)

TYPE
  ObjDesc = [0..MaxObjDesc];

  RefT = Word.T;
  PCT  = Word.T;

TYPE
  ObjState = { Active, Moved, Deallocated };

  ObjDescT = BITS 96 FOR RECORD
    ref: BITS 32 FOR RefT;
    pc:  BITS 32 FOR PCT;
    tc:  BITS 14 FOR [0..16_3fff];
    state: BITS 2 FOR ObjState;
  END;

  PCCnt = RECORD 
    pc   : PCT; 
    cnt  : BITS 32 FOR Word.T;
    size : INTEGER;
    tc   : INTEGER;
  END;

PROCEDURE RefAnyToRefT(r: REFANY): RefT;
PROCEDURE AddrToPCT(pc: ADDRESS): PCT;
PROCEDURE WordToRefT(ref: Word.T): RefT;
PROCEDURE GetRef(ref: RefT): REFANY;
PROCEDURE GetPC(ref: RefT): ADDRESS;
PROCEDURE Hash(ref: RefT): ObjDesc;


END RTMachineHeapTrace.

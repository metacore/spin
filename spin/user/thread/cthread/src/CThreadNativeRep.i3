(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Cthread representation.
 *) 
INTERFACE CThreadNativeRep;
IMPORT CThreadNative, Thread, Space;
IMPORT CodeRegions, CPU, Word;
IMPORT Region;

CONST
  NumRegions = 64;

REVEAL CThreadNative.T = CThreadNative.Public BRANDED OBJECT
  space: Space.T;
  lock: MUTEX;
  fpustate: CPU.FloatRegs;
  fpuused : BOOLEAN := FALSE;
  done: Thread.Condition;
  returned: BOOLEAN := FALSE;
  result: Word.T;
  ctnext: CThreadNative.T;
  ras: CodeRegions.T;
  (* For timings *)
  protectedregions: ARRAY [0..NumRegions] OF Region.T;
  casRestartBegin, casRestartEnd, casRollForwardLength: Word.T;
END;

END CThreadNativeRep.

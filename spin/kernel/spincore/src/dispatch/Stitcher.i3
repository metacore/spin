(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 9-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added exceptions.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Made safe by moving EXTERNS elsewhere. Eliminated cache flushops, 
 *      and (unused) PatchOffset
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 * 
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *
 *)

INTERFACE Stitcher;
IMPORT Dispatcher, AliasDesc, Word;

(*
 * clone a stub
 *)

PROCEDURE CloneStub(desc: AliasDesc.T; 
                    optLevel: INTEGER; 
                    nArgs: INTEGER; 
                    res: BOOLEAN; 
                    saveRegs: BOOLEAN;
                    track: BOOLEAN): PROCANY 
                    RAISES { Dispatcher.Error };

(*
 * optimize a stub
 *)

PROCEDURE Optimize(clone: PROCANY; 
                   optLevel: INTEGER): PROCANY;

PROCEDURE Generate(outProc: PROCANY;
                   inProcs: REF ARRAY OF PROCANY;
                   inPos: REF ARRAY OF ADDRESS): PROCANY;

TYPE
  GuardDesc = RECORD
    guard      : PROCANY;
    closure    : REFANY;
    useClosure : BITS BITSIZE(Word.T) FOR BOOLEAN;
  END;

PROCEDURE GuardOptimizations(guards: REF ARRAY OF GuardDesc;
			     nArgs: INTEGER; 
			     res: BOOLEAN; 
			     saveRegs: BOOLEAN):PROCANY;

PROCEDURE GetLinear (proc: PROCANY;
                     nArgs: INTEGER;
                     useClosure: BOOLEAN;
                     closure: REFANY): PROCANY;

(*
 * initialization 
 *)

PROCEDURE Init();

END Stitcher.

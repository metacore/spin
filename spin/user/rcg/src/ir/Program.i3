(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)


INTERFACE Program;

IMPORT Procedure;
IMPORT IntIntTbl, Ir, Word;
IMPORT Instruction, InstructionTbl, InstructionSeq;

TYPE
  InstArray = UNTRACED REF ARRAY OF Instruction.T;
  WordArray = UNTRACED REF ARRAY OF Word.T;

TYPE
  Index = [0 .. LAST(Ir.MemoryDisplacement) DIV 8];
  T = REF RECORD
    (*
       last is the last index in use in procs
    *)
    disassembling_root := FALSE;
    last   : CARDINAL := 0;
    procs  : ProcArray;

    (* New global table info. *)
    GPsize : Index;
    GPtable: IntIntTbl.T;
    globals : WordArray;

    map : InstructionTbl.T;
    remaining : InstructionSeq.T;
  END;

  ProcArray = REF ARRAY OF Procedure.T;

PROCEDURE NewT () : T;
PROCEDURE AddProc (t: T; p: Procedure.T) : CARDINAL;
PROCEDURE GetNewGPValue(prog: T; key: Word.T): Ir.MemoryDisplacement;

END Program.

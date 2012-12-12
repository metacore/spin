(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 *
 *)


INTERFACE JumpTable;

IMPORT Wr, SortedRCGTbl;
IMPORT IntPairList;

EXCEPTION Duplicate;
EXCEPTION Done;

(*
  a jump table is a map from values to blocks of instructions

  code generation is done within the assembler, in AssemblerJT.m3

  Because Ir.i3 imports this interface, we cannot use
    Ir.InstructionArray in this interface
  Because BasicBlock.i3 imports Ir.i3, we cannot use BasicBlock.T

  jump table needs to be at the end of a basic block -- the successor
    field points to the next basic block
*)

TYPE
  T = REF RECORD
    input: CARDINAL;    (* input register *)
    successor: REFANY;  (* BasicBlock.T *)
    data: SortedRCGTbl.Default;
    done: Canonical;
    else_block: REFANY;       (*Ir.InstructionArray *)
    size : CARDINAL;
  END;

  TableData = REF RECORD
    block: REFANY;  (* Ir.InstructionArray *)
    next:  REFANY;  (* BasicBlock.T        *)
  END;

  DataArray = REF ARRAY OF RECORD
    value: INTEGER;
    block: REFANY;  (* Ir.InstructionArray *)
    next: REFANY;   (* BasicBlock.T        *)
  END;
  
  Canonical = REF RECORD
    (* registers used for jump table
         input: input register
         tmp0, tmp1, tmp2: temporary scratch registers -- use caller-save

         all tmp registers are necessary if a jump table is used
         otherwise, only one scratch register is necessary
     *)
    input, tmp0, tmp1, tmp2: INTEGER;
    successor: REFANY;  (* BasicBlock.T *)
    data: DataArray;
    clustering: ClusterInfo;
    usesTable: BOOLEAN;
    else_block: REFANY; (* Ir.InstructionArray *)
  END;

  Cluster = RECORD
    min: INTEGER;
    intervals: IntPairList.T;
  END;
  Implementation = { Linear, Binary, Table };
  Clusters = REF ARRAY OF Cluster;

  ClusterInfo = REF ARRAY OF RECORD
    lo, hi: INTEGER;
  END;


(* creator
   r is the input register
 *)
PROCEDURE NewT (r: CARDINAL) : T;

(* insert a value *)
PROCEDURE Insert (t: T; value: INTEGER; block: REFANY (*Ir.InstructionArray*);
                  next: REFANY (*BasicBlock.T*) := NIL)
  RAISES {Duplicate, Done};

(* insert an ELSE block *)
PROCEDURE InsertElse (t: T; block: REFANY (*Ir.InstructionArray*))
  RAISES {Duplicate, Done};

(* change to array form for linear access *)
PROCEDURE Canonicalize (t: T) : Canonical;

(* set successor basic block *)
PROCEDURE SetSuccessor (t: T; bb: REFANY (*BasicBlock.T*));
  
(* for debugging *)
EXCEPTION Problem;
PROCEDURE Print (t: T; s: Wr.T := NIL) RAISES {Problem};

(* platform-specific: implemented in AssemblerJT.m3 *)
PROCEDURE ImplementationType (lo, hi: INTEGER) : Implementation;

(* copy it, buddy *)
PROCEDURE Copy (jt: T) : T RAISES {Problem};

END JumpTable.

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


INTERFACE BasicBlock;

IMPORT Ir, Wr;
IMPORT IntSet;

  (****************  basic block types       ************)

  (* A basic block consists of an array of instructions
     and a parallel array of dataflow information
     also, an array of successors
  *)
VAR
  sweepId := 1;

TYPE
  T = REF RECORD
    index: Ir.BasicBlockIndex := 0;

    (* Number of instructions from beginning of procedure to
       beginning of this block.  Used during assembly *)
    offset : CARDINAL := 0;

    (* This variable is used during traversals of the CFG. Every
       sweep gets a unique id number in an increasing sequence.*)
    sweepMarker: CARDINAL := 0;

    (* loop information *)
    isLoopHead, isLoopTail: BOOLEAN := FALSE;
    head, tail: T := NIL; (* The head and tail of the loop the block is in *)
    pre, next: T := NIL;  (* The loop pre-header and the block following 
                             the loop exit *)

    (* instruction info *)
    instructions: Ir.InstructionArray;
    instruction_count: CARDINAL := 0;

    (* dataflow junk *)
    infoTop, infoBottom: Ir.DataflowInfo;
    
    (* connectivity info *)
    successors, predecessors: TArray;
    edges_out: EdgeArray := NIL;

    (* currently unused: register information *)
    registerLow, registerHigh: CARDINAL;
    registerOffset: CARDINAL;  (* move all registers by this offset *)

    (* control-flow information *)
    (* i means immediate *)
    dominators, postdominators: IntSet.T := NIL;
    idominator, ipostdominator: T := NIL;
    dominates, postdominates: IntSet.T := NIL;
    idominates, ipostdominates : IntSet.T := NIL;
    (* set of bbs that properly control this block *)
    cd : IntSet.T := NIL;
    (* set of bbs that immediately (and properly) control this block *)
    icd : RECORD
      bb: T := NIL;
      edge: CARDINAL := 0;
    END;

    (* used for copying *)
    copy : T := NIL;

    (* true if the basic block calls m3_fault *)
    terminal : BOOLEAN := FALSE;

    (* not required: for use in building IR *)
    count: CARDINAL := 0;
  END;

  TArray = REF ARRAY OF T;
  EdgeArray = REF ARRAY OF CARDINAL;

  Proc = PROCEDURE (block: T; arg: REFANY) RAISES ANY;

(* Traverse the block and all of its successors.  Execute p preorder. *)
PROCEDURE Sweep (block: T; p: Proc; arg: REFANY) RAISES ANY;

(* Traverse the block and all of its successors.  Execute p postorder. *)
PROCEDURE SweepPost (block: T; p: Proc; arg: REFANY) RAISES ANY;

(* Traverse the block and all of its predecessors. *)
PROCEDURE ReverseSweep(block: T; p: Proc; arg: REFANY) RAISES ANY;

(* Traverse the block and all of its successors in some topological order.
   Note that back edges from loop tails to heads are not considered real
   edges for the purposes of this sweep. *)
PROCEDURE TopSweep(block: T; p: Proc; arg: REFANY) RAISES ANY;

(* Traverse the start block and all of its successors until you 
   encounter stop. *)
PROCEDURE SweepBetween(start, stop: T; p: Proc; arg: REFANY);

PROCEDURE GetBackEdgePred(block: T): T;
PROCEDURE GetBackEdgeSucc(block: T): T;

EXCEPTION Problem;
PROCEDURE Print (t: T; s: Wr.T := NIL) RAISES {Problem};

(* modifies current field
   current field points to copy if one exists *)
PROCEDURE Copy (t: T) : T;

(* return array that looks like:
   a1[0..offset] a2[0..last(a2)] a1[offset+1..last(a1)] *)   
PROCEDURE InsertArray (a1, a2: TArray; offset: CARDINAL) : TArray;

(* split bb after index and return new one *)
PROCEDURE Split (bb: T; index: INTEGER; p: REFANY (*Procedure.T*))
  : T RAISES {Problem};

EXCEPTION Done;

TYPE
  InstructionProc = PROCEDURE (VAR i: Ir.Instruction; r: REFANY) RAISES ANY;

(* traverse all of the instructions in the block,
   including inside jump tables *)
PROCEDURE InstructionSweep (bb: T; p: InstructionProc; r: REFANY) RAISES ANY;

(* traverse all of the instructions in the block,
   excluding inside jump tables *)
PROCEDURE InstructionSweepShallow (bb: T; p: InstructionProc; r: REFANY)
  RAISES ANY;

END BasicBlock.

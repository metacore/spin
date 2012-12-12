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

INTERFACE Procedure;

IMPORT Wr;
IMPORT BasicBlock;
IMPORT IntSet;

CONST Brand = "Ir Procedure representation";

(* when something cannot be done, raise this *)
EXCEPTION NotYet;

TYPE
  T = REF RECORD
    flags : FlagSet := FlagSet {};
    addr : ADDRESS := NIL;
    index: CARDINAL := 0;

    (* necessary for assembly *)
    table: BasicBlock.TArray := NIL;    (* map integers onto basic blocks *)
    topo: REF ARRAY OF CARDINAL := NIL; (* reverse topological sort order *)
    assembled_size: CARDINAL := 0;
    gp: ADDRESS := NIL;                 (* gp value *)

    (* flow graph information *)
    top, bottom: BasicBlock.T := NIL;   (* top and bottom of flow graph *)
    bottoms: REF ARRAY OF BasicBlock.T := NIL;  (* blocks with ret *)
    edges: EdgeArray := NIL;
    edge_count: CARDINAL := 0;
    defs, uses: REF ARRAY OF IntSet.T;
    has_loops := FALSE;

    instructions: InstructionIndex;     (* map instruction # -> location *)

    frame: CARDINAL := 0;               (* frame size in bytes *)

    (* value returned by function *)
    returns: REFANY := NIL;             (* an Expression.T *)

    (* must disassemble -- called by a root procedure *)
    must_disassemble := FALSE;

    (* register usage *)
    registers: SET OF [0..31];
  END;

  FlagSet = SET OF Flags;
  Flags = { Dominators, Postdominators, ControlDependence,
            Edges, Available, Reaching, Live, Constant, Label,
            Fast, TopoSort };

  InstructionIndex = RECORD
    size: CARDINAL := 0;
    map: IndexArray;
  END;
  IndexArray = REF ARRAY OF BasicBlock.T;

  EdgeArray = REF ARRAY OF Edge;
  Edge = RECORD
    source, dest: BasicBlock.T;
    expr: REFANY; (* really Expression.T *)
  END;

(* sanity checker *)
PROCEDURE Check (t: T) : BOOLEAN;

EXCEPTION Problem;
PROCEDURE Print (t: T; s: Wr.T := NIL) RAISES {Problem};

(* put basic block in order, stripping out fall-through jumps *)
PROCEDURE Linearize (t: T);

(* copy a procedure, add space in bb table for extra basic blocks *)
PROCEDURE Copy (t: T; bbs: CARDINAL) : T RAISES {Problem};

(* execute p on every basic block in the procedure, preorder *)
PROCEDURE Sweep (t: T; p: BasicBlock.Proc; r: REFANY) RAISES {Problem};

(* execute p on every basic block in the procedure, postorder *)
PROCEDURE SweepPost (t: T; p: BasicBlock.Proc; r: REFANY);

(* setup instruction mapping (bb, offset) <-> index *)
PROCEDURE IndexInstruction (VAR ia: InstructionIndex; bb: BasicBlock.T)
  : CARDINAL;

(* add a basic block at index *)
PROCEDURE AddBB (p: T; bb: BasicBlock.T; index: INTEGER);

(* creator *)
PROCEDURE NewT () : T;

(* execute p on every instruction in the procedure, preorder *)
PROCEDURE InstructionSweep (t: T; p: BasicBlock.InstructionProc; r: REFANY)
  RAISES {Problem};

(* return value *)
PROCEDURE Returns (t: T) : REFANY (*Expression.T*) ;

END Procedure.

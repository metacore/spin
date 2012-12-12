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

INTERFACE Analysis;

IMPORT Ir, BasicBlock, Procedure;
IMPORT Residue;

EXCEPTION NotReady;
EXCEPTION Problem;

(*
 * an analysis does what its name indicates: it performs
 * a dataflow analysis
 *)


TYPE
  Direction = { Forward, Backward };

  PublicT = OBJECT METHODS
    Name () : TEXT;
    Direction () : Direction;

    Initialize (c: Ir.Code);
    Cleanup (c: Ir.Code);

    IsLoopStart (bb: BasicBlock.T) : BOOLEAN;
    Converged (r1, r2: Residue.T) : BOOLEAN;
    InvalidateResidueBlock (bb: BasicBlock.T);

    Merge (r1, r2: Residue.T) : Residue.T;
    BestGuess (p: Procedure.T) : Residue.T;
    StartGuess (p: Procedure.T) : Residue.T;
    Union (r1, r2: Residue.T) : Residue.T;

    GetResidueIn (bb: BasicBlock.T) : Residue.T;
    GetResidueOut (bb: BasicBlock.T) : Residue.T;

    SetResidueIn (bb: BasicBlock.T; r: Residue.T);
    SetResidueOut (bb: BasicBlock.T; r: Residue.T);
    
    PrintResidueIn (bb: BasicBlock.T);
    PrintResidueOut (bb: BasicBlock.T);

    (* return the new residue that the instruction i creates *)
    GenKill (i: Ir.Instruction; r: Residue.T; p: Procedure.T) : Residue.T RAISES {Problem};
    Copy (rfrom, rto: Residue.T) : Residue.T;

    SetAllLocals ();
    SetFlags (p: Procedure.T) RAISES { NotReady };
  END;

  T <: PublicT;

END Analysis.

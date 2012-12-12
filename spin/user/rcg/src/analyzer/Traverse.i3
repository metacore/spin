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

INTERFACE Traverse;

IMPORT Analysis, Residue, BasicBlock, BasicBlockQueue, Procedure;

PROCEDURE IDFA_All(start, finish: BasicBlock.T; analysis: Analysis.T;
                   p: Procedure.T)
  RAISES {Analysis.Problem};

PROCEDURE IDFALoop(start: BasicBlock.T; analysis: Analysis.T;
                   p: Procedure.T)
  : BasicBlock.T RAISES {Analysis.Problem};

PROCEDURE GatherBlockSuccessors(block: BasicBlock.T; q: BasicBlockQueue.T;
                                is_forward: BOOLEAN);

PROCEDURE IsLoopStart(block: BasicBlock.T; is_forward: BOOLEAN) : BOOLEAN;

PROCEDURE BackEdgeResidue(analysis: Analysis.T; block: BasicBlock.T)
  : Residue.T;

PROCEDURE CanAnalyze(block: BasicBlock.T; analysis: Analysis.T) : BOOLEAN;

PROCEDURE GetResidueAfter (a: Analysis.T; p: Procedure.T; i: INTEGER)
  : Residue.T RAISES {Analysis.Problem};

PROCEDURE GetResidueBefore (a: Analysis.T; p: Procedure.T; i: INTEGER)
  : Residue.T RAISES {Analysis.Problem};

END Traverse.


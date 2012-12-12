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

INTERFACE Flow;

IMPORT Wr;
IMPORT Expression;
IMPORT BasicBlock, Procedure;

EXCEPTION NoComplement;

(* compute dominators and post-dominators *)
PROCEDURE Dominators (p: Procedure.T);
PROCEDURE Postdominators (p: Procedure.T);

(* number all of the edges *)
PROCEDURE Edges (p: Procedure.T);

(* given an edge number, return the edge that represents the
   "other" control exit from the same basic block
*)
PROCEDURE EdgeComplement (p: Procedure.T; edge: CARDINAL) : CARDINAL;

(* compute control dependence
   requires: post-dominator tree is built
*)
PROCEDURE CD (p: Procedure.T);

(* least common ancestor in dominator tree
   requires: dominator tree is built
*)
PROCEDURE DominatorLCA (bb1, bb2: BasicBlock.T) : BasicBlock.T;

(* least common ancestor in control dependence graph
   requires: control dependence graph tree is built
*)
PROCEDURE ControlLCA (bb1, bb2: BasicBlock.T) : BasicBlock.T;

(* compute the expression that represents the conditions to
   go from bbdom to bb
   requires: flow information is computed
             available expression is computed
 *)
PROCEDURE PathExpression (p: Procedure.T; bb, bbdom: BasicBlock.T;
                          old: Expression.T) : Expression.T;

(* computes topological sort of basic blocks in p *)
PROCEDURE ReverseTopologicalSort (p: Procedure.T);

(* print flow information *)
PROCEDURE Print (p: Procedure.T; s: Wr.T := NIL);

END Flow.

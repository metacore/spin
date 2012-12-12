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

INTERFACE Constant;

(* constant propagation *)

IMPORT Analysis, Residue, BasicBlock;

TYPE
  T <: Analysis.T;
  R <: Residue.T;

(* constructor for T *)
PROCEDURE NewT () : T;

EXCEPTION Problem;

(* optimizations *)
PROCEDURE Optimize (b: BasicBlock.T; arg: REFANY) RAISES {Problem};

END Constant.

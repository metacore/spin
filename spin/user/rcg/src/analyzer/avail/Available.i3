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

INTERFACE Available;

(* Available expression analysis *)

IMPORT Analysis, Residue, Expression;
IMPORT Procedure, Program;

TYPE
  T <: Analysis.T;
  R <: Residue.T;

(* constructor for T *)
PROCEDURE NewT (): T;

(* after computing available expressions, compute calls *)
PROCEDURE LocateCalls (p: Procedure.T; prog: Program.T);

(* compute conditions that govern exit edges *)
PROCEDURE Condition (p: Procedure.T) RAISES {Analysis.Problem};

PROCEDURE GetReg (r: R; num: [0..31]) : Expression.T;

END Available.

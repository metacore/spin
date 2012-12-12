
(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 * 03-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *
 *)


INTERFACE Guard;

IMPORT Procedure, Program;

EXCEPTION Cannot;
EXCEPTION Problem;

(* analyze a guard *)
PROCEDURE AnalyzeGuard (p: PROCANY; program: Program.T) : Procedure.T
  RAISES {Cannot, Problem};

(* take a bunch of guards and combine them *)
PROCEDURE OptimizeAll (procs: REF ARRAY OF PROCANY) : PROCANY
  RAISES {Cannot, Problem};

(* take a bunch of guards and combine them *)
PROCEDURE OptimizeGuards (procs: REF ARRAY OF PROCANY; count: INTEGER)
  : PROCANY RAISES {Cannot, Problem};

END Guard.

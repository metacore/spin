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

INTERFACE Reaching;

(* Reaching definitions analysis *)

IMPORT Procedure;
IMPORT Analysis, Available;

TYPE
  T <: Analysis.T;

(* constructor for T
   n is the number of variables in the program
*)
PROCEDURE NewT (n: CARDINAL): T;


(* compute the expression that represents the result
   available expressions and control flow analysis must
   have been run first

   current assumption is that available expression for v0
   has complete info

   not always true, but it holds for the simple guards we have
*)
PROCEDURE Result (p: Procedure.T; a: Available.T) RAISES {Procedure.NotYet};

END Reaching.


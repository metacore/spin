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

INTERFACE Live;

(* Live variable analysis *)

IMPORT Analysis, Residue;

(* The callee saved registers (9-15), 
   the stack pointer (30) and
   the return address register (26) are live at the end of the procedure.

   We are free to scramble any of the others, because the calling
   convention states that their values are unpredictable upon return.

   these are used in Coalesce
 *)
CONST CalleeSaved
  = ARRAY [1..9] OF INTEGER {9, 10, 11, 12, 13, 14, 15, 26, 30};


TYPE
  T <: Analysis.T;

(* constructor for T
   n is the number of variables
 *)
PROCEDURE NewT(n: CARDINAL): T;

PROCEDURE FindCallerSaveRegister (start: CARDINAL; r: Residue.T)
  : CARDINAL RAISES {Analysis.Problem};

END Live.




(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

(* Utility procs for Alpha AXP architecture *)
INTERFACE Alpha;

PROCEDURE RegName (addr : INTEGER) : TEXT;
(* Return the reg name that stand for reg address "addr". *)

END Alpha.

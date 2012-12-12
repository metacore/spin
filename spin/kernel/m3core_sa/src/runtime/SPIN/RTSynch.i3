(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 05-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Lock-free synchronization support for the runtime.
 *)
INTERFACE RTSynch;

(* These atomic operations return the new value of the variable *)

PROCEDURE Inc(VAR a: INTEGER; delta: INTEGER := 1) : INTEGER;
PROCEDURE Dec(VAR a: INTEGER; delta: INTEGER := 1) : INTEGER;

PROCEDURE IncAddr(VAR a: ADDRESS; delta: INTEGER) : ADDRESS;
PROCEDURE DecAddr(VAR a: ADDRESS; delta: INTEGER) : ADDRESS;

END RTSynch.

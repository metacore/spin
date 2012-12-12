(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 *
 *)

INTERFACE RCG;

IMPORT ParseParams, Wr;

VAR verbose := TRUE;

(* shell command stuff *)
CONST
  CommandName = "rcg";
  CommandHelp = "zap | iptest | linear | jt | kill | verbose";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;

(* run-time code generation *)
PROCEDURE Clone (proc: PROCANY; name: TEXT): PROCANY;

(* initialization *)
PROCEDURE Init (outout: Wr.T);

END RCG.


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

INTERFACE Stdio;
(* simple I/O on Spin *)

IMPORT Wr;

VAR
  stdout: Wr.T;
  stderr: Wr.T;

PROCEDURE Init(output: Wr.T);

END Stdio.

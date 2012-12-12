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

MODULE Stdio;
(* simple I/O on Spin *)

IMPORT Wr;

PROCEDURE Init(output: Wr.T) =
  BEGIN
    stdout := output;
    stderr := output;
  END Init;

BEGIN
END Stdio.

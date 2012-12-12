(*
 *
 * Copyright 1996, 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *)

UNSAFE MODULE Utils;

IMPORT Wr, Stdio, Thread;

<* FATAL Thread.Alerted, Wr.Failure *>

PROCEDURE Error(msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "ERROR >> " & msg & "\n");
  END Error;

PROCEDURE Unimplemented (msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "UNIMPLEMENTED >> " & msg & "\n");
  END Unimplemented;

BEGIN
END Utils.

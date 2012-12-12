(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE Strand;
IMPORT Thread;

PROCEDURE GetCurrent(): T =
  BEGIN
    RETURN Thread.Self();
  END GetCurrent;
PROCEDURE Yield() =
  BEGIN
  END Yield;
BEGIN
END Strand.

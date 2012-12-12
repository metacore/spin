(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE BsdWait;
IMPORT Word, Ctypes;

PROCEDURE StatusCode (x : Ctypes.unsigned_int) : INTEGER =
  BEGIN
    RETURN Word.And(x, 16_FF);
  END StatusCode;

PROCEDURE ExitCode (x : Ctypes.unsigned_int) : INTEGER =
  BEGIN
    RETURN Word.And(Word.RightShift(x, 8), 16_FF);
  END ExitCode;

PROCEDURE Compose(stat, exit : INTEGER) : Ctypes.unsigned_int =
  BEGIN
    RETURN (Word.LeftShift(Word.And(exit, 16_FF), 8)
	    + Word.And(stat, 16_FF));
  END Compose;
BEGIN
END BsdWait.

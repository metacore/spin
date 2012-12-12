(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE Round;
IMPORT CPU;
IMPORT Word;

PROCEDURE Up8 (v: INTEGER) : INTEGER =
  BEGIN
    RETURN Word.And(v-1, Word.Not(7)) + 8;
  END Up8;

PROCEDURE Up (v, boundary: INTEGER) : INTEGER =
  BEGIN
    RETURN Word.And(v-1, Word.Not(boundary-1)) + boundary;
  END Up;

PROCEDURE Down (v, boundary: INTEGER) : INTEGER =
  BEGIN
    RETURN Word.And(v, Word.Not(boundary-1));
  END Down;
  
PROCEDURE UpToPage (v: INTEGER): INTEGER =
  BEGIN
    RETURN Word.And(v-1, Word.Not(CPU.PAGESIZE-1))
           + CPU.PAGESIZE;
  END UpToPage;

PROCEDURE DownToPage (v: INTEGER): INTEGER =
  BEGIN
    RETURN Word.And(v, Word.Not(CPU.PAGESIZE-1));
  END DownToPage;

BEGIN
END Round.

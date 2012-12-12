(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 05-Jan-94  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

MODULE MachineMem;

IMPORT Word;

PROCEDURE BytesToPages(value : Word.T): Word.T = 
  BEGIN
     RETURN(Word.RightShift(value, PGSHIFT));
  END BytesToPages;

PROCEDURE PagesToBytes(value : Word.T): Word.T = 
  BEGIN
    RETURN(Word.LeftShift(value, PGSHIFT));
  END PagesToBytes;

PROCEDURE RoundToPage(value : Word.T): Word.T = 
  BEGIN
    (*
     *	If m3 didn't suck describing unsigned words we'd have:
     *      (value + (PAGESIZE -1)) AND ~(PAGESIZE -1)
     *)
    RETURN(Word.And(Word.Plus(value,Word.Minus(PAGESIZE,1)),
                    Word.Not(Word.Minus(PAGESIZE,1))));
  END RoundToPage;

PROCEDURE TruncToPage(value : Word.T): Word.T = 
  BEGIN
    (*
     *	If m3 didn't suck describing unsigned words we'd have:
     *      (value AND ~(PAGESIZE -1))
     *)
    RETURN(Word.And(value, Word.Not(Word.Minus(PAGESIZE,1))));
  END TruncToPage;

BEGIN
END MachineMem.

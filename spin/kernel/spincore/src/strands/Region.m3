(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 28-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fix to compare such that the arraysort generic sorts in
 *	descending order.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Generic region support, to support restartable atomic
 *      sequences and non-preemptible code regions.
 *)
MODULE Region;
IMPORT Word;

(* The compare function will sort an array in descending order *)
PROCEDURE Compare(READONLY a, b: T) : [-1 .. 1] =
  BEGIN
    IF Word.LT(a.begin, b.begin) THEN RETURN 1;
    ELSIF Word.GT(a.begin, b.begin) THEN RETURN -1;
    ELSE RETURN 0;
    END;
  END Compare;

BEGIN
END Region.

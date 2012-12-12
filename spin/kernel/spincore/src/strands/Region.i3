(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Generic region support, to support restartable atomic
 *      sequences and non-preemptible code regions.
 *)
INTERFACE Region;
IMPORT Word;

TYPE T = RECORD
  begin : Word.T;
  length  : Word.T;
END;

PROCEDURE Compare(READONLY a, b: T) : [-1 .. 1];

END Region.

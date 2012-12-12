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

INTERFACE MachineMem;

IMPORT Word, CPU;

CONST
  PGSHIFT: Word.T = CPU.PGSHIFT;
  PAGESIZE: Word.T = CPU.PAGESIZE;
  PAGEMASK: Word.T = CPU.PAGEMASK;


PROCEDURE BytesToPages(value : Word.T): Word.T;
PROCEDURE PagesToBytes(value : Word.T): Word.T;
PROCEDURE RoundToPage(value : Word.T): Word.T;
PROCEDURE TruncToPage(value : Word.T): Word.T;

END MachineMem.










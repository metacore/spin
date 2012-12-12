
(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)


(*
 * HISTORY
 * 24-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)

INTERFACE HeapData;

IMPORT Word;

CONST Brand = "Heap Data";

TYPE T = RECORD
  heap, stack, global: Word.T := 0;
  alloc, dealloc: [0..16_ffffffff] := 0;
  alloc_size, dealloc_size: Word.T := 0;
END;

END HeapData.


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

INTERFACE PCData;

IMPORT Word;

CONST Brand = "PC Data";

TYPE T = RECORD
  count, size: Word.T := 0;
END;

END PCData.

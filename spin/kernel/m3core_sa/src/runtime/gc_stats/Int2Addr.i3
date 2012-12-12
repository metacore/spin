
(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)


(*
 * HISTORY
 * 3-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)

INTERFACE Int2Addr;

IMPORT Word;

CONST Brand = "Int To Addr";

TYPE T = RECORD
  primary, secondary: Word.T;
  addr: ADDRESS;
END;

PROCEDURE Compare (VAR a, b: T): [-1 .. 1];

END Int2Addr.

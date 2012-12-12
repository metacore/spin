
(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 * 16-Dec-96  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)

INTERFACE IntPair;

CONST Brand = "Integer Pairs";

TYPE T = REF RECORD
  left, right: INTEGER;
END;

PROCEDURE Equal (k1, k2: T): BOOLEAN;

END IntPair.

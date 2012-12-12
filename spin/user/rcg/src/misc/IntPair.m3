
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

MODULE IntPair;

PROCEDURE Equal (k1, k2: T) : BOOLEAN =
  BEGIN
    RETURN k1.left = k2.left AND k1.right = k2.right;
  END Equal;

BEGIN
END IntPair.

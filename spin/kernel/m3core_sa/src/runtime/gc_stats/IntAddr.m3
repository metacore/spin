
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

MODULE IntAddr;

PROCEDURE Compare (VAR a, b: T) : [-1 .. 1] =
  VAR
    aprimary := a.primary;
    bprimary := b.primary;
  BEGIN
    IF aprimary < bprimary THEN
      RETURN -1;
    ELSIF aprimary = bprimary THEN
      RETURN 0;
    ELSE
      RETURN 1;
    END;
  END Compare;

BEGIN
END IntAddr.

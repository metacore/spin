
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

MODULE Int2Addr;

IMPORT Word;

PROCEDURE Compare (VAR a, b: T) : [-1 .. 1] =
  VAR
    aprimary := a.primary;
    bprimary := b.primary;
    asecond, bsecond: Word.T;
  BEGIN
    IF aprimary < bprimary THEN
      RETURN -1;
    ELSIF aprimary = bprimary THEN
      asecond := a.secondary;
      bsecond := b.secondary;
      IF asecond < bsecond THEN
        RETURN -1;
      ELSIF asecond = bsecond THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END;
    ELSE
      RETURN 1;
    END;
  END Compare;

BEGIN
END Int2Addr.

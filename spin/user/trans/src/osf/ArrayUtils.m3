(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE ArrayUtils; (* uses bcopy, bzero *)
IMPORT Cstring;

PROCEDURE Clear (VAR a: ARRAY OF CHAR) =
  BEGIN
    EVAL Cstring.memset(ADR(a[0]), 0, NUMBER(a));
  END Clear;
  
PROCEDURE Set (VAR a: ARRAY OF CHAR; ch: CHAR) =
  BEGIN
    EVAL Cstring.memset(ADR(a[0]), ORD(ch), NUMBER(a));
  END Set;
  
BEGIN
END ArrayUtils.

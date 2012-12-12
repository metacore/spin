(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	bzero and memset are available in m3core
 *
 * 28-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE (* RTMisc is unsafe *)
MODULE ArrayUtils;
IMPORT RTMisc, Cstring, Ctypes;

PROCEDURE Clear (VAR a: ARRAY OF CHAR) =
  BEGIN
    RTMisc.Zero(LOOPHOLE(ADR(a[0]), Ctypes.void_star), NUMBER(a));
  END Clear;
  
PROCEDURE Set (VAR a: ARRAY OF CHAR; ch: CHAR) =
  BEGIN
    EVAL Cstring.memset(LOOPHOLE(ADR(a[0]), Ctypes.void_star), ORD(ch), NUMBER(a));
  END Set;
  
BEGIN
END ArrayUtils.

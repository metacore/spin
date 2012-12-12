(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Kernel and CPU interfaces
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Unsafe conversion interfaces for trusted clients.
 *)
UNSAFE (* our interface is marked unsafe *)
MODULE UnsafeConvert;
IMPORT RTMisc, Ctypes, M3toC;

PROCEDURE BytesToRefArrayOfChar (    cp  : Ctypes.char_star;
                                     len : CARDINAL; 
                                 VAR ra  : REF ARRAY OF CHAR) RAISES {LengthMismatch} =
  BEGIN
    IF ra = NIL THEN 
      ra := NEW(REF ARRAY OF CHAR, len);
    END;
    BytesToArrayOfChar(cp, len, ra^);
  END BytesToRefArrayOfChar;

PROCEDURE BytesToArrayOfChar (    cp  : Ctypes.char_star;
			          len : CARDINAL; 
                              VAR ra  : ARRAY OF CHAR) RAISES {LengthMismatch} =
  BEGIN
    IF len > NUMBER(ra) THEN
      RAISE LengthMismatch;
    END;
    RTMisc.Copy(cp, ADR(ra[FIRST(ra)]), len);
  END BytesToArrayOfChar;

PROCEDURE StoT(s: Ctypes.char_star; <* UNUSED *>len: INTEGER := 0) : TEXT =
  BEGIN
    RETURN M3toC.CopyStoT(s);
  END StoT;

BEGIN
END UnsafeConvert.

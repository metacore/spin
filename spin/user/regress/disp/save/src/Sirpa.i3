(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 15-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

INTERFACE Sirpa;

EXCEPTION
  Exception;

PROCEDURE Sirpa0 ()
                  RAISES { Exception };

PROCEDURE Sirpa1 (a1 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa2 (a1 : INTEGER; a2 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa5 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa15 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER; a11 : INTEGER; a12 : INTEGER; 
                   a13 : INTEGER; a14 : INTEGER; a15 : INTEGER)
                   RAISES { Exception };

END Sirpa.


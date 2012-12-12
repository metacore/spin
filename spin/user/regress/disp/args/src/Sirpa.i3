(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

INTERFACE Sirpa;

EXCEPTION
  Exception;

(*
 * to test passing arguments
 *)

PROCEDURE Sirpa0 ()
                  RAISES { Exception };

PROCEDURE Sirpa1 (a1 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa2 (a1 : INTEGER; a2 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa3 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa4 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa5 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa6 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa7 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa8 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa9 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                  a9 : INTEGER)
                  RAISES { Exception };

PROCEDURE Sirpa10 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER)
                  RAISES { Exception };

END Sirpa.


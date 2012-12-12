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

IMPORT Word;

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



PROCEDURE Linear0 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear1 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear2 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear3 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear4 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear5 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear6 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear7 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear8 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear9 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear10 (VAR a: ARRAY OF Word.T)
                    RAISES { Exception };



PROCEDURE Linear0Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear1Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear2Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear3Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear4Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear5Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear6Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear7Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear8Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear9Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception };

PROCEDURE Linear10Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                    RAISES { Exception };

END Sirpa.


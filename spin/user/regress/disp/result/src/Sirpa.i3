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

VAR
  cnt: INTEGER := 0;

PROCEDURE SirpaTRUE (a1 : INTEGER; a2 : INTEGER): BOOLEAN
                     RAISES { Exception };

PROCEDURE SirpaFALSE (a1 : INTEGER; a2 : INTEGER): BOOLEAN
                      RAISES { Exception };

PROCEDURE SirpaOne (a1 : INTEGER; a2 : INTEGER): INTEGER
                    RAISES { Exception };

PROCEDURE SirpaTwo (a1 : INTEGER; a2 : INTEGER): INTEGER
                    RAISES { Exception };

PROCEDURE SirpaThree (a1 : INTEGER; a2 : INTEGER): INTEGER
                      RAISES { Exception };

PROCEDURE SirpaFour (a1 : INTEGER; a2 : INTEGER): INTEGER
                     RAISES { Exception };

PROCEDURE SirpaVoid (a1 : INTEGER; a2 : INTEGER)
                     RAISES { Exception };

PROCEDURE SirpaVoidToo (a1 : INTEGER; a2 : INTEGER)
                        RAISES { Exception };

END Sirpa.


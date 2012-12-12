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

MODULE Sirpa;
IMPORT IO, Fmt;

PROCEDURE SirpaTRUE (a1 : INTEGER; a2 : INTEGER): BOOLEAN
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> SirpaTRUE : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
    RETURN TRUE;
  END SirpaTRUE;

PROCEDURE SirpaFALSE (a1 : INTEGER; a2 : INTEGER): BOOLEAN
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> SirpaFALSE : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
    RETURN FALSE;
  END SirpaFALSE;

PROCEDURE SirpaOne (a1 : INTEGER; a2 : INTEGER): INTEGER
                    RAISES { Exception } =
  BEGIN
    IO.Put ("----> SirpaOne : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 AND a1 # 333 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
    RETURN 1;
  END SirpaOne;

PROCEDURE SirpaTwo (a1 : INTEGER; a2 : INTEGER): INTEGER
                    RAISES { Exception } =
  BEGIN
    IO.Put ("----> SirpaTwo : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 AND a1 # 333 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
    RETURN 2;
  END SirpaTwo;

PROCEDURE SirpaThree (a1 : INTEGER; a2 : INTEGER): INTEGER
                      RAISES { Exception } =
  BEGIN
    IO.Put ("----> SirpaThree : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 AND a1 # 333 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
    RETURN 3;
  END SirpaThree;

PROCEDURE SirpaFour (a1 : INTEGER; a2 : INTEGER): INTEGER
                     RAISES { Exception } =
  BEGIN
    IO.Put ("----> SirpaFour : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 AND a1 # 333 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
    RETURN 4;
  END SirpaFour;

PROCEDURE SirpaVoid (a1 : INTEGER; a2 : INTEGER)
                     RAISES { Exception } =
  BEGIN
    IO.Put ("----> SirpaVoid : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 AND a1 # 333 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
  END SirpaVoid;

PROCEDURE SirpaVoidToo (a1 : INTEGER; a2 : INTEGER)
                     RAISES { Exception } =
  BEGIN
    IO.Put ("----> SirpaVoidToo : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 AND a1 # 333 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
  END SirpaVoidToo;

BEGIN
END Sirpa.

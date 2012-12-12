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

PROCEDURE Sirpa2 (a1 : INTEGER; a2 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa2 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
  END Sirpa2;

PROCEDURE Sirpa3 (a1 : INTEGER; a2 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa3 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    INC(cnt);
  END Sirpa3;

BEGIN
END Sirpa.

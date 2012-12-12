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

MODULE Sirpa;
IMPORT IO, Fmt;

PROCEDURE Sirpa0 ()
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa0 <----\n" );
    IF FALSE THEN RAISE Exception; END;
  END Sirpa0;

PROCEDURE Sirpa1 (a1 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa1 : " );
    IO.Put (Fmt.Int (a1) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
  END Sirpa1;

PROCEDURE Sirpa2 (a1 : INTEGER; a2 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa2 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
  END Sirpa2;

PROCEDURE Sirpa5 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa5 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    IF a3 # 333 THEN RAISE Exception; END;
    IF a4 # 444 THEN RAISE Exception; END;
    IF a5 # 555 THEN RAISE Exception; END;
  END Sirpa5;

PROCEDURE Sirpa15 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER; a11 : INTEGER; a12 : INTEGER; 
                   a13 : INTEGER; a14 : INTEGER; a15 : INTEGER)
                   RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa10 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & " ");
    IO.Put (Fmt.Int (a6) & " ");
    IO.Put (Fmt.Int (a7) & " ");
    IO.Put (Fmt.Int (a8) & " ");
    IO.Put (Fmt.Int (a9) & " ");
    IO.Put (Fmt.Int (a10) & " ");
    IO.Put (Fmt.Int (a11) & " ");
    IO.Put (Fmt.Int (a12) & " ");
    IO.Put (Fmt.Int (a13) & " ");
    IO.Put (Fmt.Int (a14) & " ");
    IO.Put (Fmt.Int (a15) & " ");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    IF a3 # 333 THEN RAISE Exception; END;
    IF a4 # 444 THEN RAISE Exception; END;
    IF a5 # 555 THEN RAISE Exception; END;
    IF a6 # 666 THEN RAISE Exception; END;
    IF a7 # 777 THEN RAISE Exception; END;
    IF a8 # 888 THEN RAISE Exception; END;
    IF a9 # 999 THEN RAISE Exception; END;
    IF a10 # 101010 THEN RAISE Exception; END;
    IF a11 # 111111 THEN RAISE Exception; END;
    IF a12 # 121212 THEN RAISE Exception; END;
    IF a13 # 131313 THEN RAISE Exception; END;
    IF a14 # 141414 THEN RAISE Exception; END;
    IF a15 # 151515 THEN RAISE Exception; END;
  END Sirpa15;

BEGIN
END Sirpa.

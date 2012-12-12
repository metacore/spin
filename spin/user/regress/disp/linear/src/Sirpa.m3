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

IMPORT IO, Fmt, Word;

PROCEDURE Sirpa0 ()
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa0\n" );
    (* this code just to get rid of the warning that exception is not raised *)
    IF FALSE THEN
      RAISE Exception; 
    END;
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

PROCEDURE Sirpa3 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa3 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    IF a3 # 333 THEN RAISE Exception; END;
  END Sirpa3;

PROCEDURE Sirpa4 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa4 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    IF a3 # 333 THEN RAISE Exception; END;
    IF a4 # 444 THEN RAISE Exception; END;
  END Sirpa4;

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

PROCEDURE Sirpa6 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa6 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & " ");
    IO.Put (Fmt.Int (a6) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    IF a3 # 333 THEN RAISE Exception; END;
    IF a4 # 444 THEN RAISE Exception; END;
    IF a5 # 555 THEN RAISE Exception; END;
    IF a6 # 666 THEN RAISE Exception; END;
  END Sirpa6;

PROCEDURE Sirpa7 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa7 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & " ");
    IO.Put (Fmt.Int (a6) & " ");
    IO.Put (Fmt.Int (a7) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    IF a3 # 333 THEN RAISE Exception; END;
    IF a4 # 444 THEN RAISE Exception; END;
    IF a5 # 555 THEN RAISE Exception; END;
    IF a6 # 666 THEN RAISE Exception; END;
    IF a7 # 777 THEN RAISE Exception; END;
  END Sirpa7;

PROCEDURE Sirpa8 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa8 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & " ");
    IO.Put (Fmt.Int (a6) & " ");
    IO.Put (Fmt.Int (a7) & " ");
    IO.Put (Fmt.Int (a8) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    IF a3 # 333 THEN RAISE Exception; END;
    IF a4 # 444 THEN RAISE Exception; END;
    IF a5 # 555 THEN RAISE Exception; END;
    IF a6 # 666 THEN RAISE Exception; END;
    IF a7 # 777 THEN RAISE Exception; END;
    IF a8 # 888 THEN RAISE Exception; END;
  END Sirpa8;

PROCEDURE Sirpa9 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                  a9 : INTEGER)
                  RAISES { Exception } =
  BEGIN
    IO.Put ("----> Sirpa9 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & " ");
    IO.Put (Fmt.Int (a6) & " ");
    IO.Put (Fmt.Int (a7) & " ");
    IO.Put (Fmt.Int (a8) & " ");
    IO.Put (Fmt.Int (a9) & "\n");
    IF a1 # 111 THEN RAISE Exception; END;
    IF a2 # 222 THEN RAISE Exception; END;
    IF a3 # 333 THEN RAISE Exception; END;
    IF a4 # 444 THEN RAISE Exception; END;
    IF a5 # 555 THEN RAISE Exception; END;
    IF a6 # 666 THEN RAISE Exception; END;
    IF a7 # 777 THEN RAISE Exception; END;
    IF a8 # 888 THEN RAISE Exception; END;
    IF a9 # 999 THEN RAISE Exception; END;
  END Sirpa9;

PROCEDURE Sirpa10 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER)
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
    IO.Put (Fmt.Int (a10) & "\n");
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
  END Sirpa10;

PROCEDURE Linear0 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 0 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear0\n" );
    (* this code just to get rid of the warning that exception is not raised *)
    IF FALSE THEN
      RAISE Exception; 
    END;
  END Linear0;

PROCEDURE Linear1 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 1 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear1 : " );
    IO.Put (Fmt.Int (a[0]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
  END Linear1;

PROCEDURE Linear2 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 2 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear2 : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
  END Linear2;

PROCEDURE Linear3 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 3 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear3 : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
  END Linear3;

PROCEDURE Linear4 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 4 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear4 : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
  END Linear4;

PROCEDURE Linear5 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 5 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear5 : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
  END Linear5;

PROCEDURE Linear6 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 6 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear6 : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
  END Linear6;

PROCEDURE Linear7 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 7 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear7 : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & " ");
    IO.Put (Fmt.Int (a[6]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
    IF a[6] # 777 THEN RAISE Exception; END;
  END Linear7;

PROCEDURE Linear8 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 8 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear8 : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & " ");
    IO.Put (Fmt.Int (a[6]) & " ");
    IO.Put (Fmt.Int (a[7]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
    IF a[6] # 777 THEN RAISE Exception; END;
    IF a[7] # 888 THEN RAISE Exception; END;
  END Linear8;

PROCEDURE Linear9 (VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 9 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear9 : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & " ");
    IO.Put (Fmt.Int (a[6]) & " ");
    IO.Put (Fmt.Int (a[7]) & " ");
    IO.Put (Fmt.Int (a[8]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
    IF a[6] # 777 THEN RAISE Exception; END;
    IF a[7] # 888 THEN RAISE Exception; END;
    IF a[8] # 999 THEN RAISE Exception; END;
  END Linear9;

PROCEDURE Linear10 (VAR a: ARRAY OF Word.T)
                    RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 10 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear10 : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & " ");
    IO.Put (Fmt.Int (a[6]) & " ");
    IO.Put (Fmt.Int (a[7]) & " ");
    IO.Put (Fmt.Int (a[8]) & " ");
    IO.Put (Fmt.Int (a[9]) & "\n");
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
    IF a[6] # 777 THEN RAISE Exception; END;
    IF a[7] # 888 THEN RAISE Exception; END;
    IF a[8] # 999 THEN RAISE Exception; END;
    IF a[9] # 101010 THEN RAISE Exception; END;
  END Linear10;




PROCEDURE Linear0Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 0 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear0Cl : " );
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
  END Linear0Cl;

PROCEDURE Linear1Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 1 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear1Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
  END Linear1Cl;

PROCEDURE Linear2Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 2 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear2Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
  END Linear2Cl;

PROCEDURE Linear3Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 3 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear3Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
  END Linear3Cl;

PROCEDURE Linear4Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 4 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear4Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
  END Linear4Cl;

PROCEDURE Linear5Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 5 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear5Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
  END Linear5Cl;

PROCEDURE Linear6Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 6 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear6Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
  END Linear6Cl;

PROCEDURE Linear7Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 7 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear7Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & " ");
    IO.Put (Fmt.Int (a[6]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
    IF a[6] # 777 THEN RAISE Exception; END;
  END Linear7Cl;

PROCEDURE Linear8Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 8 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear8Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & " ");
    IO.Put (Fmt.Int (a[6]) & " ");
    IO.Put (Fmt.Int (a[7]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
    IF a[6] # 777 THEN RAISE Exception; END;
    IF a[7] # 888 THEN RAISE Exception; END;
  END Linear8Cl;

PROCEDURE Linear9Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                   RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 9 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear9Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & " ");
    IO.Put (Fmt.Int (a[6]) & " ");
    IO.Put (Fmt.Int (a[7]) & " ");
    IO.Put (Fmt.Int (a[8]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
    IF a[6] # 777 THEN RAISE Exception; END;
    IF a[7] # 888 THEN RAISE Exception; END;
    IF a[8] # 999 THEN RAISE Exception; END;
  END Linear9Cl;

PROCEDURE Linear10Cl (cl: REF INTEGER; VAR a: ARRAY OF Word.T)
                    RAISES { Exception } =
  BEGIN
    IF NUMBER(a) # 10 THEN
      RAISE Exception;
    END;
    IO.Put ("----> Linear10Cl : " );
    IO.Put (Fmt.Int (a[0]) & " ");
    IO.Put (Fmt.Int (a[1]) & " ");
    IO.Put (Fmt.Int (a[2]) & " ");
    IO.Put (Fmt.Int (a[3]) & " ");
    IO.Put (Fmt.Int (a[4]) & " ");
    IO.Put (Fmt.Int (a[5]) & " ");
    IO.Put (Fmt.Int (a[6]) & " ");
    IO.Put (Fmt.Int (a[7]) & " ");
    IO.Put (Fmt.Int (a[8]) & " ");
    IO.Put (Fmt.Int (a[9]) & " ");
    IO.Put (Fmt.Int (cl^) & "\n");
    IF cl^ # 12345 THEN
      RAISE Exception; 
    END;
    IF a[0] # 111 THEN RAISE Exception; END;
    IF a[1] # 222 THEN RAISE Exception; END;
    IF a[2] # 333 THEN RAISE Exception; END;
    IF a[3] # 444 THEN RAISE Exception; END;
    IF a[4] # 555 THEN RAISE Exception; END;
    IF a[5] # 666 THEN RAISE Exception; END;
    IF a[6] # 777 THEN RAISE Exception; END;
    IF a[7] # 888 THEN RAISE Exception; END;
    IF a[8] # 999 THEN RAISE Exception; END;
    IF a[9] # 101010 THEN RAISE Exception; END;
  END Linear10Cl;

BEGIN
END Sirpa.

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
IMPORT IO, Fmt, Thread;

PROCEDURE Sirpa0 () =
  BEGIN
    IO.Put("----> Sirpa0\n" );
    Inc();
  END Sirpa0;

PROCEDURE Sirpa1 (a1 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa1 : " );
    IO.Put(Fmt.Int (a1) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    Inc();
  END Sirpa1;

PROCEDURE Sirpa2 (a1 : INTEGER; a2 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa2 : " );
    IO.Put(Fmt.Int (a1) & " ");
    IO.Put(Fmt.Int (a2) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    IF a2 # 222 THEN Result := FALSE; END;
    Inc();
  END Sirpa2;

PROCEDURE Sirpa3 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa3 : " );
    IO.Put(Fmt.Int (a1) & " ");
    IO.Put(Fmt.Int (a2) & " ");
    IO.Put(Fmt.Int (a3) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    IF a2 # 222 THEN Result := FALSE; END;
    IF a3 # 333 THEN Result := FALSE; END;
    Inc();
  END Sirpa3;

PROCEDURE Sirpa4 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa4 : " );
    IO.Put(Fmt.Int (a1) & " ");
    IO.Put(Fmt.Int (a2) & " ");
    IO.Put(Fmt.Int (a3) & " ");
    IO.Put(Fmt.Int (a4) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    IF a2 # 222 THEN Result := FALSE; END;
    IF a3 # 333 THEN Result := FALSE; END;
    IF a4 # 444 THEN Result := FALSE; END;
    Inc();
  END Sirpa4;

PROCEDURE Sirpa5 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa5 : " );
    IO.Put(Fmt.Int (a1) & " ");
    IO.Put(Fmt.Int (a2) & " ");
    IO.Put(Fmt.Int (a3) & " ");
    IO.Put(Fmt.Int (a4) & " ");
    IO.Put(Fmt.Int (a5) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    IF a2 # 222 THEN Result := FALSE; END;
    IF a3 # 333 THEN Result := FALSE; END;
    IF a4 # 444 THEN Result := FALSE; END;
    IF a5 # 555 THEN Result := FALSE; END;
    Inc();
  END Sirpa5;

PROCEDURE Sirpa6 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa6 : " );
    IO.Put(Fmt.Int (a1) & " ");
    IO.Put(Fmt.Int (a2) & " ");
    IO.Put(Fmt.Int (a3) & " ");
    IO.Put(Fmt.Int (a4) & " ");
    IO.Put(Fmt.Int (a5) & " ");
    IO.Put(Fmt.Int (a6) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    IF a2 # 222 THEN Result := FALSE; END;
    IF a3 # 333 THEN Result := FALSE; END;
    IF a4 # 444 THEN Result := FALSE; END;
    IF a5 # 555 THEN Result := FALSE; END;
    IF a6 # 666 THEN Result := FALSE; END;
    Inc();
  END Sirpa6;

PROCEDURE Sirpa7 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa7 : " );
    IO.Put(Fmt.Int (a1) & " ");
    IO.Put(Fmt.Int (a2) & " ");
    IO.Put(Fmt.Int (a3) & " ");
    IO.Put(Fmt.Int (a4) & " ");
    IO.Put(Fmt.Int (a5) & " ");
    IO.Put(Fmt.Int (a6) & " ");
    IO.Put(Fmt.Int (a7) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    IF a2 # 222 THEN Result := FALSE; END;
    IF a3 # 333 THEN Result := FALSE; END;
    IF a4 # 444 THEN Result := FALSE; END;
    IF a5 # 555 THEN Result := FALSE; END;
    IF a6 # 666 THEN Result := FALSE; END;
    IF a7 # 777 THEN Result := FALSE; END;
    Inc();
  END Sirpa7;

PROCEDURE Sirpa8 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa8 : " );
    IO.Put(Fmt.Int (a1) & " ");
    IO.Put(Fmt.Int (a2) & " ");
    IO.Put(Fmt.Int (a3) & " ");
    IO.Put(Fmt.Int (a4) & " ");
    IO.Put(Fmt.Int (a5) & " ");
    IO.Put(Fmt.Int (a6) & " ");
    IO.Put(Fmt.Int (a7) & " ");
    IO.Put(Fmt.Int (a8) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    IF a2 # 222 THEN Result := FALSE; END;
    IF a3 # 333 THEN Result := FALSE; END;
    IF a4 # 444 THEN Result := FALSE; END;
    IF a5 # 555 THEN Result := FALSE; END;
    IF a6 # 666 THEN Result := FALSE; END;
    IF a7 # 777 THEN Result := FALSE; END;
    IF a8 # 888 THEN Result := FALSE; END;
    Inc();
  END Sirpa8;

PROCEDURE Sirpa9 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                  a9 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa9 : " );
    IO.Put(Fmt.Int (a1) & " ");
    IO.Put(Fmt.Int (a2) & " ");
    IO.Put(Fmt.Int (a3) & " ");
    IO.Put(Fmt.Int (a4) & " ");
    IO.Put(Fmt.Int (a5) & " ");
    IO.Put(Fmt.Int (a6) & " ");
    IO.Put(Fmt.Int (a7) & " ");
    IO.Put(Fmt.Int (a8) & " ");
    IO.Put(Fmt.Int (a9) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    IF a2 # 222 THEN Result := FALSE; END;
    IF a3 # 333 THEN Result := FALSE; END;
    IF a4 # 444 THEN Result := FALSE; END;
    IF a5 # 555 THEN Result := FALSE; END;
    IF a6 # 666 THEN Result := FALSE; END;
    IF a7 # 777 THEN Result := FALSE; END;
    IF a8 # 888 THEN Result := FALSE; END;
    IF a9 # 999 THEN Result := FALSE; END;
    Inc();
  END Sirpa9;

PROCEDURE Sirpa10 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER) =
  BEGIN
    IO.Put("----> Sirpa10 : " );
    IO.Put(Fmt.Int (a1) & " ");
    IO.Put(Fmt.Int (a2) & " ");
    IO.Put(Fmt.Int (a3) & " ");
    IO.Put(Fmt.Int (a4) & " ");
    IO.Put(Fmt.Int (a5) & " ");
    IO.Put(Fmt.Int (a6) & " ");
    IO.Put(Fmt.Int (a7) & " ");
    IO.Put(Fmt.Int (a8) & " ");
    IO.Put(Fmt.Int (a9) & " ");
    IO.Put(Fmt.Int (a10) & "\n");
    IF a1 # 111 THEN Result := FALSE; END;
    IF a2 # 222 THEN Result := FALSE; END;
    IF a3 # 333 THEN Result := FALSE; END;
    IF a4 # 444 THEN Result := FALSE; END;
    IF a5 # 555 THEN Result := FALSE; END;
    IF a6 # 666 THEN Result := FALSE; END;
    IF a7 # 777 THEN Result := FALSE; END;
    IF a8 # 888 THEN Result := FALSE; END;
    IF a9 # 999 THEN Result := FALSE; END;
    IF a10 # 101010 THEN Result := FALSE; END;
    Inc();
  END Sirpa10;

PROCEDURE Inc () =
  BEGIN
    LOCK MuArgs DO
      INC(Cnt); 
      Thread.Signal(CondArgs);
    END;
  END Inc;

PROCEDURE Asynch () =
  BEGIN
    LOCK MuAsynch DO
      IF NOT AsynchOK THEN
        Thread.Wait(MuAsynch, CondAsynch1);
      END;
      IO.Put("This line should be printed third\n");
      Thread.Signal(CondAsynch2);
    END;
  END Asynch;

BEGIN
  MuArgs := NEW(MUTEX);
  CondArgs := NEW(Thread.Condition);
  MuAsynch := NEW(MUTEX);
  CondAsynch1 := NEW(Thread.Condition);
  CondAsynch2 := NEW(Thread.Condition);
END Sirpa.

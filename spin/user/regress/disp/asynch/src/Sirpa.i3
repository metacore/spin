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
IMPORT Thread;

(*
 * for testing whether this is actually asynchronous
 *)

VAR
  MuAsynch: MUTEX;
  CondAsynch1: Thread.Condition;
  CondAsynch2: Thread.Condition;
  AsynchOK: BOOLEAN;

PROCEDURE Asynch ();

(*
 * to test passing arguments
 *)

VAR
  Result: BOOLEAN;
  Cnt: INTEGER;
  MuArgs: MUTEX;
  CondArgs: Thread.Condition;

PROCEDURE Sirpa0 ();

PROCEDURE Sirpa1 (a1 : INTEGER);

PROCEDURE Sirpa2 (a1 : INTEGER; a2 : INTEGER);

PROCEDURE Sirpa3 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER);

PROCEDURE Sirpa4 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER);

PROCEDURE Sirpa5 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER);

PROCEDURE Sirpa6 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER);

PROCEDURE Sirpa7 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER);

PROCEDURE Sirpa8 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER);

PROCEDURE Sirpa9 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                  a9 : INTEGER);

PROCEDURE Sirpa10 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER);


END Sirpa.


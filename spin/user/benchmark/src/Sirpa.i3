(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 *)
INTERFACE Sirpa;

PROCEDURE E0 ();

PROCEDURE E1 (a1 : INTEGER);

PROCEDURE E2 (a1 : INTEGER; a2 : INTEGER);

PROCEDURE E3 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER);

PROCEDURE E4 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER);

PROCEDURE E5 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER);

PROCEDURE E6 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER);

PROCEDURE EA (a1 : REF ARRAY [1..1024] OF INTEGER);

PROCEDURE E7 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER);

PROCEDURE E8 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER);

PROCEDURE E9 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                  a9 : INTEGER);

PROCEDURE E10 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER);

VAR
  min, max, acc: INTEGER;

PROCEDURE Foo(t1: INTEGER);

END Sirpa.


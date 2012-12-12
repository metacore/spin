(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel and MachineCPU interfaces
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 *)
MODULE Sirpa;
IMPORT CPU, Fmt, IO;

PROCEDURE E0 () =
  BEGIN
  END E0;

PROCEDURE E1 (<* UNUSED *> a1 : INTEGER) =
  BEGIN
  END E1;

PROCEDURE E2 (<* UNUSED *> a1 : INTEGER; <* UNUSED *> a2 : INTEGER) =
  BEGIN
  END E2;

PROCEDURE E3 (<* UNUSED *> a1 : INTEGER; <* UNUSED *> a2 : INTEGER; <* UNUSED *> a3 : INTEGER) =
  BEGIN
  END E3;

PROCEDURE E4 (<* UNUSED *> a1 : INTEGER; <* UNUSED *> a2 : INTEGER; <* UNUSED *> a3 : INTEGER; <* UNUSED *> a4 : INTEGER) =
  BEGIN
  END E4;

PROCEDURE E5 (<* UNUSED *> a1 : INTEGER; <* UNUSED *> a2 : INTEGER; <* UNUSED *> a3 : INTEGER; <* UNUSED *> a4 : INTEGER; 
                  <* UNUSED *> a5 : INTEGER) =
  BEGIN
  END E5;

PROCEDURE E6 (<* UNUSED *> a1 : INTEGER; <* UNUSED *> a2 : INTEGER; <* UNUSED *> a3 : INTEGER; <* UNUSED *> a4 : INTEGER; 
                  <* UNUSED *> a5 : INTEGER; <* UNUSED *> a6 : INTEGER) =
  BEGIN
  END E6;

PROCEDURE EA (<*UNUSED*>a1 : REF ARRAY [1..1024] OF INTEGER) =
  BEGIN
  END EA;

PROCEDURE E7 (<* UNUSED *> a1 : INTEGER; <* UNUSED *> a2 : INTEGER; <* UNUSED *> a3 : INTEGER; <* UNUSED *> a4 : INTEGER; 
                  <* UNUSED *> a5 : INTEGER; <* UNUSED *> a6 : INTEGER; <* UNUSED *> a7 : INTEGER) =
  BEGIN
  END E7;

PROCEDURE E8 (<* UNUSED *> a1 : INTEGER; <* UNUSED *> a2 : INTEGER; <* UNUSED *> a3 : INTEGER; <* UNUSED *> a4 : INTEGER; 
                  <* UNUSED *> a5 : INTEGER; <* UNUSED *> a6 : INTEGER; <* UNUSED *> a7 : INTEGER; <* UNUSED *> a8 : INTEGER) =
  BEGIN
  END E8;

PROCEDURE E9 (<* UNUSED *> a1 : INTEGER; <* UNUSED *> a2 : INTEGER; <* UNUSED *> a3 : INTEGER; <* UNUSED *> a4 : INTEGER; 
                  <* UNUSED *> a5 : INTEGER; <* UNUSED *> a6 : INTEGER; <* UNUSED *> a7 : INTEGER; <* UNUSED *> a8 : INTEGER; 
                  <* UNUSED *> a9 : INTEGER) =
  BEGIN
  END E9;

PROCEDURE E10 (<* UNUSED *> a1 : INTEGER; <* UNUSED *> a2 : INTEGER; <* UNUSED *> a3 : INTEGER; <* UNUSED *> a4 : INTEGER; 
                   <* UNUSED *> a5 : INTEGER; <* UNUSED *> a6 : INTEGER; <* UNUSED *> a7 : INTEGER; <* UNUSED *> a8 : INTEGER; 
                   <* UNUSED *> a9 : INTEGER; <* UNUSED *> a10 : INTEGER) =
  BEGIN
  END E10;

VAR
  i := 0;

PROCEDURE Foo(t1: INTEGER) =
  VAR
    d, ave, t2: INTEGER;
  BEGIN
    t2 := CPU.CycleCounter();
    d := t2-t1; 
    min := MIN(min, d);
    max := MAX(max, d);
    acc := acc + d;
    INC(i);
    IF i = 100 THEN
      ave := acc DIV 100;
      IO.Put("min: " & Fmt.Int(min) & " max: " & Fmt.Int(max) &
        " ave: " & Fmt.Int(ave) & 
        " res: " & Fmt.Int((acc DIV 100) * 1000 DIV 133) & "\n" );
      i := 0;
      min := LAST(INTEGER);
      max := FIRST(INTEGER);
      acc := 0;
    END;
  END Foo;


BEGIN
END Sirpa.

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

(*
 * some random record types
 *)

TYPE 
  RRT1 = REF RECORD 
    i1 : INTEGER;
    i2 : INTEGER;
    next : RRT1;
  END;
  
  RRT2 = REF RECORD 
    i1 : INTEGER;
    i2 : INTEGER;
    i3 : INTEGER;
    i4 : INTEGER;
  END;
  
(*
 * some random procedures
 *)

PROCEDURE Qpa1 (a: RRT1; b: RRT2): RRT1;

PROCEDURE Qpa2 (a: RRT1; b: REF RRT2): RRT1;

PROCEDURE Qpa3 (a: RRT1; VAR b: RRT2): RRT1;

PROCEDURE Qpa4 (a: RRT1; READONLY b: RRT2): RRT1;

PROCEDURE Qpa5 (a: RRT1; b: RRT2): RRT2;

PROCEDURE Qpa6 (a: RRT1; b: RRT2);

END Sirpa.


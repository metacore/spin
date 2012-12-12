(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 10-Nov-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

INTERFACE Performance;

CONST
  Brand  = "Performance";
  N_ITER = 1000;          (* Number of iterations for each test *)

TYPE
  SecureT <: REFANY;

PROCEDURE Run();
  (* Run all performance tests. *)

PROCEDURE Null();

PROCEDURE Null1();
PROCEDURE Null2();
PROCEDURE Null3();
PROCEDURE Null4();
PROCEDURE Null5();
PROCEDURE Null6();

PROCEDURE One  ( a1                             : SecureT );
PROCEDURE Two  ( a1, a2                         : SecureT );
PROCEDURE Four ( a1, a2, a3, a4                 : SecureT );
PROCEDURE Eight( a1, a2, a3, a4, a5, a6, a7, a8 : SecureT );

END Performance.

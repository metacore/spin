(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)


INTERFACE Register;

    
TYPE
  T = 
    {
      v0,
      t0,
      t1,
      t2,
      t3,
      t4,
      t5,
      t6,
      t7,
      s0,
      s1,
      s2,
      s3,
      s4,
      s5,
      fp,
      a0,
      a1,
      a2,
      a3,
      a4,
      a5,
      t8,
      t9,
      t10,
      t11,
      ra,
      t12,
      at,
      gp,
      sp,
      zero
  };

CONST
  Names = ARRAY T OF TEXT {
    "v0",
    "t0",
    "t1",
    "t2",
    "t3",
    "t4",
    "t5",
    "t6",
    "t7",
    "s0",
    "s1",
    "s2",
    "s3",
    "s4",
    "s5",
    "fp",
    "a0",
    "a1",
    "a2",
    "a3",
    "a4",
    "a5",
    "t8",
    "t9",
    "t10",
    "t11",
    "ra",
    "t12",
    "at",
    "gp",
    "sp",
    "zero"
  };

CONST
  caller_save = ARRAY [0 .. 13] OF T { T.v0, T.t0, T.t1,
                                       T.t2, T.t3, T.t4,
                                       T.t5, T.t6, T.t7,
                                       T.t8, T.t9, T.t10,
                                       T.t11, T.t12 };

  callee_save = ARRAY [0 .. 5] OF T { T.s0, T.s1, T.s2,
                                      T.s3, T.s4, T.s5 };

EXCEPTION Cannot;

PROCEDURE FindCallerSave (used: SET OF [0..31]; ignore: INTEGER := 0)
  : [0..31] RAISES {Cannot};

PROCEDURE FindCalleeSave (used: SET OF [0..31]; ignore: INTEGER := 0)
  : [0..31] RAISES {Cannot};

END Register.

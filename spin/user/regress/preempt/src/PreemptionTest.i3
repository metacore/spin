(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 08-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

INTERFACE PreemptionTest;
IMPORT Regress;

CONST
  TestName = "preempt";
  TestHelp = "tests preemption - hangs in error case";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "Ping-pong", T1 }
  };

CONST
  (*
     This test should be run a few times in a row.
   *)
  nIterations = 3;

PROCEDURE T1(): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

CONST
  Brand = "PreemptionTest";

END PreemptionTest.

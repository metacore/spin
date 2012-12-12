(*
s * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 06-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

INTERFACE AccessModeTest;
IMPORT Regress;

CONST
  TestName = "sec-am";
  TestHelp = "tests permissions and access modes";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "permissions by themselves", T1 },
  Regress.TestDesc{ "permissions in access mode", T2 },
  Regress.TestDesc{ "equality/containement of access modes", T3 },
  Regress.TestDesc{ "combining access modes", T4 },
  Regress.TestDesc{ "verifying all simple permissions const", T5 }
  };

CONST
  nIterations = 1;

PROCEDURE T1(): BOOLEAN;
PROCEDURE T2(): BOOLEAN;
PROCEDURE T3(): BOOLEAN;
PROCEDURE T4(): BOOLEAN;
PROCEDURE T5(): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

CONST
  Brand = "AccessModeTest";

END AccessModeTest.

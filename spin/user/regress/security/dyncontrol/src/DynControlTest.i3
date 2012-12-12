(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 17-Dec-97  Robert Grimm (rgrimm) at the University of Washington
 *      Broke up into individual procedures
 *
 * 29-Oct-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

INTERFACE DynControlTest;
IMPORT Regress;

CONST
  TestName = "sec-dyn";
  TestHelp = "tests dynamic access checks";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "Object setup", T1 },
  Regress.TestDesc{ "No access control", T2 },
  Regress.TestDesc{ "Impose code and object checks", T3 },
  Regress.TestDesc{ "Code check fails", T4 },
  Regress.TestDesc{ "Object 1 check fails", T5 },
  Regress.TestDesc{ "Object 2 check fails", T6 },
  Regress.TestDesc{ "Object 3 check fails", T7 },
  Regress.TestDesc{ "Code and object checks pass", T8 },
  Regress.TestDesc{ "NIL for object 1", T9 },
  Regress.TestDesc{ "NIL for object 2", T10 },
  Regress.TestDesc{ "NIL for object 3", T11 },
  Regress.TestDesc{ "Check type checking", T12 },
  Regress.TestDesc{ "Impose domain transfer", T13 },
  Regress.TestDesc{ "Execute domain transfer", T14 },
  Regress.TestDesc{ "Impose domain transfer and checks", T15 },
  Regress.TestDesc{ "Code check fails", T16 },
  Regress.TestDesc{ "Object 1 check fails", T17 },
  Regress.TestDesc{ "Object 2 check fails", T18 },
  Regress.TestDesc{ "Object 3 check fails", T19 },
  Regress.TestDesc{ "Checks pass and domain is changed", T20 },
  Regress.TestDesc{ "Remove access control", T21 },
  Regress.TestDesc{ "Execute without access control", T22 },
  Regress.TestDesc{ "Impose code and object checks", T23 },
  Regress.TestDesc{ "Reduce object checks away", T24 },
  Regress.TestDesc{ "Execute with NIL access checks", T25 }
  };

CONST
  nIterations = 3;

PROCEDURE T1(): BOOLEAN;
PROCEDURE T2(): BOOLEAN;
PROCEDURE T3(): BOOLEAN;
PROCEDURE T4(): BOOLEAN;
PROCEDURE T5(): BOOLEAN;
PROCEDURE T6(): BOOLEAN;
PROCEDURE T7(): BOOLEAN;
PROCEDURE T8(): BOOLEAN;
PROCEDURE T9(): BOOLEAN;
PROCEDURE T10(): BOOLEAN;
PROCEDURE T11(): BOOLEAN;
PROCEDURE T12(): BOOLEAN;
PROCEDURE T13(): BOOLEAN;
PROCEDURE T14(): BOOLEAN;
PROCEDURE T15(): BOOLEAN;
PROCEDURE T16(): BOOLEAN;
PROCEDURE T17(): BOOLEAN;
PROCEDURE T18(): BOOLEAN;
PROCEDURE T19(): BOOLEAN;
PROCEDURE T20(): BOOLEAN;
PROCEDURE T21(): BOOLEAN;
PROCEDURE T22(): BOOLEAN;
PROCEDURE T23(): BOOLEAN;
PROCEDURE T24(): BOOLEAN;
PROCEDURE T25(): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

TYPE
  SecureT <: REFANY;

PROCEDURE Work(          a1 : SecureT;
                READONLY a2 : SecureT;
                          i : INTEGER;
                     VAR a3 : SecureT; );

CONST
  Brand = "DynControlTest";

END DynControlTest.

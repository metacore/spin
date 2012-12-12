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

INTERFACE ManagerTest;
IMPORT Regress;

CONST
  TestName = "sec-sm";
  TestHelp = "tests security manager";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "Mediation cache test 1", T1 },
  Regress.TestDesc{ "Set mediation cache size to 2, 2", T2 },
  Regress.TestDesc{ "Mediation cache test 2", T3 },
  Regress.TestDesc{ "Set mediation cache size to 8, 8", T4 },
  Regress.TestDesc{ "Mediation cache test 3", T5 },
  Regress.TestDesc{ "Mediation cache test 4", T6 },
  Regress.TestDesc{ "Mediation cache test 5", T7 },
  Regress.TestDesc{ "Reset security policy", T8 },
  Regress.TestDesc{ "Security domain change test 1", T9 },
  Regress.TestDesc{ "Security domain change test 2", T10 },
  Regress.TestDesc{ "Security domain change test 3", T11 },
  Regress.TestDesc{ "Security domain change test 4", T12 },
  Regress.TestDesc{ "Type security test", T13 }
  };

CONST
  (*
     These tests should be run a few times in a row,
     since during the initial development deadlock occurred
     due to a complicated chain of events.
   *)
  nIterations = 5;

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

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

(* Used for testing *)

EXCEPTION
  DasEndeDerWelt;

TYPE
  PingPongT <: REFANY;

PROCEDURE Step     ( i : INTEGER                  ) : BOOLEAN;
PROCEDURE StepTwo  ( i : INTEGER; raise : BOOLEAN ) : BOOLEAN
  RAISES { DasEndeDerWelt };
PROCEDURE StepThree( i : INTEGER; raise : BOOLEAN ) : BOOLEAN
  RAISES { DasEndeDerWelt };
PROCEDURE StepFour ( i : INTEGER                  ) : BOOLEAN;

PROCEDURE Ping( x : PingPongT; i : INTEGER );
PROCEDURE Pong( x : PingPongT; i : INTEGER );

PROCEDURE PingTwo( x : PingPongT; i : INTEGER ) : BOOLEAN;

CONST
  Brand = "ManagerTest";

END ManagerTest.

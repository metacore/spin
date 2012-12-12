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

INTERFACE DynTypes;
IMPORT Regress;

CONST
  TestName = "dyntypes";
  TestHelp = "test correctness of types in dynamically loaded code";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "typecode", TypecodeTest },
  Regress.TestDesc{ "narrow", NarrowTest },
  Regress.TestDesc{ "exec", ExecTest },
  Regress.TestDesc{ "typecase", TypecaseTest }
  };

CONST
  nIterations = 1;

PROCEDURE TypecodeTest(): BOOLEAN;
PROCEDURE NarrowTest(): BOOLEAN;
PROCEDURE ExecTest(): BOOLEAN;
PROCEDURE TypecaseTest(): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

TYPE
  T = OBJECT 
    f1: INTEGER;
  METHODS
    m1();
    m2(i: INTEGER): INTEGER;
  END;
  
  OT1 <: T;

END DynTypes.





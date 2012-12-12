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

INTERFACE DispType;
IMPORT Regress;

CONST
  TestName = "disp-type";
  TestHelp = "test dynamic typing of procedures done by the dispatcher";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "simple correct 1", SimpleCorrect1 },
  Regress.TestDesc{ "simple correct 2", SimpleCorrect2 },
  Regress.TestDesc{ "simple correct 3", SimpleCorrect3 },
  Regress.TestDesc{ "simple correct 4", SimpleCorrect4 },
  Regress.TestDesc{ "simple correct 5", SimpleCorrect5 },

  Regress.TestDesc{ "closure correct 1", ClosureCorrect1 },
  Regress.TestDesc{ "closure correct 2", ClosureCorrect2 },
  Regress.TestDesc{ "closure correct 3", ClosureCorrect3 },
  Regress.TestDesc{ "closure correct 4", ClosureCorrect4 },
  Regress.TestDesc{ "closure correct 5", ClosureCorrect5 },
  Regress.TestDesc{ "closure correct 6", ClosureCorrect6 },

  Regress.TestDesc{ "arg types 1", ArgTypes1 },
  Regress.TestDesc{ "arg types 2", ArgTypes2 },
  Regress.TestDesc{ "arg types 3", ArgTypes3 },
  Regress.TestDesc{ "arg types 4", ArgTypes4 },
  Regress.TestDesc{ "arg types 5", ArgTypes5 },

  Regress.TestDesc{ "simple incorrect 1", SimpleIncorrect1 },
  Regress.TestDesc{ "simple incorrect 2", SimpleIncorrect2 },
  Regress.TestDesc{ "simple incorrect 3", SimpleIncorrect3 },
  Regress.TestDesc{ "simple incorrect 4", SimpleIncorrect4 },
  Regress.TestDesc{ "simple incorrect 5", SimpleIncorrect5 },

  Regress.TestDesc{ "closure incorrect 1", ClosureIncorrect1 },
  Regress.TestDesc{ "closure incorrect 2", ClosureIncorrect2 },
  Regress.TestDesc{ "closure incorrect 3", ClosureIncorrect3 },

  Regress.TestDesc{ "no args 1", NoArgs1 },
  Regress.TestDesc{ "no args 2", NoArgs2 },

  Regress.TestDesc{ "asynch 1", Asynch1 },
  Regress.TestDesc{ "asynch 2", Asynch2 },
  Regress.TestDesc{ "asynch 3", Asynch3 },
  Regress.TestDesc{ "asynch 4", Asynch4 }
  };

CONST
  nIterations = 1;

PROCEDURE SimpleCorrect1(): BOOLEAN;
PROCEDURE SimpleCorrect2(): BOOLEAN;
PROCEDURE SimpleCorrect3(): BOOLEAN;
PROCEDURE SimpleCorrect4(): BOOLEAN;
PROCEDURE SimpleCorrect5(): BOOLEAN;

PROCEDURE ClosureCorrect1(): BOOLEAN;
PROCEDURE ClosureCorrect2(): BOOLEAN;
PROCEDURE ClosureCorrect3(): BOOLEAN;
PROCEDURE ClosureCorrect4(): BOOLEAN;
PROCEDURE ClosureCorrect5(): BOOLEAN;
PROCEDURE ClosureCorrect6(): BOOLEAN;

PROCEDURE ArgTypes1(): BOOLEAN;
PROCEDURE ArgTypes2(): BOOLEAN;
PROCEDURE ArgTypes3(): BOOLEAN;
PROCEDURE ArgTypes4(): BOOLEAN;
PROCEDURE ArgTypes5(): BOOLEAN;

PROCEDURE SimpleIncorrect1(): BOOLEAN;
PROCEDURE SimpleIncorrect2(): BOOLEAN;
PROCEDURE SimpleIncorrect3(): BOOLEAN;
PROCEDURE SimpleIncorrect4(): BOOLEAN;
PROCEDURE SimpleIncorrect5(): BOOLEAN;

PROCEDURE ClosureIncorrect1(): BOOLEAN;
PROCEDURE ClosureIncorrect2(): BOOLEAN;
PROCEDURE ClosureIncorrect3(): BOOLEAN;

PROCEDURE NoArgs1 (): BOOLEAN;
PROCEDURE NoArgs2 (): BOOLEAN;

PROCEDURE Asynch1 (): BOOLEAN;
PROCEDURE Asynch2 (): BOOLEAN;
PROCEDURE Asynch3 (): BOOLEAN;
PROCEDURE Asynch4 (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispType.


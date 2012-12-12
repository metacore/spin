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

INTERFACE DispNoinvoke;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-noinvoke";
  TestHelp = "test evaluation order of guards and handlers";

CONST
  TestList = ARRAY OF Regress.TestDesc{

  Regress.TestDesc{ "no result, default", Test1 },
  Regress.TestDesc{ "no result, no handler installed", Test2 },
  Regress.TestDesc{ "no result, no handler invoked", Test3 },
  Regress.TestDesc{ "no result, many handlers invoked", Test4 },
  Regress.TestDesc{ "result, default", Test5 },
  Regress.TestDesc{ "result, no handler installed", Test6 },
  Regress.TestDesc{ "result, no handler invoked", Test7 },
  Regress.TestDesc{ "result, many handlers invoked", Test8 }
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE Test1 (): BOOLEAN;
PROCEDURE Test2 (): BOOLEAN;
PROCEDURE Test3 (): BOOLEAN;
PROCEDURE Test4 (): BOOLEAN;
PROCEDURE Test5 (): BOOLEAN;
PROCEDURE Test6 (): BOOLEAN;
PROCEDURE Test7 (): BOOLEAN;
PROCEDURE Test8 (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispNoinvoke.

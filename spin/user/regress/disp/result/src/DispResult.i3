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

INTERFACE DispResult;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-result";
  TestHelp = "test argument passing through the dispatcher";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "simple", DoSimple },
  Regress.TestDesc{ "result-1", DoResult1 },
  Regress.TestDesc{ "result-2", DoResult2 },
  Regress.TestDesc{ "result-3", DoResult3 },
  Regress.TestDesc{ "no-result", DoNoResult },
  Regress.TestDesc{ "no-args", DoNoArgs }
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE DoSimple (): BOOLEAN;
PROCEDURE DoResult1 (): BOOLEAN;
PROCEDURE DoResult2 (): BOOLEAN;
PROCEDURE DoResult3 (): BOOLEAN;
PROCEDURE DoNoResult (): BOOLEAN;
PROCEDURE DoNoArgs (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispResult.


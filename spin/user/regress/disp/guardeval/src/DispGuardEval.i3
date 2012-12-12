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

INTERFACE DispGuardEval;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-guard";
  TestHelp = "test evaluation order of guards and handlers";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "guard evaluation order", Test }
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE Test (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispGuardEval.

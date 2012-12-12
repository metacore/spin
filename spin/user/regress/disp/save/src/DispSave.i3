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

INTERFACE DispSave;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-save";
  TestHelp = "test save passing through the dispatcher";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "test1 ", Test1 },
  Regress.TestDesc{ "test2 ", Test2 },
  Regress.TestDesc{ "test3 ", Test3 }
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE Test1 (): BOOLEAN;
PROCEDURE Test2 (): BOOLEAN;
PROCEDURE Test3 (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispSave.
 

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

INTERFACE DispClosure;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-closure";
  TestHelp = "test closure passing through the dispatcher";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "test1 ", Test1 },
  Regress.TestDesc{ "test2 ", Test2 }
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE Test1 (): BOOLEAN;
PROCEDURE Test2 (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispClosure.
 

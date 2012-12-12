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

INTERFACE DispAsynch;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-asynch";
  TestHelp = "test argument passing through the dispatcher";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "asynch", DoAsynch },
  Regress.TestDesc{ "args (1st)", DoArgs },
  Regress.TestDesc{ "args (2nd)", DoArgs }
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE DoAsynch (): BOOLEAN;
PROCEDURE DoArgs (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispAsynch.


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

INTERFACE DispException;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-except";
  TestHelp = "test propagation of exceptions through the dispatcher";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "exception", ExceptionTest }
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE ExceptionTest (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispException.

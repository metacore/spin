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

INTERFACE DispLinear;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-linear";
  TestHelp = "test linearized argument passing through the dispatcher";
  
CONST
  TestList = ARRAY OF Regress.TestDesc{
    Regress.TestDesc{ "linear", DoLinear }
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE DoLinear (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispLinear.

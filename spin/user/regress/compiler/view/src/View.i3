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

INTERFACE View;
IMPORT Regress;

CONST
  TestName = "view";
  TestHelp = "test view";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "view", Test }
  };

CONST
  nIterations = 1;

PROCEDURE Test (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END View.

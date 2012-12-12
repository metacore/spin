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

INTERFACE Hello;
IMPORT Regress;

CONST
  TestName = "hello";
  TestHelp = "simplest test, prints \"hello world\"";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "hello", Hello }
  };

CONST
  nIterations = 1;

PROCEDURE Hello (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

CONST
  Brand = "Hello";

END Hello.

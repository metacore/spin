(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE PagingBasic;
IMPORT Regress;

CONST
  TestName = "pagingbasic";
  TestHelp = "tests basic disk paging";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "pagingbasic", PagingBasic }
  };

CONST
  nIterations = 1;

PROCEDURE PagingBasic (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END PagingBasic.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *)

INTERFACE SpaceBasic;
IMPORT Regress;

CONST
  TestName = "spacebasic";
  TestHelp = "tests basic Space-style allocation and deallocation";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "spacebasic", SpaceBasic }
  };

CONST
  nIterations = 1;

PROCEDURE SpaceBasic (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END SpaceBasic.

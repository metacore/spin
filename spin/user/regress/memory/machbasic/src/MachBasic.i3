(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *)

INTERFACE MachBasic;
IMPORT Regress;

CONST
  TestName = "machbasic";
  TestHelp = "tests basic Mach-style allocation and deallocation";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "machbasic", MachBasic }
  };

CONST
  nIterations = 1;

PROCEDURE MachBasic (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END MachBasic.

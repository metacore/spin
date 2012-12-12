(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Turnned into a regression test.
 *
 * 23-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. 
 *
 *)

INTERFACE BitField;
IMPORT Regress;

CONST
  TestName = "bitfield";
  TestHelp = "test bitfield";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "bitfield", BitField },
  Regress.TestDesc{ "sign", Sign }
  };

CONST
  nIterations = 1;

PROCEDURE BitField (): BOOLEAN;
PROCEDURE Sign (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END BitField.




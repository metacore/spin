(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Jan-96 Przemek Pardyak (pardy) at the University of Washington
 *	Filtered output.
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created.  Control over regression tests.
 *
 *)

INTERFACE Regress;
IMPORT ParseParams, Wr;

CONST CommandName = "regress";
CONST CommandHelp = "[-quiet|-verbose] -all|-clean|<test-name> - run regression tests";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

TYPE
  TestProc = PROCEDURE (): BOOLEAN;

  TestDesc = RECORD
    name: TEXT;
    test: TestProc;
  END;

VAR
  Writer: Wr.T;
  ErrorWriter: Wr.T;

CONST
  Brand = "Regress";

END Regress.

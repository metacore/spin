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

INTERFACE DispImpose;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-impose";
  TestHelp = "test imposed guards";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "single", DoSingle },
  Regress.TestDesc{ "NIL guards", DoNILGuards },
  Regress.TestDesc{ "TRUE guards", DoTRUEGuards },
  Regress.TestDesc{ "FALSE guards", DoFALSEGuards },
  Regress.TestDesc{ "mixed guards", DoMixedGuards },
  Regress.TestDesc{ "multiple guards", DoMultipleGuards }
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE DoSingle (): BOOLEAN;
PROCEDURE DoNILGuards (): BOOLEAN;
PROCEDURE DoTRUEGuards (): BOOLEAN;
PROCEDURE DoFALSEGuards (): BOOLEAN;
PROCEDURE DoMixedGuards (): BOOLEAN;
PROCEDURE DoMultipleGuards (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispImpose.


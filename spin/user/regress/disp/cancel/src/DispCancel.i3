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

INTERFACE DispCancel;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-cancel";
  TestHelp = "test argument passing through the dispatcher";

CONST
  TestList = ARRAY OF Regress.TestDesc{
  Regress.TestDesc{ "single", DoSingle },
  Regress.TestDesc{ "NIL guards", DoNILGuards } (*,
  Regress.TestDesc{ "TRUE guards", DoTRUEGuards },
  Regress.TestDesc{ "FALSE guards", DoFALSEGuards },
  Regress.TestDesc{ "mixed guards", DoMixedGuards }*)
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE DoSingle (): BOOLEAN;
PROCEDURE DoNILGuards (): BOOLEAN;
PROCEDURE DoTRUEGuards (): BOOLEAN;
PROCEDURE DoFALSEGuards (): BOOLEAN;
PROCEDURE DoMixedGuards (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispCancel.


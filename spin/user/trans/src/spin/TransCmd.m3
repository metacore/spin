(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransCmd;
IMPORT IO, TransCommands;
IMPORT ParseParams;
IMPORT NameServer;
IMPORT TransInterface;
IMPORT Auth;

PROCEDURE Run (pp : ParseParams.T) : BOOLEAN =
  BEGIN
    pp.reset();
    TransCommands.Execute(SUBARRAY(pp.arg^, 1, NUMBER(pp.arg^)-1));
    RETURN TRUE;
  END Run;

BEGIN
  TRY
    EVAL TransInterface.Export(NEW(Auth.AuthAlways));
    IO.Put("Transaction manager loaded.\n");
  EXCEPT
    NameServer.Error => IO.Put("Failed to export trans.\n");
  END;
END TransCmd.

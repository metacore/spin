(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Salvaged from long dormancy
 * 13-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE Unmount;

IMPORT IO;
IMPORT Error;
IMPORT FileSystem;
IMPORT ParseParams;


PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    path: TEXT;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* Command *)
      path := pp.getNext();
      FileSystem.Unmount(path);
    EXCEPT
    | Error.E(e) =>
        IO.PutError("unmount:"& e.message() & "\n");
    | ParseParams.Error => IO.PutError("Usage: mount dev fstype path\n");
    END;
    RETURN TRUE;
  END Run;

BEGIN
END Unmount.



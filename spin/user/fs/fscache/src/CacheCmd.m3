(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *
 *)

MODULE CacheCmd;

IMPORT ParseParams, Text, IO;
IMPORT FileDataCache;

PROCEDURE Run (pp: ParseParams.T) : BOOLEAN =
  VAR
    option: TEXT;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* skip command name *)
      option := pp.getNext();
      IF Text.Equal(option, "hello") THEN
        IO.Put("Cache says hello.\n");
      ELSIF Text.Equal(option, "stat") THEN
        FileDataCache.PrintStat(FileDataCache.GetCentralCache());
      ELSE
        IO.Put("Cache: Unknown request: " & option & "\n");
      END;
      RETURN TRUE;
    EXCEPT
    | ParseParams.Error =>
      IO.Put(CommandName & CommandHelp & "\n");
      RETURN FALSE;
    END;
  END Run;

BEGIN
END CacheCmd.


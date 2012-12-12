(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 8-July-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *)
(* Program for testing read *)
MODULE RmDir;

IMPORT IO,ParseParams;
IMPORT FileSystem;
IMPORT Error;

CONST DEBUG = FALSE;


PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
VAR
	thePath : TEXT;
BEGIN

  IF DEBUG = TRUE THEN
    IO.Put("rmdir called.\n");
  END;
  
  pp.reset();

  TRY
    pp.skipNext(); (* skip command *)
    thePath := pp.getNext();
  EXCEPT
  | ParseParams.Error => IO.Put("usage: " & CommandHelp & "\n");
    RETURN FALSE;
  END;
  
  IF DEBUG = TRUE THEN
    IO.Put("RmDir about to call FileSystem.RmDir.\n");
  END;

  TRY
    FileSystem.RmDir(thePath);
  EXCEPT
  | Error.E => IO.PutError("RmDir "&thePath&" failed.");
    RETURN FALSE;
  END;

  RETURN TRUE;
END Run;


BEGIN
END RmDir.




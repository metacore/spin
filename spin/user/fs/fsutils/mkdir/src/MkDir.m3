(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 4-July-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *)
(* Program for testing read *)
MODULE MkDir;

IMPORT IO,ParseParams;
IMPORT FileSystem;
IMPORT Error;

CONST DEBUG = TRUE;


PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
VAR
	thePath : TEXT;
BEGIN

  IF DEBUG = TRUE THEN
    IO.Put("mkdir called.\n");
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
    IO.Put("MkDir about to call FileSystem.MkDir.\n");
  END;

  TRY
    FileSystem.MkDir(thePath);
  EXCEPT
  | Error.E => IO.PutError("MkDir "&thePath&" failed.");
    RETURN FALSE;
  END;

  RETURN TRUE;
END Run;


BEGIN
END MkDir.




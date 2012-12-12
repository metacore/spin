(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)
MODULE Touch;

IMPORT IO, ParseParams;
IMPORT FileSystem;
IMPORT LFSTypes;
IMPORT Error;

PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
VAR
  theCommand:TEXT;
  thepath : TEXT;
BEGIN
  pp.reset();
  TRY
    theCommand:= pp.getNext();
    thepath := pp.getNext();
  EXCEPT
  | ParseParams.Error => IO.Put("usage: " & CommandHelp & "\n");
    RETURN FALSE;
  END;
  TRY 
    (*EVAL FileSystem.Open(LFSTypes.OCREATE,thepath);*)
    EVAL FileSystem.Open(thepath);
  EXCEPT
  | Error.E(e) => 
    IO.Put("Open of " & thepath & " failed.  Error was: " & e.message() & "\n");
    RETURN FALSE;
  END;
  RETURN TRUE;
END Run;

BEGIN
END Touch.

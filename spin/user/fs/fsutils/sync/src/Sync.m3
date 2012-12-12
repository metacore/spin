(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Made command call the new mp.sync command, instead of cheezing the 
 *      mp name out of an iNode.
 *
 * 05-Jun-96  Tim Bradley (tbradley) at the University of Washington
 *	Sync command for writing fs state to disk.  Created.
 *
 *)
MODULE Sync;

IMPORT ParseParams,IO;
IMPORT FileSystem,File;
IMPORT LFSRep;
IMPORT Error;

PROCEDURE Run(pp: ParseParams.T) : BOOLEAN =
VAR
  devName: TEXT;
  fsName : TEXT;
  command: TEXT;

BEGIN
  pp.reset();
  TRY
    command := pp.getNext();
    devName := pp.getNext();
    fsName := pp.getNext();
    IO.Put("Syncing fs:" & fsName & " at device:"&devName&".\n");
  EXCEPT
  | ParseParams.Error => IO.Put(CommandHelp & "\n");
    RETURN FALSE;
  END;

  TRY
    FileSystem.Sync(devName, fsName);
  EXCEPT
  | Error.E(e) => IO.Put(e.message() & "\n");
    RETURN FALSE;
  END;

  RETURN TRUE;

END Run;

BEGIN
END Sync.




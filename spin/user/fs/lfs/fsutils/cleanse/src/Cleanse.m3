(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 25-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *)
MODULE Cleanse;

IMPORT IO,ParseParams;
IMPORT FileSystem;
IMPORT LFSRep;
(* 
IMPORT Segment;
IMPORT LFSTypes;
IMPORT Cleaner;
*)
IMPORT Error;
    
CONST DEBUG = TRUE;
          
PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
  VAR
    thePath : TEXT;
    fp: LFSRep.FileTPublic;
    mp: LFSRep.MP;
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("Cleanse called.\n");
    END;

    pp.reset();
    TRY
      pp.skipNext();
      thePath := pp.getNext();
    EXCEPT
    | ParseParams.Error => IO.Put("usage: " & CommandHelp & "\n");
      RETURN TRUE;
    END;

    TRY
      fp:=NARROW(FileSystem.Open(thePath),LFSRep.FileTPublic);
    EXCEPT
    | Error.E =>
      IO.Put("Error in Open, File Not Found.\n");
      RETURN TRUE;
    END;

    mp:=fp.getMP();

    fp.close();
    IF mp.cleaner.cleanNextSegment() = TRUE THEN
      IO.Put("Segment Cleaned.\n");
    ELSE
      IO.Put("No Segment Cleaned.\n");
    END;

    IF DEBUG = TRUE THEN
      IO.Put("Cleanse shell command exiting.\n");
    END;
    
    RETURN TRUE;
  END Run;
  

BEGIN
END Cleanse.




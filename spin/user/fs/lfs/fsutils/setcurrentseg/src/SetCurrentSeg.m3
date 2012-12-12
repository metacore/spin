(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Oct-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *)
MODULE SetCurrentSeg;

IMPORT IO,ParseParams;
IMPORT FileSystem;
IMPORT LFSRep;
IMPORT Error;
    
CONST DEBUG = TRUE;
          
PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
  VAR
    thePath : TEXT;
    fp: LFSRep.FileTPublic;
    mp: LFSRep.MP;
    segmentNumber :INTEGER;
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("GetSegDesc called.\n");
    END;

    pp.reset();
    TRY
      pp.skipNext();
      thePath := pp.getNext();
      segmentNumber := pp.getNextInt();
    EXCEPT
    | ParseParams.Error => IO.Put("usage: " & CommandHelp & "\n");
      RETURN TRUE;
    END;

    IF segmentNumber<0 THEN
      IO.Put("usage: Segment number must be >0.\n");
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

    mp.demoCurrentSegment := segmentNumber;

    IF DEBUG = TRUE THEN
      IO.Put("getfreelist shell command exiting.\n");
    END;
    
    RETURN TRUE;
  END Run;

BEGIN

END SetCurrentSeg.











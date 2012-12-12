(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Oct-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *)
MODULE GetFreeList;

IMPORT IO,Wr,Fmt,ParseParams;
IMPORT FileSystem;
IMPORT Segment;
(*
IMPORT SegBuffer;
IMPORT IMap;
IMPORT INode;
IMPORT INodeInfo;
IMPORT INodeInfoArray;
IMPORT LFSTypes;
*)
IMPORT LFSRep;
IMPORT InfoFile;
IMPORT Error;
    
CONST DEBUG = TRUE;
          
PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
  VAR
    thePath : TEXT;
    freeList: REF ARRAY OF CHAR;
    fp: LFSRep.FileTPublic;
    mp: LFSRep.MP;
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("GetSegDesc called.\n");
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

    IO.Put("Obtained mountpoint.\n");

    fp.close();

    mp.segInfo.segFreeList.getFreeList(freeList);

    IO.Put("Num Segments: "&Fmt.Int(LAST(freeList^))&"\n");

    FOR i := 0 TO LAST(freeList^) DO
      IO.Put(Fmt.Int(i));
      WITH state = freeList[i] DO
        IF state=Segment.DIRTY THEN 
          IO.Put("Dirty ");
        ELSIF state=Segment.CLEAN THEN
          IO.Put("Clean ");
        ELSIF state=Segment.FREE THEN
          IO.Put("Free ");
        ELSE 
          IO.Put("Unknown ");
        END;
      END;
      IO.Put("Segment: "&Fmt.Int(i)&"\n");
    END;


    IF DEBUG = TRUE THEN
      IO.Put("getfreelist shell command exiting.\n");
    END;
    
    RETURN TRUE;
  END Run;



PROCEDURE LFSGetFreeList(wr: Wr.T) =
  VAR
    thePath : TEXT;
    freeList: REF ARRAY OF CHAR;
    fp: LFSRep.FileTPublic;
    mp: LFSRep.MP;
  BEGIN

    (*    pp.reset();
          TRY
          pp.skipNext();
          thePath := pp.getNext();
          EXCEPT
          | ParseParams.Error => Wr.PutText("usage: " & CommandHelp & "\n");
          RETURN TRUE;
          END;
    *)

    (* need to stick in a filename that exists *)
    thePath := "/lfs/LFSFile";

    TRY
      fp:=NARROW(FileSystem.Open(thePath),LFSRep.FileTPublic);
    EXCEPT
    | Error.E =>
      Wr.PutText(wr,"Error, need to create file '/lfs/LFSFile'\n");
      RETURN;
    END;

    mp:=fp.getMP();

    fp.close();

    mp.segInfo.segFreeList.getFreeList(freeList);

    Wr.PutText(wr,"Num Segments:"&Fmt.Int(LAST(freeList^))&"\n");

    FOR i := 0 TO LAST(freeList^) DO
      Wr.PutText(wr,Fmt.Int(i));
      WITH state = freeList[i] DO
        IF state=Segment.DIRTY THEN 
          Wr.PutText(wr,"Dirty ");
        ELSIF state=Segment.CLEAN THEN
          Wr.PutText(wr,"Clean ");
        ELSIF state=Segment.FREE THEN
          Wr.PutText(wr,"Free ");
        ELSE 
          Wr.PutText(wr,"Unknown ");
        END;
      END;
      Wr.PutText(wr,"Segment: "&Fmt.Int(i)&"\n");
    END;

  END LFSGetFreeList;
  

BEGIN
  TRY
     InfoFile.Create("/proc/lfsgetfreelist",LFSGetFreeList);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("gc procfs files:" & e.message() & "\n");
  END;
END GetFreeList.











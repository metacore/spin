(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Oct-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *)
MODULE GetSegDesc;

IMPORT IO,Wr,Fmt,ParseParams;
IMPORT FileSystem;
IMPORT Segment;
IMPORT SegBuffer;
IMPORT IMap;
IMPORT INode;
(*
IMPORT INodeInfo;
IMPORT INodeInfoArray;
IMPORT Cleaner;
*)
IMPORT LFSTypes;
IMPORT LFSRep;
IMPORT Error;
IMPORT InfoFile;
    
CONST DEBUG = TRUE;
          
PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
  VAR
    thePath : TEXT;
    segmentNumber:CARDINAL;
    fp: LFSRep.FileTPublic;
    mp: LFSRep.MP;
    segmentDescriptor:REF ARRAY OF CHAR;
    nextLoc:Segment.DiskAddress;
    epoch:INTEGER;
    segSumEntry: Segment.SegSummaryEntry;
    entries : CARDINAL;
    deadFlag :BOOLEAN;
    currDiskAddress:Segment.DiskAddress;
    currINode:INode.T;
    liveCount:CARDINAL;
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

    segmentDescriptor := NEW(REF ARRAY OF CHAR,
                             mp.segInfo.blocksInSegment*BYTESIZE(Segment.SegSummaryEntry));

    entries := SegBuffer.GetSegmentSummary(mp.segInfo,
                                           segmentNumber,
                                           segmentDescriptor,
                                           nextLoc,
                                           epoch);

    IO.Put("Recovered "&Fmt.Int(entries)&" entries from segment "&Fmt.Int(segmentNumber)&".\n");

    IO.Put(Fmt.Int(entries)&" entries.\n");

    FOR i:= 0 TO entries-1 DO
      deadFlag := FALSE;

      segSumEntry:=
          VIEW(SUBARRAY(segmentDescriptor^,
                        i*BYTESIZE(Segment.SegSummaryEntry),
                        BYTESIZE(Segment.SegSummaryEntry)),
               Segment.SegSummaryEntry);


      IO.Put("(");
      (*
        IO.Put(Fmt.Int(segmentNumber)&",");
        IO.Put(Fmt.Int(i)&",");
      *)
      IO.Put(Fmt.Int(segSumEntry.iNode)&",");
      IO.Put(Fmt.Int(segSumEntry.offset)&",");


      (* if the flag says DATA then check to see if it is live data *)
      IF segSumEntry.flag = LFSTypes.DATA THEN
        IO.Put("DATA,");
      ELSE
        IO.Put("INODE,");
      END;

      (* First we check to see if the value is already illegal, if so
         we don't need to check on disk *)
      IF (segSumEntry.iNode # LFSTypes.ILLEGALDISKVALUE AND
        segSumEntry.offset # LFSTypes.ILLEGALDISKVALUE) THEN
        
        TRY
          currINode := NARROW(mp.imap.getINode(segSumEntry.iNode),INode.T);
        EXCEPT
        | IMap.InvalidINum =>
          
          deadFlag := TRUE;

          (* this can occur if the file has been deleted since the
             segment was written.*)
        END;

        IF deadFlag = FALSE THEN

          currDiskAddress.block   := i;

          (* if the block is alive then increment live block counter
             AND add that block to the iNode info array *)
          IF ((currINode.verifyData(LFSTypes.INodeBlockInfo{currDiskAddress,
                                                            segSumEntry.offset}) = TRUE) 
            OR (currINode.verifyMeta(LFSTypes.INodeBlockInfo{currDiskAddress,
                                                             segSumEntry.offset}) = TRUE))
           THEN
            liveCount := liveCount + 1;

            IO.Put("LIVE)\n");
              
          ELSE 
            IO.Put("DEAD)\n"); 
          END;

        ELSE
          IO.Put("DEAD)\n");
        END;

      END; (* end if diskAddress legal *)

    END;  (*  end FOR loop *)

    IF DEBUG = TRUE THEN
      IO.Put("getsegdesc shell command exiting.\n");
    END;
    
    RETURN TRUE;
  END Run;



PROCEDURE LFSGetSegDesc(wr: Wr.T) =
  VAR
    thePath : TEXT;
    segmentNumber:CARDINAL;
    fp: LFSRep.FileTPublic;
    mp: LFSRep.MP;
    segmentDescriptor:REF ARRAY OF CHAR;
    nextLoc:Segment.DiskAddress;
    epoch:INTEGER;
    segSumEntry: Segment.SegSummaryEntry;
    entries : CARDINAL;
    deadFlag :BOOLEAN;
    currDiskAddress:Segment.DiskAddress;
    currINode:INode.T;
    liveCount:CARDINAL;
  BEGIN

    IF DEBUG = TRUE THEN
      Wr.PutText(wr,"GetSegDesc called.\n");
    END;


    (* need to stick in these defaults *)
    thePath := "/lfs/LFSFile";


    TRY
      fp:=NARROW(FileSystem.Open(thePath),LFSRep.FileTPublic);
    EXCEPT
    | Error.E =>
      Wr.PutText(wr,"File '/lfs/LFSFile' must exist.\n");
      RETURN;
    END;


    mp:=fp.getMP();
    segmentNumber := mp.demoCurrentSegment;


    Wr.PutText(wr,"Obtained mountpoint.\n");

    fp.close();

    segmentDescriptor := NEW(REF ARRAY OF CHAR,
                             mp.segInfo.blocksInSegment*BYTESIZE(Segment.SegSummaryEntry));

    entries := SegBuffer.GetSegmentSummary(mp.segInfo,
                                           segmentNumber,
                                           segmentDescriptor,
                                           nextLoc,
                                           epoch);

    Wr.PutText(wr,"Recovered "&Fmt.Int(entries)&" entries from segment "&Fmt.Int(segmentNumber)&".\n");

    Wr.PutText(wr,Fmt.Int(entries)&" entries.\n");

    FOR i:= 0 TO entries-1 DO
      deadFlag := FALSE;

      segSumEntry:=
          VIEW(SUBARRAY(segmentDescriptor^,
                        i*BYTESIZE(Segment.SegSummaryEntry),
                        BYTESIZE(Segment.SegSummaryEntry)),
               Segment.SegSummaryEntry);


      Wr.PutText(wr,"(");
      (*
        Wr.PutText(wr,Fmt.Int(segmentNumber)&",");
        Wr.PutText(wr,Fmt.Int(i)&",");
      *)
      Wr.PutText(wr,Fmt.Int(segSumEntry.iNode)&",");
      Wr.PutText(wr,Fmt.Int(segSumEntry.offset)&",");


      (* if the flag says DATA then check to see if it is live data *)
      IF segSumEntry.flag = LFSTypes.DATA THEN
        Wr.PutText(wr,"DATA,");
      ELSE
        Wr.PutText(wr,"INODE,");
      END;

      (* First we check to see if the value is already illegal, if so
         we don't need to check on disk *)
      IF (segSumEntry.iNode # LFSTypes.ILLEGALDISKVALUE AND
        segSumEntry.offset # LFSTypes.ILLEGALDISKVALUE) THEN
        
        TRY
          currINode := NARROW(mp.imap.getINode(segSumEntry.iNode),INode.T);
        EXCEPT
        | IMap.InvalidINum =>
          
          deadFlag := TRUE;

          (* this can occur if the file has been deleted since the
             segment was written.*)
        END;

        IF deadFlag = FALSE THEN

          currDiskAddress.block   := i;

          (* if the block is alive then increment live block counter
             AND add that block to the iNode info array *)
          IF ((currINode.verifyData(LFSTypes.INodeBlockInfo{currDiskAddress,
                                                            segSumEntry.offset}) = TRUE) 
            OR (currINode.verifyMeta(LFSTypes.INodeBlockInfo{currDiskAddress,
                                                             segSumEntry.offset}) = TRUE))
           THEN
            liveCount := liveCount + 1;

            Wr.PutText(wr,"LIVE)\n");
              
          ELSE 
            Wr.PutText(wr,"DEAD)\n"); 
          END;

        ELSE
          Wr.PutText(wr,"DEAD)\n");
        END;

      END; (* end if diskAddress legal *)

    END;  (*  end FOR loop *)

    IF DEBUG = TRUE THEN
      Wr.PutText(wr,"getsegdesc shell command exiting.\n");
    END;
    
  END LFSGetSegDesc;
  

BEGIN
  TRY
    InfoFile.Create("/proc/lfsgetsegdesc",LFSGetSegDesc);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("gc procfs files:" & e.message() & "\n");
  END;
END GetSegDesc.




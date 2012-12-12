(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Made cleaner thread forked by launch() and work in background.
 *	Made thread safe.
 *	XXX. Need to implement policies;
 *		when it's invoked.	(periodically for the time being)
 *		when it stops.		(after cleaning 9 segments)
 *		etc...
 *
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel and MachineCPU interfaces
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Due to Segment.read() I/F changes.
 *
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.  LFS Cleaner searches through log segments copying live blocks
 *	to fs segment.  When segment is clean we register it with the LFS free
 *      list.
 *
 *)
MODULE Cleaner;

IMPORT IO, Fmt, Auth, Thread, ThreadExtra;
IMPORT IMap;
IMPORT INodeInfo;
IMPORT INodeInfoArray;
FROM INodeInfoArray IMPORT NewINodeInfoArray;

IMPORT INode;

IMPORT SegBuffer;

FROM Segment IMPORT SegSummaryEntry;
FROM Segment IMPORT DiskAddress;
IMPORT Segment;

IMPORT LFSTypes;
FROM LFSTypes IMPORT RCharArray;
FROM LFSTypes IMPORT ShortCard;
FROM LFSTypes IMPORT INodeBlockInfo;
FROM LFSTypes IMPORT ILLEGALDISKVALUE;

IMPORT LFSLock;

IMPORT CleanerInterface;


CONST DEBUG = FALSE;
      FUNOUTPUT = FALSE;
      OUTMSG = FALSE;		(* prints a few msg *)

(* if we find fewer than this number of live
   blocks in a segment, we'll clean it *)
CONST LIFETHRESHOLD = 800;


(* XXX. FIXME.  Choose appropreate value.  Time are in u-sec. *)
(* For the time being, cleaner runs every 5 sec *)
CONST CLEANERSLEEPTIME = 5000000;

REVEAL T = Public BRANDED
OBJECT

  (* for counting how many live blocks there are in a segment *)
  liveCount                : CARDINAL; 

  iMap                     : IMap.T;
  blockCharArray           : RCharArray;

  (*currentSegmentDescriptor : REF ARRAY OF SegSummaryEntry;*)
  currentSegmentDescriptor : REF ARRAY OF CHAR;
  currentSegmentNumber     : ShortCard;
 
  segInfo                  : Segment.T;
  segBuffer                : SegBuffer.Buffer;

  bigBuffer                : REF ARRAY OF CHAR;
  lock			   : LFSLock.T; (* used for synchronization *)
  mu			   : MUTEX;
  cond                     : Thread.Condition;
  continue		   : BOOLEAN;	(* if true, cleaner is working *)

OVERRIDES
  cleanNextSegment := CleanNextSegment;
  initialize       := Initialize;
  start := Start;
  stop := Stop;
END;


PROCEDURE Start(self: T) =
  BEGIN
    self.continue := TRUE;
    Thread.Signal(self.cond);

    IF DEBUG THEN IO.Put("cleaner started\n"); END;
  END Start;

(* XXX. not tested *)
PROCEDURE Stop(self: T) =
  BEGIN
    self.continue := FALSE;

    IF DEBUG THEN IO.Put("cleaner stopped\n"); END;
  END Stop;

(* runs until it gets an exception, checks every so often
   to see if the cleaner should run *)
PROCEDURE CleanerControl(arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  PROCEDURE Clean() = 
    VAR
      ret : BOOLEAN;
    BEGIN 
      TRY
        self.lock.lockwhole();			(* lock LFS *)
	IF OUTMSG THEN IO.Put("LFS: Cleaning started. LFS is suspended\n"); END;
        FOR i:= 0 TO 4 DO
	  (* XXX. FIXME.  It's better to stop cleaning when there are
	   * enough free segments and/or when cleaning takes too long.
	   *)
          TRY
	    ret := self.cleanNextSegment();	(* clean a segment *)
          EXCEPT
          ELSE
	    (* XXX. do whatever necessary *)
          END;
        END; (* end of FOR loop *)
      FINALLY
	IF OUTMSG THEN IO.Put("\nLFS: Cleaning end. LFS is resumed\n"); END;
        self.lock.unlockwhole();		(* unlock LFS *)
      END;
  END Clean;

  VAR
    interval : CARDINAL;
    self : T;
  BEGIN

    interval := CLEANERSLEEPTIME;
    self := NARROW(arg, T);

    WHILE TRUE DO
      
      IF NOT self.continue THEN
	Thread.Wait(self.mu, self.cond);
      END;

      Thread.Pause(interval);			(* Sleep for a while *)
	(* XXX FIXME.  Due to a bug in Machine.Hertz(),
	 * Thread.Pause() does not work properly on PCs.
	 * It waits longer on PCs than on ALPHAs.
	 *)

      Clean();
    END;

  END CleanerControl;


(* CleanNextSegment *)
(* Part 1
   Function loads a disk segment into the cleaners buffer, then
   scans through the loaded buffer and counts the live blocks,
   if below a threshold, cleaning occurs.*)

(* Part 2
   Now cleaning takes place
   This means simply writing all the live blocks to the global
   segment buffer and updating the inodes to reflect the new block
   positions.
*)

(* Part 3
   Finally, all the inodes are written to disk, Inodes are planned to know
   how this is done.*)


PROCEDURE CleanNextSegment(self:T):BOOLEAN =
  BEGIN

    (* find the next segment to clean *)
    IF self.segInfo.segmentsOnDisk < 2 THEN
      IO.PutError("File System has less than 2 segments, " &
                  "cleaning is IMPOSSIBLE!\n");
      RETURN FALSE;
    END;

    TRY
      self.currentSegmentNumber := self.segInfo.segFreeList.getNextDirtySeg();

      IF OUTMSG THEN
	IO.Put("seg:"& Fmt.Int(self.currentSegmentNumber)&". ");
      END;
    EXCEPT
    | Segment.NoDirtySegs =>
      IF FUNOUTPUT THEN
        IO.Put("Cleaner found NOT dirty segments to clean.\n");
      END;
      RETURN FALSE;
    END;

    (* if we cleaned the segment, remove it from the dirty list
     * and declare it clean *)
    IF CleanSegment(self,self.currentSegmentNumber) = TRUE THEN
      IF OUTMSG THEN IO.Put(" cleaned. "); END;
      IF self.currentSegmentNumber #
	 self.segInfo.segFreeList.declareNextDirtyCleaned() THEN
        IF FUNOUTPUT = TRUE THEN
          IO.Put("Error in clean, dirty list changed during cleaning.\n");
        END;
      END;

      (* otherwise the segment wasn't cleaned,
       * we put it on the back of the dirty list *)
    ELSE
      IF OUTMSG THEN IO.Put(" not cleaned. "); END;
      TRY
        self.segInfo.segFreeList.moveNextDirtyToBack();
      EXCEPT
      | Segment.NoDirtySegs =>
        IF FUNOUTPUT = TRUE THEN
          IO.PutError("freelist couldn't move full segment to back of list.\n");
        END;
      END;
      RETURN FALSE;
    END;

    RETURN TRUE;

  END CleanNextSegment;


PROCEDURE CleanSegment(self:T; segmentToClean:CARDINAL):BOOLEAN =

  (* Used in stages 1 and 2 *)
  VAR currDiskAddress : DiskAddress;
      currINodeInfo   : INodeInfo.T;
      currINode       : INode.T;
      segSumEntry     : SegSummaryEntry;
      bytesInBlock    : CARDINAL;

      dataInfoArray, metaInfoArray:INodeInfoArray.T;

      (* used for stage 3 *)
      arraySize       : CARDINAL;
      clumpStart      : ShortCard;
      clumpEnd        : ShortCard;
      currentINum     : ShortCard;
      nextINum        : ShortCard;
      endOfClumpFlag  : BOOLEAN;
      endOfArrayFlag  : BOOLEAN;
(*      iNodeUpdateArray: REF ARRAY OF INodeInfo.T; *)

      deadFlag        : BOOLEAN;
      totalLive       : CARDINAL;

      tempC           : CARDINAL;
      entries         : CARDINAL;  (* entries in array returned by getSegmentSummary *)

      nextSegDescALoc : DiskAddress;

      epoch           : INTEGER;

  BEGIN

    IF FUNOUTPUT = TRUE THEN
      IO.Put("Cleaner.CleanSegment called on segment:"&Fmt.Int(segmentToClean)&".\n");
    END;

    bytesInBlock := self.segInfo.bytesInBlock;

    self.currentSegmentNumber := segmentToClean;

    (* Find if this segment is reserved by the segment buffer *)
    (* If it is then move on to the next one *)
    IF self.segBuffer.getCurrentSegment()=self.currentSegmentNumber THEN
      IF FUNOUTPUT = TRUE THEN
        IO.Put("Attempt to clean segment reserved by segBuffer, skipping segment "&
          Fmt.Int(self.currentSegmentNumber)&".\n");
      END;
      RETURN FALSE;
    END;


    (* info arrays for collecting positions of live blocks *)
    dataInfoArray:= NewINodeInfoArray(self.segInfo.blocksInSegment);
    metaInfoArray:= NewINodeInfoArray(self.segInfo.blocksInSegment);


    (********************************************************)
    (* Part 1, determining which blocks are live, summing their count *)
    IF DEBUG = TRUE THEN
      IO.Put("\n*********************************\n");
      IO.Put("Cleaner Stage 1 Starting.\n");
      IO.Put("Cleaning Segment Number " & 
        Fmt.Int(self.currentSegmentNumber) & ".\n");
    END;

    self.liveCount := 0;

    (* load segment descriptor *)
    TRY
      entries := SegBuffer.GetSegmentSummary(self.segInfo,
                                             self.currentSegmentNumber,
                                             self.currentSegmentDescriptor,
                                             nextSegDescALoc,
                                             epoch);

      IF FUNOUTPUT = TRUE THEN
        IO.Put("Cleaner got segment descriptor with "&Fmt.Int(entries)&" entries.\n");
      END;

      IF DEBUG = TRUE THEN
        IO.Put("Loaded.\n");
      END;
    EXCEPT
    ELSE
      (* XXX. Do whatever necessary *)
      RETURN FALSE;
    END;

    currDiskAddress.segment := self.currentSegmentNumber;

    (* cycle through segment descriptor entries, checking for their 
       life status.*)

    IF DEBUG = TRUE THEN
      IO.Put("Counting live data blocks in segment");
    END;

    FOR i:= 0 TO entries-1 DO
      deadFlag := FALSE;

      segSumEntry:=
          VIEW(SUBARRAY(self.currentSegmentDescriptor^,
                        i*BYTESIZE(SegSummaryEntry),
                        BYTESIZE(SegSummaryEntry)),
               SegSummaryEntry);

      (* if the flag says DATA then check to see if it is live data *)
      IF segSumEntry.flag = LFSTypes.DATA THEN


        (* First we check to see if the value is already illegal, if so
           we don't need to check on disk *)
        IF (segSumEntry.iNode # ILLEGALDISKVALUE AND
          segSumEntry.offset # ILLEGALDISKVALUE) THEN

          TRY
            currINode := self.iMap.getINode(segSumEntry.iNode);
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
            IF (currINode.verifyData(INodeBlockInfo{currDiskAddress,
                                                    segSumEntry.offset}) = TRUE)
             THEN
              self.liveCount := self.liveCount + 1;

              IF DEBUG = TRUE THEN
                IO.Put(".");
              END;

              currINodeInfo := NEW(INodeInfo.T);
              
              currINodeInfo.iNode:=segSumEntry.iNode;
              currINodeInfo.block:=segSumEntry.offset;
              currINodeInfo.diskAddress:=currDiskAddress;
              
              TRY
                EVAL dataInfoArray.addElement(currINodeInfo);
              EXCEPT
              | INodeInfoArray.INodeInfoArrayError =>
                (* Well at the moment this exception is never raised *)
                IF FUNOUTPUT = TRUE THEN
                  IO.Put("Warning, cleaner recieved error adding "&
                  "element to INodeInfoArray\n");
                END;
              END;

            END;

          END;

        END; (* end if diskAddress legal *)

      END; (* end if flag says DATA *)

    END;  (*  end FOR loop *)


    IF DEBUG = TRUE THEN
      IO.Put("\n"&Fmt.Int(self.liveCount)&" live blocks counted.\n");
    END;

    totalLive := self.liveCount;

    IF totalLive > LIFETHRESHOLD THEN

      IF FUNOUTPUT = TRUE THEN
        IO.Put("segment:"&Fmt.Int(segmentToClean)&" contained "&
          Fmt.Int(totalLive)&" live data segments, cleaning aborted.\n");
      END;
      RETURN FALSE;
    END;


    (********************************************************)
    (* Part 2, if there are few enough live data blocks, we clean the segment *)
    IF DEBUG =TRUE THEN
      IO.Put("\n*********************************\n");
      IO.Put("Beginning Cleaner Stage 2.\n");
    END;

    tempC := bytesInBlock*(self.segInfo.blocksInSegment - self.segInfo.blocksForSummary);

    IF self.bigBuffer = NIL OR NUMBER(self.bigBuffer^) < tempC THEN
      IF FUNOUTPUT = TRUE THEN
        IO.Put("\nAllocating cleaner buffer of size:"&Fmt.Int(tempC)&".\n");
      END;
      self.bigBuffer := NEW(REF ARRAY OF CHAR,tempC);
    END;


    (* read entire segment into bigBuffer *)
    TRY
      self.segInfo.read(DiskAddress{self.currentSegmentNumber,0},
                  (* XXX self.bigBuffer, *)
                        self.bigBuffer^,
                        0,
                        tempC); 
    EXCEPT
    | Segment.OffsetOutOfBounds =>
      IF FUNOUTPUT = TRUE THEN
        IO.Put("Cleaner recieved OffsetOutOfBounds error "&
          "from segment.read()\n");
        IO.Put("Aborting cleaning.\n");
      END;
      RETURN FALSE;
    | Segment.SegmentOutOfBounds =>
      IF FUNOUTPUT = TRUE THEN
        IO.Put("Cleaner recieved SegmentOutOfBounds error "&
          "from segment.read()\n");
        IO.Put("Aborting cleaning.\n");
      END;
      RETURN FALSE;
    | Segment.BadnumBytesValue =>
      IF FUNOUTPUT = TRUE THEN
        IO.Put("Cleaner recieved Segment.BadnumBytesValue error "&
          "from segment.read()\n");
        IO.Put("Aborting cleaning.\n");
      END;
      RETURN FALSE;
    END;


    (* sort the inode info array *)
    IF DEBUG = TRUE THEN
      IO.Put("sorting");
    END;

    EVAL dataInfoArray.sort();

    (* For each live block that we cached *)
    (* yes 1 is the first element *)
    FOR i:= 1 TO dataInfoArray.size() DO
                          
      (* we must read it from disk to buffer, then
         from buffer to LFS write buffer *)
      (* Seems like lfs write might provide a function
         so that only one copy is needed, but this is
         fine for now...*)
      TRY
        currINodeInfo := dataInfoArray.getElement(i);
      EXCEPT
      | INodeInfoArray.INodeInfoArrayError =>
        (* at this point this exception is never raised *)
      END;
        
      (* now write that block to the LFS write buffer *)

      segSumEntry := 
          VIEW(SUBARRAY(self.currentSegmentDescriptor^,
                        currINodeInfo.diskAddress.block*BYTESIZE(SegSummaryEntry),
                        BYTESIZE(SegSummaryEntry)),
               SegSummaryEntry);

      currINodeInfo.diskAddress :=
          self.segBuffer.write(self.bigBuffer,
                               segSumEntry,
                               bytesInBlock,                               
                               currINodeInfo.diskAddress.block*bytesInBlock,
                               FALSE);

      IF DEBUG = TRUE THEN
        IO.Put(".");
      END;

    END;  (* end of writing live blocks to system buffer *)
    

      

    (********************************************************)
    (* Part 3      
       All right, now we've written all the live blocks to the
       LFS write buffer and recorded exactly where in the dataInfoArray
       Now we need to tell each INode where we moved the blocks *)
    
    (* We have an array sorted by inode, so inodes are guaranteed to be
       clustered *)
    (* Find the last of the current inode cluster, send that SUBARRAY
       to that iNode.update, then move counter to next inode clump,
       until cursor is out of range *)

    IF DEBUG = TRUE THEN
      IO.Put("\n*********************************\n");
      IO.Put("Beginning Cleaner Stage 3.\n");
    END;
    
    arraySize := dataInfoArray.size();
    
    IF DEBUG = TRUE THEN
      IO.Put("dataInfoArray size is:"&Fmt.Int(arraySize)&".\n");
    END;

    (* if there are any blocks to copy *)
    IF arraySize > 0 THEN

      clumpStart := 1;
      clumpEnd   := 1;

      TRY
        currINodeInfo := dataInfoArray.getElement(clumpEnd);
      EXCEPT
      | INodeInfoArray.INodeInfoArrayError =>
        IF FUNOUTPUT = TRUE THEN
          IO.Put("INodeInfoArray.INodeInfoArrayError raised.\n");
        END;
        (* at this point this exception is never raised *)
      END;

      currentINum := currINodeInfo.iNode;

      endOfArrayFlag := FALSE;
      
      WHILE endOfArrayFlag # TRUE DO
        
        IF DEBUG = TRUE THEN
          IO.Put(".");
        END;

        TRY
          currentINum  := (dataInfoArray.getElement(clumpEnd)).iNode;
        EXCEPT
        | INodeInfoArray.INodeInfoArrayError =>
          (* at this point this exception is never raised *)
        END;

        clumpStart   := clumpEnd;
        clumpEnd     := clumpEnd+1;

        endOfClumpFlag := FALSE;
        
        WHILE (endOfClumpFlag # TRUE AND endOfArrayFlag # TRUE) DO

          IF clumpEnd>arraySize THEN
            endOfArrayFlag := TRUE;
            endOfClumpFlag := TRUE;

          ELSE (* else we aren't at end of array *)

            TRY
              nextINum := (dataInfoArray.getElement(clumpEnd)).iNode;
            EXCEPT
            | INodeInfoArray.INodeInfoArrayError =>
              (* at this point this exception is never raised *)
            END;

            IF currentINum=nextINum THEN
              clumpEnd := clumpEnd + 1;
            ELSE
              endOfClumpFlag := TRUE;
            END;

          END;
          
        END;  (* end of clump is found by this loop *)


        (* load the inode and update it with the clump of changes *)
        WITH updateArray =  SUBARRAY(dataInfoArray.getArray()^,
                                     clumpStart-1,
                                     clumpEnd-clumpStart) DO

          TRY
            WITH inode = NARROW(self.iMap.getINode(currentINum),INode.T) DO

              EVAL inode.update(updateArray,NUMBER(updateArray));
              inode.flush();
            END;
          EXCEPT
          | IMap.InvalidINum =>
            IF FUNOUTPUT = TRUE THEN
              IO.Put("Cleaner attempted to get invalid iNum from IMap\n");
              IO.Put("Aborting Cleaning\n");
            END;
            RETURN FALSE;
          END;

        END;

        (* This check is prolly unnecessary *)
        IF clumpEnd > arraySize THEN
          endOfArrayFlag := TRUE;
        END;
      END;
    END;   (* end of IF arraySize > 0 *)

    (*******************************************************)
    (* end of cleaning segment data blocks *)
    



    (********************************************************)
    (* Part 4, determining which inode meta data blocks are live, summing their count *)
    IF DEBUG = TRUE THEN
      IO.Put("\n*********************************\n");
      IO.Put("Cleaner Stage 4 Starting, finding live inode meta-data blocks.\n");
      IO.Put("Cleaning Segment Number " & 
        Fmt.Int(self.currentSegmentNumber) & ".\n");
    END;

    self.liveCount := 0;

    (* cycle through segment descriptor entries, checking for their 
       life status.*)

    IF DEBUG = TRUE THEN
      IO.Put("Counting live meta data blocks in segment.\n");
    END;

    FOR i:= 0 TO (entries-1) DO

      deadFlag := FALSE;

      segSumEntry :=
          VIEW(SUBARRAY(self.currentSegmentDescriptor^,
                        i*BYTESIZE(SegSummaryEntry),
                        BYTESIZE(SegSummaryEntry)),
               SegSummaryEntry);

      (* only examine block if flag says it is meta data *)
      IF segSumEntry.flag = LFSTypes.META THEN

        (* First we check to see if the value is already illegal, if so
           we don't need to check on disk *)
        IF (segSumEntry.iNode # ILLEGALDISKVALUE AND
          segSumEntry.offset # ILLEGALDISKVALUE) THEN
          
          TRY
            currINode := self.iMap.getINode(segSumEntry.iNode);
          EXCEPT
          | IMap.InvalidINum =>

            deadFlag := TRUE;

            (* this situation can occur if iNode has been deleted since segment
               was written.*)

          END;

          IF deadFlag = FALSE THEN

            currDiskAddress.block   := i;

            (* if the meta data block is alive then increment live block counter
               and ADDRESS that block to the iNode info array *)
            IF (currINode.verifyMeta(INodeBlockInfo{currDiskAddress,
                                                    segSumEntry.offset}) = TRUE)
             THEN


              IF DEBUG = TRUE THEN
                IO.Put(Fmt.Int(i));
                IO.Put(".");
              END;


              self.liveCount := self.liveCount + 1;            
              currINodeInfo := NEW(INodeInfo.T);
              
              currINodeInfo.iNode:=segSumEntry.iNode;
              currINodeInfo.block:=segSumEntry.offset;
              currINodeInfo.diskAddress:=currDiskAddress;
              
              TRY
                EVAL metaInfoArray.addElement(currINodeInfo);
              EXCEPT
              (* Well at the moment this exception is never raised.
              | INodeInfoArray.INodeInfoArrayError =>
                (* Well at the moment this exception is never raised *)
                IF FUNOUTPUT = TRUE THEN
                  IO.Put("Warning, cleaner recieved error when adding "&
                    "element to INodeInfoArray\n");
                END;
	      *)
              END;

            ELSE
              (*
                self.currentSegmentDescriptor[i].iNode := ILLEGALDISKVALUE;        
                self.currentSegmentDescriptor[i].offset:= ILLEGALDISKVALUE;
              *)
            END;
          END;

        END; (* end if diskAddress legal *)

      END; (* end if entry flag said META data *)

    END;  (*  end FOR loop *)

    IF FUNOUTPUT = TRUE THEN
      IO.Put("\n"&Fmt.Int(self.liveCount)&" live blocks counted.\n");
    END;

    totalLive := totalLive + self.liveCount;

    (********************************************************)
    (* Part 5 was rendered obsolete *)


    (********************************************************)
    (* Part 6     
       Alright, now we've remembered all the live inode meta data blocks in the
       segment, next we ask the responsible inode to write those blocks out for
       us.
    *)
    
    (* We have an array sorted by inode, so inodes are guaranteed to be
       clustered *)
    (* Find the last of the current inode cluster, send that SUBARRAY
       to that iNode.update, then move counter to next inode clump,
       until cursor is out of range *)
    IF DEBUG = TRUE THEN
      IO.Put("\n*********************************\n");
      IO.Put("Beginning Cleaner Stage 6, moving inode meta-data\n");
    END;

    (* sort the inode info array *)
    EVAL metaInfoArray.sort();

    arraySize := metaInfoArray.size();
    
    IF DEBUG = TRUE THEN
      IO.Put("metaInfoArray size is:"&Fmt.Int(arraySize)&".\n");
    END;

    (* if there are any blocks to move *)
    IF arraySize > 0 THEN

      clumpStart := 1;
      clumpEnd   := 1;

      TRY
        currINodeInfo := metaInfoArray.getElement(clumpEnd);
      EXCEPT
      | INodeInfoArray.INodeInfoArrayError =>
        IF FUNOUTPUT = TRUE THEN
          IO.Put("INodeInfoArray.INodeInfoArrayError raised.\n");
        END;
        (* at this point this exception is never raised *)
      END;
      
      currentINum := currINodeInfo.iNode;

      endOfArrayFlag := FALSE;
      
      WHILE endOfArrayFlag # TRUE DO
        
        IF DEBUG = TRUE THEN
          IO.Put(".");
        END;

        TRY
          currentINum  := (metaInfoArray.getElement(clumpEnd)).iNode;
        EXCEPT
        | INodeInfoArray.INodeInfoArrayError =>
          (* at this point this exception is never raised *)
        END;

        clumpStart   := clumpEnd;
        clumpEnd     := clumpEnd+1;

        endOfClumpFlag := FALSE;
        
        WHILE (endOfClumpFlag # TRUE AND endOfArrayFlag # TRUE) DO

          IF clumpEnd>arraySize THEN
            endOfArrayFlag := TRUE;
            endOfClumpFlag := TRUE;

          ELSE (* else we aren't at end of array *)

            TRY
              nextINum := (metaInfoArray.getElement(clumpEnd)).iNode;
            EXCEPT
            | INodeInfoArray.INodeInfoArrayError =>
              (* at this point this exception is never raised *)
            END;

            IF currentINum=nextINum THEN
              clumpEnd := clumpEnd + 1;
            ELSE
              endOfClumpFlag := TRUE;
            END;

          END;

        END;  (* end of clump is found by this loop *)

        WITH updateArray =  SUBARRAY(metaInfoArray.getArray()^,
                                     clumpStart-1,
                                     clumpEnd-clumpStart) DO

          TRY
            WITH inode = NARROW(self.iMap.getINode(currentINum),INode.T) DO
              EVAL inode.updateMeta(updateArray,NUMBER(updateArray));
              inode.flush();
            END;
          EXCEPT
          | IMap.InvalidINum =>
            IF FUNOUTPUT = TRUE THEN
              IO.Put("Cleaner attempted to get invalid iNum from IMap\n");
              IO.Put("Aborting Cleaning\n");
            END;
            RETURN FALSE;
          END;

        END;

        (* This check is prolly unnecessary *)
        IF clumpEnd > arraySize THEN
          endOfArrayFlag := TRUE;
        END;
        
      END;
    END;   (* end of IF arraySize > 0 *)

    (* end of inode meta-data cleaning *)
    (********************************************************)


    IF DEBUG THEN
      IO.Put("\nInodes have been updated with new meta-data.\n");
    END;

    (* At this point, all the iNodes have been notified of
       any changes in their block positions

       All live contents of the segment have been moved,
       the segment has been cleaned.
    *)

    IF FUNOUTPUT THEN
      IO.Put("Copied "&Fmt.Int(totalLive)&" live blocks from segment"&
	     Fmt.Int(self.currentSegmentNumber)& ", "&
	     Fmt.Int((((self.segInfo.blocksInSegment-totalLive)*100) DIV
			self.segInfo.blocksInSegment))&
        "% of segment freed.\n");
    END;

    RETURN TRUE;

  END CleanSegment;
    

PROCEDURE Initialize(self: T;
		     segInfo: Segment.T;
		     buffer: SegBuffer.Buffer;
		     iMap:IMap.T;
		     lock: LFSLock.T) : T=
  BEGIN
    
    (* for counting how many live blocks there are in a segment *)
    self.liveCount:= 0; 

    self.currentSegmentNumber:= 0;
    
    self.segInfo := segInfo;  
    self.segBuffer:= buffer;
    self.iMap:=iMap;
    self.lock := lock;
    self.continue := FALSE;	(* by default, cleaner is stopped *)
    self.mu := NEW(MUTEX);
    self.cond := NEW(Thread.Condition);
    
    (* Alloc an array to hold segment descriptors *)
    self.currentSegmentDescriptor := 
        NEW(REF ARRAY OF CHAR,
            self.segInfo.blocksInSegment* BYTESIZE(SegSummaryEntry));
    
    (* used for storing the block data before it is written *)
    self.blockCharArray := NEW(REF ARRAY OF CHAR,
                               self.segInfo.bytesInBlock);
  
    self.bigBuffer := NIL;

    (* fork thread for myself *)
    Launch(self);
    
    RETURN self;
    
  END Initialize;

(* Function to call to fork cleaner *)
PROCEDURE Launch(self: T) =
  BEGIN
    IF DEBUG THEN IO.Put("LaunchCleaner called.\n"); END;
      
    (* fork the cleaner control function here *)
    EVAL ThreadExtra.PFork(CleanerControl, self);
  END Launch;


BEGIN
  (* export myself to others *)
  EVAL CleanerInterface.Export(NEW (Auth.AuthAlways));
END Cleaner.

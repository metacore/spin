(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Cleaned up.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Improved performance by eliminating unnecessary copyblock operation
 *	and memory allocation.  Changed not to use WriteSize().
 *	Changed Read() I/F to use VAR ARRAY OF CHAR, not REF ARRAY OF CHAR.
 *
 * 10-Sep-96  Eric Christoffersen (ericc) at the University of Washington
 *	Added RefWrite, which allows for multiple blocks to be written to segBuffer with
 *	a single call.  Removed many memory allocations.
 *
 * 14-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Replaced flush, forceflush, bufferInit, getSegmentSummary.  
 *      Flush-stuff now only writes dirty data to disk (with segment descriptors).  
 *	New segment descriptors surround data and enable cleaner and roll
 *	forward to detect invalid portions of segment.
 *
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)
MODULE  SegBuffer;

(* IMPORTS *)

IMPORT  IO,Fmt;
IMPORT  Segment;
IMPORT  FastBuffer;
IMPORT  Error;
(* IMPORT  Debugger; *)

CONST DEBUG = FALSE;
      (* DATA and ILLEGALDISKVALUE must be same as LFSTypes.* *)
      DATA  = 0;
      ILLEGALDISKVALUE = 65535;
      SUMMARYENTRYSIZE = BYTESIZE(Segment.SegSummaryEntry);

(* Used only by FlushToDisk and GetSegumentSummary
 * note: segment summary area consists of
 *	- SegSummaryHeader (1 block)
 *	- actuall data including meta data (n blocks)
 *	- SegSummaryHeader (1 block)
 *)
TYPE SegSummaryHeader = RECORD
    blocks : INTEGER;		(* number of data blocks in a segment summary *)
    epoch : INTEGER;		(* epoch for a summary *)
    nextLoc : Segment.DiskAddress; (* loc of next segment summary area *)
  END;
  (* Used only by FlushToDisk and GetSegumentSummary
   * note: segment summary area consists of
   *	- SegSummaryHeader (1 block)
   *	- actuall data including meta data (n blocks)
   *	- segment summary ( BYTESIZE(SegSummaryEntry) * n = 6 * n bytes. 
   *			    rounded up to blocks)
   *	- SegSummaryHeader (1 block)
   *	Only number of data blocks and epoch followed data blocks in the  
   *	first implementation.  I've changed to put the copy of segment summary
   *	header at both first and last block as 1 block is big enough to hold
   *	it and it does not harm anything.  (owa)
   *)

REVEAL Buffer = Public BRANDED
OBJECT
  blocks	   : REF ARRAY OF CHAR;
  summaryEntries   : REF ARRAY OF CHAR;
				(* needs to be char for fast writing to disk *)
  writeptr         : CARDINAL;  (* location of write pointer, in blocks *)
  dirtyPtr         : CARDINAL;
	(* points to the first dirty (unwritten) data block in segBuffer *)
  bufflength       : CARDINAL;  (* length of segBuffer in bytes *)
  currentSeg       : CARDINAL;  
  segInfo          : Segment.T;
  mu               : MUTEX;
  epoch            : INTEGER;
OVERRIDES
  write             := Write;
  refWrite          := RefWrite;
  read              := Read;
  init              := BufferInit;
  getEpoch          := GetEpoch;
  getOffset         := GetOffset;
  setOffset         := SetOffset;  (* used by roll-forward in LFS.m3 *)
  getCurrentSegment := GetCurrentSegment;
  setCurrentSegment := SetCurrentSegment;  (* used by roll-forward in LFS.m3 *)
  seginfo           := Seginfo;
  flushToDisk       := FlushToDisk;
  forceFlush        := ForceFlush;
END;

(* PROCEDURES *)

(* BufferInit:
 * initializes an instance of a Buffer to have a number of blocks equal to
 * length minus the necessary blocks for the segment summary data, for each
 * block to have blockSize chars, sets the arrays of segment summary entries
 * to have length minus number of summary blocks elements, and gets a free
 * segment to begin buffering If sync was called on a partially filled segment
 * then written portion of segment is read back into segBuffer
 *)
PROCEDURE  BufferInit (self              : Buffer;
                      segment            : Segment.T;
                      writeLoc           : Segment.DiskAddress) : Buffer =
VAR numBytes,entries:CARDINAL;
    nextSegDescALoc :Segment.DiskAddress;
    blocksInSegment := segment.blocksInSegment;
    bytesInBlock := segment.bytesInBlock;
BEGIN

  IF DEBUG THEN
    IO.Put("SegBuf init called, write pointer at s:"&Fmt.Int(writeLoc.segment)&
      " b:"&Fmt.Int(writeLoc.block)&".\n");
  END;

  self.bufflength := blocksInSegment * bytesInBlock;
  self.blocks := NEW(REF ARRAY OF CHAR, self.bufflength);
  self.summaryEntries:= NEW(REF ARRAY OF CHAR,blocksInSegment*SUMMARYENTRYSIZE);
  self.mu         := NEW(MUTEX);
  self.epoch      := 0;

  self.currentSeg := writeLoc.segment;
  self.writeptr   := writeLoc.block;
  self.dirtyPtr   := writeLoc.block;
  self.segInfo    := segment;

  (* read old data back into segBuffer *)
  (* get old segment descriptor info, including last epoch *)
  entries:= GetSegmentSummary(segment,
                              self.currentSeg,
                              self.summaryEntries,
                              nextSegDescALoc,
                              self.epoch);

  (* if there are valid entries to read then load the data into the segBuffer *)
  IF entries > 0 THEN
    numBytes := writeLoc.block * segment.bytesInBlock;
    WITH tmpLoc = Segment.DiskAddress{self.currentSeg, 0},
	 buf = SUBARRAY(self.blocks^, 0, numBytes)
     DO
      TRY
	segment.read(tmpLoc, buf, 0, numBytes);
	(* read a (partial) segment to in-memory buffer *)
      EXCEPT
      | Error.E(e) =>
        IO.PutError("BufferInit. " & e.message() & "\n");
      ELSE 
	IO.PutError("BufferInit. read error\n");
      END;
    END;

    IF DEBUG THEN
      IO.Put("Inited segBuffer with " & Fmt.Int(numBytes) & " of old segInfo");
    END;
  END;
  RETURN  self;

END  BufferInit;

(* Write:
   writes the block data to the current offset in the segment
   buffer where writeptr points.  It records the iNum and iOffset
   for that block in the inodes and offsets arrays.  The extent
   and segInfo parameters are needed in case the buffer needs to
   be flushed.  In which case disk info, free segments info, etc,
   are needed. Write returns the block offset in the buffer 
   of the block written. *)
PROCEDURE Write (self     : Buffer;
                 data     : Block;
                 location : Segment.SegSummaryEntry;
                 length   : CARDINAL;       (* how much to copy in buffer *)
                 from     : CARDINAL:= 0;  (* from where in buffer to start copy *)
                 lockit   : BOOLEAN := TRUE) : Segment.DiskAddress =
  VAR
    tempAddr : Segment.DiskAddress;
    bytesInBlock,blocksInSegBuf,spaceLeft : INTEGER;

    PROCEDURE WriteInternal() =
      BEGIN
      (* XXX this seems to assume there is at least one block left.. *)
      (* make sure it's correct (owa) *)
      IF ((blocksInSegBuf - self.writeptr) <=
	  BlocksForMeta(blocksInSegBuf, bytesInBlock)) THEN
	IO.Put("------- SegBuf.Write: no space left\n");
      END;

      tempAddr := Segment.DiskAddress{self.currentSeg, self.writeptr};

      (* copy the data from user array onto segBuffer *)
      WITH size = MIN(bytesInBlock,length) DO
	SUBARRAY(self.blocks^, self.writeptr * bytesInBlock, size) :=
		SUBARRAY(data^,from,size);
      END;

      (* set the segment descriptor to contain the user data *)
      WITH ptr = self.writeptr*SUMMARYENTRYSIZE DO
        SUBARRAY(self.summaryEntries^, ptr, SUMMARYENTRYSIZE) :=
	         VIEW(location, ARRAY [0..SUMMARYENTRYSIZE-1] OF CHAR);
      END;

      self.writeptr  := self.writeptr  + 1;
      spaceLeft      := blocksInSegBuf - self.writeptr;

      (* XXX comment-ed out (owa)
      IF self.writeptr-self.dirtyPtr < 0 OR
	WriteSize(self.writeptr-self.dirtyPtr,bytesInBlock) >= spaceLeft THEN
      *)

      IF spaceLeft <= BlocksForMeta(blocksInSegBuf, bytesInBlock) THEN
	IF DEBUG THEN
	  IO.Put("SegBuf.Write: flush. writeptr " & Fmt.Int(self.writeptr) &
		 ", dirtyPtr " & Fmt.Int(self.dirtyPtr) &
		 ", spaceLeft " & Fmt.Int(spaceLeft) &
		 "\n");
	END;

	TRY
	  EVAL self.flushToDisk();
	EXCEPT
	| Error.E(e) =>
	  IO.PutError("SegBuffer::WriteInternal. " & e.message() & "\n");
	| Segment.NoFreeSegs =>
	  IO.PutError("SegBuffer::WriteInternal. No free segment left.\n");
	END;
      END;
    END WriteInternal;

  BEGIN

    bytesInBlock   := self.segInfo.bytesInBlock;
    blocksInSegBuf := self.bufflength DIV bytesInBlock;

    (* if the client has requested a lock then do our stuff with a lock *)
    IF lockit THEN
      LOCK self.mu DO
	WriteInternal();
      END;
    ELSE (* no lock *)
	WriteInternal();
    END;

    IF DEBUG THEN
      IO.Put("SegBuf.Write: returning. " &
	     "new writeptr: " & Fmt.Int(self.writeptr) &
             ", s: " & Fmt.Int(tempAddr.segment) &
 	     ", b: " & Fmt.Int(tempAddr.block) & "\n");
    END;

    RETURN  tempAddr;
  END  Write;

(* 
 * RefWrite:

 * This is a faster version of write, but requires more intelligence
 * from the caller.

 * Function should be called with all data that is to be written, RefWrite then
 * writes as much of data as possible to current segBuffer.  RefWrite cannot
 * write across segBuffers as caller must record block positions in order to
 * update inode.  In order for caller to do this, RefWrite returns the number of
 * bytes of input data that were written to segBuffer.

 * Note that this method will only write whole blocks to segBuffer, caller
 * must still handle partially altered blocks.
 * Because of this, bytesWritten will always be a multiple of bytesInBlock.


 * Version of write that takes 
 *   READONLY data array,
 *   the inum of the file writing, 
 *   the offset in the file of the first block of the array (as this method
 *      wont write partial blocks)
 *   loc in array to start copying from (in bytes!)
 *   amount to copy (in blocks!) <= ensures that the user only specifies
	block amounts

 *   RETURNS the disk address where the array began to be written to disk
 *           bytesWritten returns the number of bytes actually written to disk
 *)
PROCEDURE RefWrite (self     : Buffer;
                    READONLY data     : ARRAY OF CHAR;      (* data to write *)
                    READONLY iNum     : CARDINAL;           (* inum of writer *)
                    READONLY offset   : CARDINAL;           (* offset in file where first block goes *)
                    READONLY from     : CARDINAL;           (* byte num in array to start copying *)
                    VAR   bytesWritten: CARDINAL;           (* a return value, how many bytes were actually written to the buffer *)
                    lockit   : BOOLEAN := TRUE) : Segment.DiskAddress =
  VAR
    tempAddr : Segment.DiskAddress;
    bytesInBlock,blocksInSegBuf,spaceLeft : INTEGER;
    location: Segment.SegSummaryEntry;
    totalBytesToWrite,amountBytesToWrite:CARDINAL;
    fromVar : CARDINAL;

  PROCEDURE RefWriteInternal() =
    BEGIN
      tempAddr := Segment.DiskAddress{self.currentSeg, self.writeptr};

      (* calculates number of bytes that can be written in the remainder of the segment *)
      spaceLeft := blocksInSegBuf-self.writeptr;

      IF DEBUG THEN
	IO.Put("spaceLeft INTEGER segment is:"&Fmt.Int(spaceLeft)&".\n");
      END;

      (* XXX comment-ed out (owa)
      amountBytesToWrite := (2*spaceLeft)-WriteSize(spaceLeft,bytesInBlock);
      *)

      amountBytesToWrite := spaceLeft - BlocksForMeta(blocksInSegBuf, bytesInBlock);
      IF DEBUG THEN
	(* XXX 
	IO.Put("WriteSize:"&Fmt.Int(WriteSize(spaceLeft,bytesInBlock))&".\n");
	*)
	IO.Put("prelim blocks:"&Fmt.Int(amountBytesToWrite)&".\n");
      END;

      amountBytesToWrite := amountBytesToWrite * bytesInBlock;

      IF DEBUG THEN
	IO.Put("prelim amountBytesToWrite:"&Fmt.Int(amountBytesToWrite)&".\n");
      END;

      IF amountBytesToWrite > totalBytesToWrite-bytesWritten THEN
	amountBytesToWrite := totalBytesToWrite-bytesWritten;
      END;

      IF DEBUG THEN
	IO.Put("amountBytesToWrite :"&Fmt.Int(amountBytesToWrite)&".\n");
      END;

      (* copy data into segBuffer *)
      SUBARRAY(self.blocks^,
	       self.writeptr * bytesInBlock,
	       amountBytesToWrite):=
		   SUBARRAY(data,fromVar,amountBytesToWrite);

      (* fill out segDescriptor data *)
      FOR i:= 0 TO (amountBytesToWrite DIV bytesInBlock)-1 DO
	SUBARRAY(self.summaryEntries^,
		 (self.writeptr+i)*SUMMARYENTRYSIZE,
		 SUMMARYENTRYSIZE):=
		   VIEW(location,
			ARRAY [0..SUMMARYENTRYSIZE-1] OF CHAR);
	location.offset := location.offset+1;
      END;

      fromVar:=fromVar+amountBytesToWrite;
      bytesWritten := bytesWritten + amountBytesToWrite;
      self.writeptr  :=  self.writeptr  + (amountBytesToWrite DIV bytesInBlock);
      spaceLeft      :=  blocksInSegBuf - self.writeptr;

      (* XXX comment-ed out (owa)
      IF WriteSize(self.writeptr-self.dirtyPtr,bytesInBlock) >= spaceLeft THEN
      *)
      IF spaceLeft <= BlocksForMeta(blocksInSegBuf, bytesInBlock) THEN
	IF DEBUG THEN
	  IO.Put("SegBuf.Write: flush. writeptr " & Fmt.Int(self.writeptr) &
		 ", dirtyPtr " & Fmt.Int(self.dirtyPtr) &
		 ", spaceLeft " & Fmt.Int(spaceLeft) &
		 "\n");
	END;
	IF DEBUG THEN
	  IO.Put("RefWrite flushing segBuffer to disk, has written:"&
		 Fmt.Int(bytesWritten)&" bytes to disk.\n");
	END;

	TRY
	  EVAL self.flushToDisk();
	EXCEPT
	| Error.E(e) =>
	  IO.PutError("SegBuffer::WriteInternal. " & e.message() & "\n");
	| Segment.NoFreeSegs =>
	  IO.PutError("SegBuffer::WriteInternal. No free segment left.\n");
	END;
      END;
    END RefWriteInternal;

  BEGIN

    IF DEBUG THEN
      IO.Put("RefWrite called, offset:"&Fmt.Int(offset)&
	     " from:"&Fmt.Int(from)&" arraySize:"&Fmt.Int(LAST(data))&".\n");
    END;

    fromVar := from;

    amountBytesToWrite := 0;
    bytesWritten := 0;
    totalBytesToWrite := 0;

    bytesInBlock   := self.segInfo.bytesInBlock;
    blocksInSegBuf := self.bufflength DIV bytesInBlock;

    (* set segDecriptor info. iNode and flag are invariant in this method *)
    location := Segment.SegSummaryEntry{iNum, offset, DATA};

    (* totalBytesToWrite must be an even multiple of bytesInBlock,
     * as this method only writes full blocks
     *)
    WITH t = LAST(data)+1-fromVar DO
      totalBytesToWrite := t-(t MOD bytesInBlock);
    END;

    (* XXXX
    IF NUMBER(data) > 512 THEN IF data[512] # 'i' THEN
	Debugger.Enter();
      END;
    END;
    *)

    (* if the client has requested a lock then do our stuff with a lock *)
    IF lockit THEN
      LOCK self.mu DO
	RefWriteInternal();
      END;
    ELSE (* no lock *)
	RefWriteInternal();
    END;

    IF DEBUG THEN
      IO.Put("SegBuf.RefWrite: returning. " &
	     "new writeptr: " & Fmt.Int(self.writeptr) &
             ", s: " & Fmt.Int(tempAddr.segment) &
 	     ", b: " & Fmt.Int(tempAddr.block) & "\n");
    END;
    RETURN  tempAddr;
  END  RefWrite;

(*
 * Read; copies data at loc.  When it's in the segment buffer
 *       read from on-memory buffer, otherwise read from disk.
 *	 Needed to change this way as currentSegment may change
 *       due to flush after getCurrentSegment().
 *	 See LFS::ReadFromDiskOrBuffer.
 *)
PROCEDURE Read (
    self : Buffer;
    VAR data : ARRAY OF CHAR ;
    loc : Segment.DiskAddress;
    to : CARDINAL := 0) : CARDINAL
    RAISES {BlockOutOfBounds} =
VAR
  bytes : CARDINAL;
BEGIN

  bytes := NUMBER(data);
  LOCK self.mu DO
    IF loc.segment = self.currentSeg THEN
      (* yes. the data is within the current buffer *)
      IF (loc.block>=self.bufflength) OR (loc.block>self.writeptr) THEN
        RAISE BlockOutOfBounds;
      ELSE
        IF DEBUG THEN
	  IO.Put("Segbuf.read: from self.blocks b:" & Fmt.Int(loc.block) &
	     " at " & Fmt.Int(loc.block*self.segInfo.bytesInBlock) & "\n");
        END;
        SUBARRAY(data, to, bytes)
	  := SUBARRAY(self.blocks^, loc.block*self.segInfo.bytesInBlock,bytes);
    (* XXXX
    IF NUMBER(data) > 512 THEN
      IF data[512] # 'i' THEN
	Debugger.Enter();
      END;
    END;
    *)
        RETURN self.currentSeg;
      END;
    ELSE
      (* nope. XXX Do I need to do "LOCK self.mu" in this case? *)
      (* arg: to is not used... *)
      TRY
        self.segInfo.read(loc, data, 0, bytes); 
      EXCEPT
      ELSE
	RAISE BlockOutOfBounds; 	(* XXX. what should I do?*)
      END;
      RETURN loc.segment;
    END; (* end of IF loc.segment = ... *)
  END; (* end of LOCK *)
END Read;

(* compute number of blocks needed for SegmentSammary *)
PROCEDURE BlocksForMeta(blocksInSegment, bytesInBlock : CARDINAL) : CARDINAL =
  BEGIN
    WITH
      entrySize = SUMMARYENTRYSIZE DO
      (* 1 block: blocks in write, epoch number and disk loc for next write. *)
      (* 1 block for Segment Header is already counted.. *)
      RETURN (1 + entrySize * blocksInSegment DIV bytesInBlock);
    END;
  END BlocksForMeta;

(* XXX what the hell does WriteSize() do?
 * it does not compute properly... changed not to use it.  (owa)
 *)
(* WriteSize:
   given size of data in bytes, returns number of blocks that data
   will take in segBuff, including segDesc info *)

(*
PROCEDURE WriteSize(dataSize:CARDINAL;bytesInBlock:CARDINAL):CARDINAL =
  BEGIN

    RETURN 8+dataSize+((2 + dataSize + ((SUMMARYENTRYSIZE*(dataSize+1))-1)) DIV bytesInBlock);

  END WriteSize;
*)


(* FlushToDisk
 * Flushes the dirty buffer to disk as one huge block at a time,
 * If the current segement still has enough space, current segment does
 * not change, only writeptr and dirtyPtr are updated.  If not, gets a
 * new segment from the freelist, and resets the writeptr to point
 * at the first block of the buffer.
 *	FIXME: Changed to get a new segment regardless to the free space
 *	       in the segment as the calcuration looked bogus!!! (owa).
 * Don't lock since it is currently only called from write, which is one
 * big critical section
 *)

(* writes dirty segBuffer data to disk, along with segment descriptor info

 * Does the following:
 * -- Seg Descriptor Part A --
 * 1.  Write seg descriptor header, states how long the data segment is,
 *     what the epoch is and where next Seg Descriptor starts.
 * -- Segbuffer Data ---------
 * 2.  Write data to disk
 * -- Seg Descriptor Part B --
 * 3.  Write segment descriptor info, the mapping from block to inode and 
 *    offset in inode that refers to block
 * 3.  Write seg descriptor header.  This is the exactly the same info with 1.
 * 4.  increment epoch

 * Size of segment descriptor map entry is BYTESIZE(Segment.SegSummaryEntry).
 *  2 bytes for inum, 4 bytes for inode offset = 6 bytes.  

 * Size of SegmentSummaryHeader 
 *   number of data blocks is 4 bytes, epoch is 4 bytes, diskAddress is 4 bytes

 * If blocksize is 512 bytes, for the segment buffer to flush x blocks 
 * of dirty data to disk requires:

 * 1 block for seg descriptor header (SegmentSummaryHeader).
 * (x blocks for data)
 * (6 * x) bytes for seg descriptor map (rounded up to blocks).
 * 1 block for seg descriptor header (SegmentSummaryHeader).

 * Total is: 1 + (((6 * x) + 511) DIV 512) + 1 blocks (excluding data block)

 * IF there is only one data block to flush, total is 3.  So there should
 * be at least 3 blocks left for the next segment descriptor update.
 * If a segment is 1024 blocks, maximum data blocks can be written is
 * 1010 and 14 blocks are used for segment descripor.
 * (ie; 1024 >= 1 + x + (((6 * x) + 511) DIV 512) + 1).
 *
 * Be carefull, that the first block for a SegDescA is already counted,
 *
 * If you have y blocks left in segment and y > 2 then:
 *
 * data blocks that can be written =
 *	(((y-2) * bytesInBlock) - 14) DIV (6 * bytesInBlock)

 *	FIXME:  This calcuration is bogus!!! (owa).
 *
 * This leaves enough room for the necessary segment descriptor stuff.
*)
PROCEDURE FlushToDisk  (self : Buffer): Segment.DiskAddress 
  RAISES {Error.E,Segment.NoFreeSegs} =
VAR
  numbytes : CARDINAL;
  location : Segment.DiskAddress;
  bytesInBlock, blocksInSegBuf, spaceLeft :CARDINAL;
  dataBlocksToWrite : INTEGER;
  currentBlock: CARDINAL;
  nextSegment : INTEGER := self.currentSeg;
  nextLoc : Segment.DiskAddress;
  header : SegSummaryHeader;
BEGIN

  bytesInBlock     := self.segInfo.bytesInBlock;
  blocksInSegBuf   := self.bufflength DIV self.segInfo.bytesInBlock;
  spaceLeft        := blocksInSegBuf - self.writeptr;
  nextLoc          := Segment.DiskAddress{self.currentSeg, self.writeptr};
  dataBlocksToWrite := self.writeptr-self.dirtyPtr;

  IF DEBUG THEN
    IO.Put("FlushToDisk Called on segment: " & Fmt.Int(self.currentSeg));
    IO.Put(" spaceLeft: " & Fmt.Int(spaceLeft) & ".\n");
    IO.Put(" writeptr: " & Fmt.Int(self.writeptr));
    IO.Put(" dirtyPtr: " & Fmt.Int(self.dirtyPtr));
    IO.Put(" dataBlocksToWrite: "&Fmt.Int(dataBlocksToWrite)&".\n");
  END;

  (* sanity check *)
  IF spaceLeft < 3 THEN
    IO.PutError("Error!  FlushToDisk called with less than 3 free blocks");
    IO.PutError("in buffer.\n  This should be an impossible state. ");
    IO.PutError("BEWARE the end is nigh!\n");
    RETURN nextLoc;
  END;

  IF dataBlocksToWrite < 1 THEN
    IO.Put("flushToDisk, but no dirty data in buffer, flush aborted.\n");
    RETURN nextLoc;
  END;

  (* calculate location of next segDesc A *)
  (* XXX comment-ed out (owa)
  IF WriteSize(self.writeptr-self.dirtyPtr,bytesInBlock)+8 > (blocksInSegBuf-self.dirtyPtr) THEN
  *)
  (* XXX. Hmm.... When Sync() is called. this could be wrong? (owa) *)
  IF TRUE THEN
    nextSegment := self.segInfo.segFreeList.getFreeSegAndRemove();
    WHILE nextSegment = self.currentSeg DO
      IF DEBUG THEN IO.Put("getFreeSegAndRemove returns current seg\n"); END;
      nextSegment := self.segInfo.segFreeList.getFreeSegAndRemove();
    END;
    nextLoc.segment := nextSegment;
    nextLoc.block   := 0;
  ELSE
    (* XXX comment-ed out (owa)
    nextSegment     := self.currentSeg;
    nextLoc.segment := self.currentSeg;
    nextLoc.block   := self.dirtyPtr +
			WriteSize(self.writeptr-self.dirtyPtr,bytesInBlock)-6;
    *)
  END;

  IF DEBUG THEN
    IO.Put("Flush determined that next SegDescA Loc is s:"&
      Fmt.Int(nextLoc.segment)&" b:"&Fmt.Int(nextLoc.block)&".\n");
  END;

  (********************************)
  (* Write SegDescA-- first block *)
  (* write values for segDesc A   *)
  (*     - count of data blocks   *)
  (*     - epoch                  *)
  (*     - loc of next segDescA   *)

  (* set up the header *)
  header := SegSummaryHeader{dataBlocksToWrite, self.epoch, nextLoc};

  (* SegSummaryHeader is always placed at the beginging of a block *)
  currentBlock := self.dirtyPtr-1;
  SUBARRAY(self.blocks^, currentBlock*bytesInBlock, BYTESIZE(SegSummaryHeader))
	:= VIEW(header, ARRAY [0..BYTESIZE(SegSummaryHeader)-1] OF CHAR);

  (* skip over data blocks *)
  currentBlock := currentBlock + dataBlocksToWrite + 1;

  (* write seg summary map *)
  WITH to = currentBlock*bytesInBlock,
       from = (currentBlock-dataBlocksToWrite)*SUMMARYENTRYSIZE
   DO
    numbytes := dataBlocksToWrite * SUMMARYENTRYSIZE;
    SUBARRAY(self.blocks^, to, numbytes):=
	SUBARRAY(self.summaryEntries^, from, numbytes);
  END;

  IF DEBUG THEN
    IO.Put("summary at " & Fmt.Int(currentBlock) &
	   " bytes " & Fmt.Int(numbytes) & "\n");
  END;

  (* to point to final block by rounding up the bytes *)
  currentBlock := currentBlock + ((numbytes+ bytesInBlock -1) DIV bytesInBlock);

  (* now write final block, contains SegSummaryHeader as well *)
  SUBARRAY(self.blocks^, currentBlock*bytesInBlock, BYTESIZE(SegSummaryHeader))
	:= VIEW(header, ARRAY [0..BYTESIZE(SegSummaryHeader)-1] OF CHAR);

  currentBlock := currentBlock + 1;

  (* write out all data to disk *)
  location := Segment.DiskAddress{self.currentSeg, self.dirtyPtr-1};
  numbytes := (currentBlock-self.dirtyPtr+1)*bytesInBlock;
  IF DEBUG THEN
    IO.Put("flush writing segDescA to disk at:"&Fmt.Int(location.segment)&
        " b:"&Fmt.Int(location.block)&".\n");
  END;
  TRY
    self.segInfo.write(location,self.blocks,(self.dirtyPtr-1)*bytesInBlock,numbytes);
  EXCEPT
    Segment.BadnumBytesValue, Segment.OffsetOutOfBounds, Segment.SegmentOutOfBounds =>
    IF DEBUG THEN
      IO.Put("Tried to flush to disk with segment = " & Fmt.Int(location.segment) & " block = " &
        Fmt.Int(location.block) & "numbytes = " & Fmt.Int(numbytes) & " and failed.\n");
    END;
  END;

  (* If we grab a new segment, we increment the epoch and update writeptr.*)
  IF nextSegment # self.currentSeg THEN

    IF self.epoch < LAST(INTEGER)-1 THEN
      INC(self.epoch,1);
    ELSE
      self.epoch:= 0;
    END;

    self.currentSeg  := nextSegment;
    self.writeptr    := 1;
      (* this sets it to one so segDesc A can be written into the first block *)
    self.dirtyPtr    := self.writeptr;
  ELSE
    self.writeptr    := nextLoc.block+1;
    self.dirtyPtr    := self.writeptr;
  END;

  nextLoc.block := nextLoc.block + 1;

  IF DEBUG THEN
    IO.Put("Flush to disk returning s:"&Fmt.Int(nextLoc.segment)&" b:"&
    Fmt.Int(nextLoc.block)&".\n");
  END;

  RETURN nextLoc;

END  FlushToDisk;

(* 
   method reads in successive segDescriptors from a given segment until:
   a) segment ends
   b) invalid segment summary is read in

   method populates the entries array with segDescriptor info

   returns number of entries added to array, 
   locNextSegDescA holds location of next segDescA,
      this location is needed by roll-forward to detect when to stop
      rolling forward.
   epoch returns the epoch of the last segment descriptor that was read in
*)
PROCEDURE GetSegmentSummary(segment:Segment.T;
                            READONLY segmentNumber:CARDINAL;
				(* number of segment to get segSummary from *)
                            entries: REF ARRAY OF CHAR;
                            VAR locNextSegDescA : Segment.DiskAddress;
                            VAR epoch:INTEGER): CARDINAL =
  VAR
    dataBlocks  : INTEGER;
    entryBlocks : CARDINAL;
    block       : FastBuffer.T;
    loc : Segment.DiskAddress;
    totalEntryCount :CARDINAL := 0;
    numBytes    : CARDINAL;
    entryIndex  : CARDINAL:=0;
    headerA,headerB : SegSummaryHeader;

  BEGIN

    IF DEBUG THEN IO.Put("GetSegmentSummary called.\n"); END;
    (*
     * As GetSegmentSummary() only copies *existing* SegSummaryEntry 
     * to the 'entries' and at least cleaner seaches entire 'entries'
     * to find dead block, need to mark each entry invalid first.
     *)
    WITH illegal = ILLEGALDISKVALUE,
	 dummy = Segment.SegSummaryEntry{illegal, illegal, illegal} DO
      FOR i:=0 TO segment.blocksInSegment-1 DO
	VIEW(SUBARRAY(entries^, i * SUMMARYENTRYSIZE, SUMMARYENTRYSIZE), 
	     Segment.SegSummaryEntry) := dummy;
      END;
    END;

    block := FastBuffer.Allocate(segment.bytesInBlock);
    loc := Segment.DiskAddress{segmentNumber, 0};

    WHILE loc.segment = segmentNumber DO
      IF DEBUG THEN IO.Put("Reading A at blk:"&Fmt.Int(loc.block)&".\n"); END;

      (* read first block into memory *)
      numBytes := NUMBER(block.data^);
      TRY
	segment.read(loc, block.data^, 0, numBytes);
      EXCEPT
      ELSE
	IO.PutError("GetSegmentSummary. can't read.\n");
      END;

      INC(loc.block);

      (* get the SegSummaryHeader from the first block *)
      VIEW(headerA, ARRAY [0..BYTESIZE(SegSummaryHeader)-1] OF CHAR) :=
	SUBARRAY(block.data^, 0, BYTESIZE(SegSummaryHeader));

      (* get data length from first block *)
      dataBlocks := headerA.blocks;

      (* this blocksInSegment-2 is sloppy,
       * value should be smaller, but this works
       *)
      IF dataBlocks>(segment.blocksInSegment-2) OR dataBlocks<1 THEN
        loc.segment := LAST(Segment.ShrtCard);	(* to exit WHILE *)
      ELSE

        IF DEBUG THEN
          IO.Put("dataBlocks="&Fmt.Int(dataBlocks)&".\n"&
            "nextLocationCheck is s:"&Fmt.Int(headerA.nextLoc.segment)&
            " b:"&Fmt.Int(headerA.nextLoc.block)&".\n"&
            "epochA is:"&Fmt.Int(headerA.epoch)&".\n");
        END;

        entryIndex := loc.block;

        (* skip data blocks *)
        loc.block := loc.block + dataBlocks;

        (* read segmentSummary map into memory *)
        entryBlocks:= (dataBlocks*SUMMARYENTRYSIZE+segment.bytesInBlock-1)
			DIV segment.bytesInBlock;
        
        IF DEBUG THEN
          IO.Put("\nReading SegSumEntry w/ "&Fmt.Int(dataBlocks)&" entries.\n");
        END;

        numBytes := dataBlocks*SUMMARYENTRYSIZE;
	WITH buf = SUBARRAY(entries^,
			    entryIndex*SUMMARYENTRYSIZE,
			    dataBlocks*SUMMARYENTRYSIZE) DO
	  TRY
	    segment.read(loc, buf, 0, numBytes);
	  EXCEPT
	  ELSE
	    IO.PutError("GetSegmentSummary.  can't read...\n");
	  END;
	END;

        loc.block := loc.block + entryBlocks;

        totalEntryCount := totalEntryCount + dataBlocks;

        (* read last block of segDesc into memory *)
        IF DEBUG THEN
          IO.Put("Reading LAST block of segDesc into memory from s:"&
		Fmt.Int(segmentNumber)& " b:"&Fmt.Int(loc.block)&".\n");
        END;

        numBytes := NUMBER(block.data^);
	TRY
	  segment.read(loc, block.data^, 0, numBytes);
	EXCEPT
	ELSE
	  IO.PutError("GetSegmentSummary.  can't read...\n");
	END;

        (* get the SegSummaryHeader from the last block *)
        VIEW(headerB, ARRAY [0..BYTESIZE(SegSummaryHeader)-1] OF CHAR) :=
	  SUBARRAY(block.data^, 0, BYTESIZE(SegSummaryHeader));

        (* now verify that the nextLocation given in the last block is the
	   same as the nextLocation from the first block, if yes then this
	   segDescriptor is valid, otherwise invalid.*)
        IF headerA = headerB THEN
	  loc := headerA.nextLoc;

          IF DEBUG THEN
            IO.Put("getSummary finishing fetch loop with nextLocation s:"&
		Fmt.Int(loc.segment)&" b:"& Fmt.Int(loc.block)&
		" epochB:"&Fmt.Int(headerB.epoch)&".\n"); 
          END;

          epoch := headerA.epoch;
          IF DEBUG THEN IO.Put("read epoch:"&Fmt.Int(epoch)&".\n"); END;

        ELSE
          IF DEBUG THEN
            IF headerA.epoch # headerB.epoch THEN
              IO.Put("EpochA:"&Fmt.Int(headerA.epoch)&
		     " EpochB:"&Fmt.Int(headerB.epoch)&".\n");
            END;

            IO.Put("Found invalid segment descriptor, halting descriptor " &
	      "reclamation and returning:"&
              Fmt.Int(totalEntryCount)&" entries.\n"&
              "nextLocation was s:"&Fmt.Int(headerB.nextLoc.segment)&
              " b:"&Fmt.Int(headerB.nextLoc.block)&
              " check was s:"&Fmt.Int(headerA.nextLoc.segment)&" b:"&
              Fmt.Int(headerB.nextLoc.block)&".\n");
          END;          

          loc.segment := LAST(Segment.ShrtCard);
          totalEntryCount := totalEntryCount - dataBlocks;
        END;
      END;
    END; (* end while not at end of segment *)

    IF DEBUG THEN
      IO.Put("Returning array w/"&Fmt.Int(totalEntryCount)&
	     " segDesc entries.\n");
      IO.Put("Returning current epoch as :"&Fmt.Int(epoch)&".\n");
    END;

    (* this returns the location of the next segDescA if it is in another
     segment, or a clearly bogus value if a corrupt segmentDescriptor was
     found *)
    locNextSegDescA := loc;

    FastBuffer.Deallocate(block);
    RETURN totalEntryCount;

  END GetSegmentSummary; 




PROCEDURE GetEpoch(self:Buffer):INTEGER=
  BEGIN
    RETURN self.epoch;
  END GetEpoch;




(* this should have a check to prevent people from doing stupid stuff *)
PROCEDURE SetOffset (self:Buffer; newOffset:CARDINAL) =
  BEGIN
    self.writeptr := newOffset;
  END SetOffset;

  
(* shouldn't have to lock these next two *)

(* GetOffset:
   returns the offset of the writeptr in the buffer *)

PROCEDURE  GetOffset  (self  :  Buffer)  :  CARDINAL  =  
BEGIN
  RETURN  self.writeptr;
END  GetOffset;

(* GetCurrentSegment:
   returns the current segment begin buffered *)

PROCEDURE GetCurrentSegment (self : Buffer) : CARDINAL =
BEGIN
  RETURN self.currentSeg;
END GetCurrentSegment;


PROCEDURE SetCurrentSegment(self:Buffer;newSegNum:CARDINAL)=
  BEGIN
    self.currentSeg := newSegNum;
  END SetCurrentSegment;


PROCEDURE Seginfo(self : Buffer) : Segment.T =
BEGIN
  RETURN self.segInfo;
END Seginfo;



(* writes dirty blocks from segbuffer to disk, records segDescriptor stuff *)
PROCEDURE ForceFlush (self : Buffer) =
VAR
  buffer   : FastBuffer.T;
  diskloc  : Segment.SegSummaryEntry;
  startoff : CARDINAL;
  
BEGIN

  IO.Put("Whos the fucker who called ForceFlush!! *******************\n");

  (* such bullshit! *)
  buffer := FastBuffer.Allocate(self.segInfo.bytesInBlock);

  LOCK self.mu DO

    (* initialize these values to be the 'invalid' value *)
    diskloc.iNode   := 65535;
    diskloc.offset  := 65535;
    diskloc.flag    := DATA;
    
    (* initialize the block pattern to be full of 'invalids' *)
    FOR i := 0 TO self.segInfo.bytesInBlock - 1 DO
      buffer.data[i] := VAL(255,CHAR);
    END;

    (* now write out these blocks to the buffer enough times to cause a flush *)
    startoff := self.getOffset();

    FOR i := startoff TO (self.bufflength DIV self.segInfo.bytesInBlock) - 1 DO
      TRY
        EVAL self.write(buffer.data,diskloc,self.segInfo.bytesInBlock,0,FALSE);
      EXCEPT
        Error.E, Segment.NoFreeSegs => 
          IO.PutError("Tried to write a block to offset " & Fmt.Int(self.getOffset()) &
            " and failed.\n");
      END;
    END;

  END; (* end lock *)

  FastBuffer.Deallocate(buffer);

END ForceFlush;

BEGIN
END SegBuffer.


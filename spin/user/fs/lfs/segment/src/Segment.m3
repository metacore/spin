(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Cleaned up.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Changed Read() I/F to improve performace.  Read takes VAR ARRAY 
 *	OF CHAR now. not REF ARRAY OF CHAR.
 *
 * 15-Aug-96  Eric Christoffersen (ericc) at the University of Washington
 *	Added dirty list.  Rewrote freelist.  Fixed write so it listened to the from and 
 *      numbytes parameters. Removed summary manipulation stuff as its now part of
 *      SegBuffer.
 *
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)
MODULE Segment;

IMPORT Error;
IMPORT SegmentInterface;
IMPORT Auth;
IMPORT IO,Fmt;
IMPORT IntQueueSeq;

CONST DEBUG = FALSE;
      INITIALDIRTYQUEUESIZE = 50;

(*********************    TYPES     *******************)
TYPE

  REVEAL FreeList = FlT BRANDED 
  OBJECT
    freesegs : REF ARRAY OF CHAR;
    dirtyQueue: IntQueueSeq.T;
    cleanedQueue: IntQueueSeq.T;
  OVERRIDES
    init             := FLInit;
    flush            := FLFlush;
    getFreeList      := GetFreeList;

    (* for use by segBuff *)
    getFreeSegAndRemove:= GetFreeSegAndRemove;
	(* declares next free segment as dirty *)

    (* for use by cleaner *)
    getNextDirtySeg  := GetNextDirtySeg;
	(* returns number of next dirty seg *)
    declareNextDirtyCleaned:= DeclareNextDirtyCleaned;
    moveNextDirtyToBack:= MoveNextDirtyToBack;

    (* for use by sync *)
    flushCleanedSegs := FlushCleanedSegs;
	(* makes all cleaned segs free, empties cleaned list *)

  END;

  REVEAL T = Public BRANDED
  OBJECT
  OVERRIDES
    write      := Write;
    read       := Read;
  END;


(*******************    PROCEDURES    *****************)

PROCEDURE FLInit (self : FreeList; READONLY buffer : REF ARRAY OF CHAR) : FreeList =
BEGIN

  self.freesegs := NEW(REF ARRAY OF CHAR, NUMBER(buffer^));

  self.dirtyQueue:=NEW(IntQueueSeq.T);
  self.dirtyQueue:=self.dirtyQueue.init(INITIALDIRTYQUEUESIZE);

  self.cleanedQueue:=NEW(IntQueueSeq.T);
  self.cleanedQueue:=self.cleanedQueue.init(INITIALDIRTYQUEUESIZE);

  FOR i := 0 TO NUMBER(buffer^) - 1 DO
    self.freesegs[i] := buffer[i];

    (* the free list should never contain clean segments, when on disk, this
       is just in case... *)
    IF self.freesegs[i] = CLEAN THEN
      self.freesegs[i] := DIRTY;
      self.dirtyQueue.addhi(i);
    ELSIF self.freesegs[i] = DIRTY THEN
      self.dirtyQueue.addhi(i);
    END;

  END;

  RETURN self;

END FLInit;

(* puts freelist into buffer *)
PROCEDURE GetFreeList(self:FreeList; VAR buffer : REF ARRAY OF CHAR) =
BEGIN
  buffer := NEW(REF ARRAY OF CHAR,NUMBER(self.freesegs^));

  buffer^ := self.freesegs^;
END GetFreeList;

(* declares cleaned segments free, then flushes free list array *)
PROCEDURE FLFlush (self : FreeList; VAR buffer : REF ARRAY OF CHAR) =
BEGIN
  FlushCleanedSegs(self);

  self.getFreeList(buffer);
END FLFlush;

(* toggles 'next' free segment in array to dirty, adds it to dirtyQueue
   and returns segment number *)
PROCEDURE GetFreeSegAndRemove(self : FreeList) : CARDINAL RAISES {NoFreeSegs} = 
VAR
  i : CARDINAL;
BEGIN

  i := 0;
  (* Search for a free segment *)
  (* XXX.  Slow! *)
  WHILE (i < NUMBER(self.freesegs^)) AND (self.freesegs[i] # FREE) DO
    i := i + 1;
  END;

  IF i < NUMBER(self.freesegs^) THEN
    self.freesegs[i] := DIRTY;
    self.dirtyQueue.addhi(i);
  ELSE
    RAISE NoFreeSegs;
  END;

  RETURN i;

END GetFreeSegAndRemove;

(* returns number of first dirty element in queue *)
(* doesn't declare element clean          *)
PROCEDURE GetNextDirtySeg(self:FreeList): CARDINAL RAISES {NoDirtySegs} =
  VAR i : INTEGER;
BEGIN

  IF(self.dirtyQueue.size()<1) THEN
    RAISE NoDirtySegs;
  END;

  i:= self.dirtyQueue.getlo();

  (* for safety's sake *)
  IF self.freesegs[i] # DIRTY THEN
    IO.PutError("GetNextDirtySeg: item on dirty list wasn't dirty.\n");
    self.freesegs[i] := DIRTY;
  END;

  RETURN i;
END GetNextDirtySeg;

(* Removes next dirty from dirty list, puts it on cleaned list.
   Doesn't declare segment free though *)
PROCEDURE DeclareNextDirtyCleaned(self:FreeList): CARDINAL RAISES {NoDirtySegs} =
  VAR i : INTEGER;
BEGIN

  IF(self.dirtyQueue.size()<1) THEN
    RAISE NoDirtySegs;
  END;

  i := self.dirtyQueue.remlo();

  self.cleanedQueue.addhi(i);

  (* for safety's sake *)
  IF self.freesegs[i] # DIRTY THEN
    IO.PutError("DeclareNextDirtyCleaned: item on dirty list wasn't dirty.\n");
  END;

  (* the segment is considered CLEAN (not FREE) until synced.  *)
  self.freesegs[i] := CLEAN;
  RETURN i;
END DeclareNextDirtyCleaned;

PROCEDURE MoveNextDirtyToBack(self:FreeList) RAISES {NoDirtySegs} =
  BEGIN
    IF(self.dirtyQueue.size()<1) THEN
      RAISE NoDirtySegs;
    END;

    self.dirtyQueue.addhi(self.dirtyQueue.remlo());
  END MoveNextDirtyToBack;


(* for use by sync, removes all elements from cleaned queue,
 * declares those elements free in freelist *)
PROCEDURE FlushCleanedSegs(self:FreeList) =
  BEGIN
    
    WHILE self.cleanedQueue.size()>0 DO
      self.freesegs[self.cleanedQueue.remhi()]:= FREE;
    END;

  END FlushCleanedSegs;

PROCEDURE Write( self           : T;
                 location      : DiskAddress;
                 data          : REF ARRAY OF CHAR;
                 from          : CARDINAL;
             VAR numBytes      : CARDINAL)
	RAISES {BadnumBytesValue,OffsetOutOfBounds,SegmentOutOfBounds,Error.E} =
VAR 
  pos : CARDINAL;
BEGIN

  IF location.block >= self.blocksInSegment THEN
    RAISE OffsetOutOfBounds;
  ELSIF location.segment>= self.segmentsOnDisk THEN
    RAISE SegmentOutOfBounds;
  ELSIF numBytes > self.blocksInSegment * self.bytesInBlock THEN
    RAISE BadnumBytesValue;
  ELSE
    pos := self.firstSegmentOffset * self.bytesInBlock +
	   (self.blocksInSegment * location.segment * self.bytesInBlock) +
	   (location.block * self.bytesInBlock);

    IF DEBUG THEN
      WITH
	numBlocks = numBytes DIV self.bytesInBlock,
	rest = numBytes MOD self.bytesInBlock 
        DO
          IO.Put("segment.write: " & Fmt.Int(numBytes) &
	         " bytes (" & Fmt.Int(numBlocks) &
		 " blocks+ " & Fmt.Int(rest) &
		 "), s: " & Fmt.Int(location.segment) &
	         ", b: " & Fmt.Int(location.block) &
		 ", at: " & Fmt.Int(pos) &
		 ", from: " & Fmt.Int(from) & ".\n");
      END;
    END;

    numBytes:=self.fsextent.write(SUBARRAY(data^,from,numBytes), pos);
  END;
END Write;


(* reads a block from disk from requested location *)
PROCEDURE Read(self          : T;
               location      : DiskAddress;
               VAR data      : ARRAY OF CHAR;
	       <* UNUSED *> to : CARDINAL;
           VAR numBytes      : CARDINAL) RAISES {BadnumBytesValue,OffsetOutOfBounds,SegmentOutOfBounds,Error.E}=
VAR
  pos : CARDINAL;
BEGIN

  IF location.block >= self.blocksInSegment THEN
    RAISE OffsetOutOfBounds;
  ELSIF location.segment >= self.segmentsOnDisk THEN
    RAISE SegmentOutOfBounds;
  ELSIF numBytes > self.blocksInSegment * self.bytesInBlock THEN
    RAISE BadnumBytesValue;
  ELSIF numBytes # NUMBER(data) THEN
    IO.Put("Warning: NUMBER(data) # numBytes at segment.read() \n");
  ELSE
    pos := self.firstSegmentOffset * self.bytesInBlock +
	   (self.blocksInSegment * location.segment * self.bytesInBlock) +
	   (location.block * self.bytesInBlock);

    IF DEBUG THEN 
      WITH
	bytes = NUMBER(data),
	numBlocks = bytes DIV self.bytesInBlock,
	rest = bytes MOD self.bytesInBlock 
	DO
          IO.Put("segment.read: " & Fmt.Int(bytes) &
	         " bytes, (" & Fmt.Int(numBlocks) &
		 " blocks + " & Fmt.Int(rest) &
	         "), s: " & Fmt.Int(location.segment) &
	         ", b: " & Fmt.Int(location.block) & ", at " &
	         Fmt.Int(pos) & "\n");
      END;
    END;
    numBytes:=self.fsextent.read(data, pos);
  END;
END Read;


BEGIN

  EVAL SegmentInterface.Export(NEW(Auth.AuthAlways));
END Segment.


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
 *	Changed Read() I/F to use VAR ARRAY OF CHAR, not REF ARRAY OF CHAR.
 *
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)
INTERFACE SegBuffer;

IMPORT Segment;
IMPORT Error;

FROM Segment IMPORT DiskAddress;

EXCEPTION
  BlockOutOfBounds;
  DataTooLarge;

TYPE
  Block = REF ARRAY OF CHAR;

  Buffer <: Public;

  Public = OBJECT
  METHODS
    (* init: initializes the segment buffer
       length is the number of blocks in the buffer
       blockSize is the number of bytes in a block
       fList is needed so that a free segment can be found
       to begin writing to *)
    
    init (segInfo : Segment.T; writeLoc : Segment.DiskAddress) : Buffer
    	  RAISES {Segment.NoFreeSegs};
    
    (* write: the Block of data, data to the segment
       buffer and returns the location on disk of the block
       written.  iNum is the inode number containing the data
       block, iOffset is the block offset in that inode of data,
       and segInfo is an LFS object containing information about
       the particular mount instance of the filesystem.
       lockit says whether we want to lock the buffer.  Normal clients should*)
    
    write (data     : Block;
           location : Segment.SegSummaryEntry;
           length   : CARDINAL;  (* how much to write from buffer *)
           from     : CARDINAL:= 0;  (* from where in buffer to write *)
           lockit   : BOOLEAN := TRUE) : DiskAddress
	   RAISES {Error.E,Segment.NoFreeSegs};
    

    (* a faster version of write, does multi blocks at once *)
    refWrite (READONLY data     : ARRAY OF CHAR;      (* data to write *)
              READONLY iNum     : CARDINAL;           (* inum of writer *)
              READONLY offset   : CARDINAL;
		    (* offset in file where first block goes, in blocks *)
              READONLY from     : CARDINAL;
		    (* block num in array to start copying *)
              VAR   bytesWritten: CARDINAL;
		    (* returns number of bytes actually written to segBuffer *)
              lockit   : BOOLEAN := TRUE) : DiskAddress 
                RAISES {Error.E,Segment.NoFreeSegs};

    (* read: copies the block residing at offset CARDINAL
        to data starting at data[to].  read returns the segment pointed
        at when the block was copied *)
    read (VAR data : ARRAY OF CHAR; diskloc: DiskAddress; to : CARDINAL := 0) : CARDINAL RAISES {BlockOutOfBounds};


    (* returns current epoch of segBuffer *)
    getEpoch():INTEGER;


    (* only roll forward needs this method, could be in private interface.*)
    setOffset (newOffset:CARDINAL);

    (* getOffset : returns the current block offset of the next block
       to be written *)    
    getOffset() : CARDINAL;


    (* getCurrentSegment : returns the current segment being buffered *)
    getCurrentSegment() : CARDINAL;

    (* used by roll forward, private interface a good idea *)
    setCurrentSegment(newSegNum:CARDINAL);

    (* returns a pointer to the segment*)
    seginfo() : Segment.T;

    (* instruct the segBuffer to write its dirty data to disk *)
    flushToDisk(): Segment.DiskAddress  RAISES {Error.E,Segment.NoFreeSegs};


    (* FUCKED, dont use *)
    (* forceFlush() fills the remaining portion of the buffer with illegal Disk values
       and flushes to disk *)
    forceFlush();

  END;


PROCEDURE GetSegmentSummary(segment:Segment.T;
                            READONLY segmentNumber:CARDINAL;
                            entries: REF ARRAY OF CHAR;
                            VAR locNextSegDescA:DiskAddress;
                            VAR epoch:INTEGER): CARDINAL;
    
END SegBuffer.



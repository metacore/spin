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
 *      Changed Read() I/F to use VAR ARRAY OF CHAR, not REF ARRAY OF CHAR.
 *
 * 17-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	New three-stack freelist
 *
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)
INTERFACE Segment;

IMPORT Extent,Error;

EXCEPTION
  NoFreeSegs;
  NoDirtySegs;
  SegmentOutOfBounds;
  SegmentDescriptorCorrupt;
  OffsetOutOfBounds;
  BadnumBytesValue;

CONST
  (* values for the On-Disk free list.  Roll-Forward needs access to them *)
  DIRTY = VAL(0,CHAR);
  CLEAN = VAL(2,CHAR);
  FREE  = VAL(1,CHAR);        (* makefs sets the freelist to all ones. *)

TYPE

  FreeList <: FlT;

  FlT = OBJECT
  METHODS
    init  (READONLY buffer: REF ARRAY OF CHAR) : FreeList;
    flush (VAR buffer: REF ARRAY OF CHAR);
    getFreeList(VAR buffer:REF ARRAY OF CHAR);
    getFreeSegAndRemove()    : CARDINAL  RAISES {NoFreeSegs};
    getNextDirtySeg()        : CARDINAL  RAISES {NoDirtySegs};
    declareNextDirtyCleaned(): CARDINAL  RAISES {NoDirtySegs};
    moveNextDirtyToBack()                RAISES {NoDirtySegs};
    flushCleanedSegs();
    freeSegCount() : CARDINAL		(* XXX need to fstest *)
  END;

  ShrtCard = BITS 16 FOR [0..65535];
  (* this MUST be the same as the ShortCard defined in
   * lfscore/src/LFSTypes.i3
   *)

  DiskAddress = RECORD
    segment : ShrtCard;
    block   : ShrtCard;
  END;

  SegSummaryEntry = RECORD
    iNode  : ShrtCard;
    offset : ShrtCard;
    flag   : ShrtCard;
  END;

  T <: Public;

  Public = OBJECT
    segFreeList        : FreeList;
    segDirtyList       : FreeList;
    bytesInBlock       : CARDINAL;
    blocksInSegment    : CARDINAL;
    segmentsOnDisk     : CARDINAL;
    firstSegmentOffset : CARDINAL;
    fsextent           : Extent.T;
    blocksForSummary   : CARDINAL;
  METHODS
    write (location      : DiskAddress;
           data          : REF ARRAY OF CHAR;
           from          : CARDINAL;
       VAR numBytes      : CARDINAL)
       RAISES {BadnumBytesValue,OffsetOutOfBounds,SegmentOutOfBounds,Error.E};

    read(location      : DiskAddress;
         VAR data          : ARRAY OF CHAR;
         to            : CARDINAL;
     VAR numBytes      : CARDINAL)
     RAISES {BadnumBytesValue,OffsetOutOfBounds,SegmentOutOfBounds,Error.E};

    getSummary(segmentNumber : CARDINAL;
               entries       : REF ARRAY OF SegSummaryEntry)
               RAISES {SegmentOutOfBounds,SegmentDescriptorCorrupt};

    setSummary(segmentNumber : CARDINAL;
               entries       : REF ARRAY OF SegSummaryEntry)
               RAISES {SegmentOutOfBounds};
  END;

END Segment.


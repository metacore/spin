(*
  Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.  Contains LFS SuperBlock structure.
 *
 *)
MODULE LFSSuperBlock;
IMPORT LFSRep;   
IMPORT LFSTypes, IMap;
IMPORT Error, IO, Fmt;

CONST
  DEBUG = FALSE;

PROCEDURE RoundUp (bytes, blocksize: CARDINAL): CARDINAL =
  BEGIN
   RETURN (bytes + blocksize - 1) DIV blocksize;
  END RoundUp;

(* Calcurate how many segments can be allocated given the total
 * number of blocks, size of each block.
 *  totalBlocks * bytesInBlock - SBHeader - IMAP
 *		= numSegs * (1 + bytesInBlock*blocksInSegment)                
 *)
PROCEDURE GetNumberOfSegments(
    totalBlocks, bytesInBlock, blocksInSegment: CARDINAL): CARDINAL =
  VAR
    total, numSegs : CARDINAL;
    fixed := SIZEOFFIXEDSUPERBLOCK;
  BEGIN

    (* see if the device is large enough to hold LFS *) 
    (* There should be at least two segments for the cleaner *)
    total := RoundUp(fixed + 2, bytesInBlock) +	(* superblock in blocks *)
	     2 * blocksInSegment; 		(* segments in blocks *)

    IF totalBlocks < total THEN
      IO.PutError("LFS needs " & Fmt.Int(total) & " blocks, but there is " &
		  Fmt.Int(totalBlocks) & "blocks\n");
      RETURN 0;
    END;

    WITH
      totalBytes = totalBlocks * bytesInBlock,
      divisor = 1 + bytesInBlock*blocksInSegment
     DO

      numSegs := (totalBytes - fixed) DIV divisor;

      (* make sure it fits *)
      WHILE TRUE DO
        total := RoundUp(fixed+numSegs, bytesInBlock) + numSegs*blocksInSegment;
        IF total > totalBlocks THEN
          DEC(numSegs);
        ELSE
          EXIT;
        END;
      END;
    END;
    IF DEBUG THEN IO.Put("Number of segments: "&Fmt.Int(numSegs)&".\n"); END;
    RETURN numSegs;
  END GetNumberOfSegments;

(* Read entire superblock from extent. Note that this may contain
 * extra bytes at the end as this read in blocks *)
PROCEDURE ReadSB(mp: LFSRep.MP; VAR buf: REF ARRAY OF CHAR) =
  VAR
    size := mp.segInfo.firstSegmentOffset*mp.segInfo.bytesInBlock;
  BEGIN
    buf := NEW(REF ARRAY OF CHAR, size);
    TRY
      size := mp.segInfo.fsextent.read(buf^, 0);
    EXCEPT
    | Error.E(e) =>
      IO.PutError("ReadSB: " & e.message());
      size := 0;
      buf := NIL;
    END;
    IF size # mp.segInfo.firstSegmentOffset*mp.segInfo.bytesInBlock THEN
      IO.PutError("Can't read superblock\n");
      buf := NIL;
    END;

  END ReadSB;

PROCEDURE WriteSB(mp: LFSRep.MP; READONLY buf: REF ARRAY OF CHAR) =
  VAR
    size : CARDINAL;
  BEGIN
    TRY
      size := mp.segInfo.fsextent.write(buf^, 0);
    EXCEPT
    | Error.E(e) =>
      IO.PutError("ReadSB: " & e.message());
    END;
  END WriteSB;

(* Read header part of superblock from extent *)
PROCEDURE ReadSBHeader(mp: LFSRep.MP; VAR header: LFSSBHeader) =
  VAR
    size : CARDINAL;
    buf : ARRAY [0..BYTESIZE(LFSSBHeader)-1] OF CHAR;
  BEGIN
    TRY
      size := mp.segInfo.fsextent.read(buf, 0);
    EXCEPT
    | Error.E(e) =>
      IO.PutError("ReadSuperBlockHeader: " & e.message());
      size := 0;
    END;
    IF size # BYTESIZE(LFSSBHeader) THEN
      IO.PutError("ReadSuperBlockHeader:read " & Fmt.Int(size) & "bytes\n:");
    END;
    
    header := VIEW(buf, LFSSBHeader);

  END ReadSBHeader;

(* Read FreeList part of superblock from extent. *)
PROCEDURE ReadSBFreeList(mp: LFSRep.MP; VAR buf: REF ARRAY OF CHAR) =
  VAR
    size := mp.segInfo.segmentsOnDisk;
    retBuf : REF ARRAY OF CHAR;
  BEGIN
    (* read entire superblock *)
    ReadSB(mp, buf);

    IF buf = NIL OR NUMBER(buf^) < size THEN
      IO.PutError("Can't read FreeList from extent\n");
    END;

    retBuf := NEW(REF ARRAY OF CHAR, size);
    retBuf^ := SUBARRAY(buf^, BYTESIZE(LFSSBHeader), size);
    buf := retBuf;

  END ReadSBFreeList;

(* Read IMap part of superblock from extent. *)
PROCEDURE ReadSBIMap(mp: LFSRep.MP; VAR buf: REF ARRAY OF CHAR) =
  VAR
    size := IMap.IMAPENTRYSIZE*LFSTypes.MAXINODES;
    offset := BYTESIZE(LFSSBHeader) + mp.segInfo.segmentsOnDisk;
    retBuf : REF ARRAY OF CHAR;
  BEGIN
    (* read entire superblock *)
    ReadSB(mp, buf);

    IF buf = NIL OR NUMBER(buf^) < size THEN
      IO.PutError("Can't read IMap from extent\n");
    END;

    retBuf := NEW(REF ARRAY OF CHAR, size);
    retBuf^ := SUBARRAY(buf^, offset, size);
    buf := retBuf;

  END ReadSBIMap;

(* check superblock header.  add more checks if necessary *)
PROCEDURE CheckSBHeader(READONLY header: LFSSBHeader;
				READONLY mp: LFSRep.MP):BOOLEAN =
  PROCEDURE ErrorMsg(msg: TEXT) =
    BEGIN
      IO.PutError(msg);
      IO.PutError("     superblock is corrupted and/or not formatted.\n");
      IO.PutError("     Perhaps, you need to run makefs.\n");
    END ErrorMsg;

  VAR
    numSegs : CARDINAL;
    totalBlocks := mp.blocksInExtent;
    bytesInBlock := mp.segInfo.bytesInBlock;
    blocksInSegment := header.blocksInSegment;
  BEGIN

    (* see if header.size is corrupted *)
    IF totalBlocks < header.size THEN
      ErrorMsg("LFS: size of superblock should not exceed extent's size\n");
      RETURN TRUE;
    END;
  
    (* see if header.blocksInSegment is corrupted *)
    IF blocksInSegment <= 0 THEN
      ErrorMsg("LFS: number of blocks in a segment should be non-zero.\n");
      RETURN TRUE;
    END;
  
    (* see if there are enough segments given num of blocks in segment *)
    numSegs := GetNumberOfSegments(totalBlocks, bytesInBlock, blocksInSegment);
    IF numSegs = 0 THEN
      RETURN TRUE;
    END;
  
    (* more test for the size *) 
    IF RoundUp(SIZEOFFIXEDSUPERBLOCK+numSegs, bytesInBlock) # header.size THEN
      ErrorMsg("LFS: SuperBlock size mismatch. " & " recalcurated " &
	Fmt.Int(RoundUp(SIZEOFFIXEDSUPERBLOCK+numSegs, bytesInBlock)) &
  	" got " & Fmt.Int(header.size) & " from disk\n");
      RETURN TRUE;
    END; 
  
    IF header.writeLoc.segment > numSegs THEN
      ErrorMsg("LFS: writeloc (segment) should not exceed segments.\n");
      RETURN TRUE;
    END;
  
    IF header.writeLoc.block > header.blocksInSegment THEN
      ErrorMsg("LFS: writeloc (block) should not exceed blocks in a seg.\n");
      RETURN TRUE;
    END;
  
    RETURN FALSE;			(* no error detected *)
  END CheckSBHeader;
  
BEGIN
END LFSSuperBlock.

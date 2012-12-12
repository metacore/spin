(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Cleaned up.
 *
 * 12-Sep-96  Eric Christoffersen (ericc) at the University of Washington
 *	Added format option and ability.  Fixed some formatting stuff
 *      but suspect more errors.
 *
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)

(* Program for testing basic functions of CleanerIMap *)
MODULE MakeFs;

IMPORT Error, ParseParams;
IMPORT MakeFsCmd;
IMPORT IO, Fmt, Text;
IMPORT Segment;
IMPORT LFSRep, LFSSuperBlock, DiskExtents;
IMPORT LFSTypes, IMap;

PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
  VAR
    thedev            : TEXT;
    blocksInSegment   : CARDINAL;
    format            : TEXT;
  BEGIN

    pp.reset();
    TRY
      pp.skipNext();		(* skip makefs *)
      IF pp.testNext("-zap") THEN
        MakeFsCmd.Uninstall();
	RETURN TRUE;
      ELSE 
        thedev  := pp.getNext();
        blocksInSegment := pp.getNextInt();
      END;
    EXCEPT
    | ParseParams.Error => IO.Put(CommandHelp & "\n");
      RETURN FALSE;
    END;

    TRY
      format  := pp.getNext();
    EXCEPT
    | ParseParams.Error =>
      format := "NoFormat";
    END;

    MakeLFS(thedev, blocksInSegment, Text.Equal(format, "Format"));

    RETURN TRUE;
  END Run;

(* Format a disk or an exent for the LFS filesystem. *)
PROCEDURE MakeLFS(thedev: TEXT; blocksInSegment: CARDINAL; format: BOOLEAN) =
  VAR
    mp                : LFSRep.MP;
    numbytes          : CARDINAL;
    numsegs           : CARDINAL;
    buffer            : REF ARRAY OF CHAR;
    firstsegoffbytes  : CARDINAL;
    firstsegoffblocks : CARDINAL;
    segByteSize,initialSegOffset:CARDINAL;
    unusedBlocks      :CARDINAL;
    header	      : LFSSuperBlock.LFSSBHeader;
    iMapEntry         : IMap.IMapEntry;
  BEGIN
    IO.Put("Creating mountable lfs on " & thedev);
    IF format THEN IO.Put(" w/superblock only.\n");
    	      ELSE IO.Put(" w/superblock and all data.\n"); END;

    mp := NEW(LFSRep.MP);
    mp.segInfo := NEW(Segment.T);
    
    (* SET up extent on disk *)
    TRY
      DiskExtents.SetupDiskExtent(mp,thedev);
    EXCEPT
    | DiskExtents.DiskNotFound =>
      IO.PutError(thedev & " not found.\n");
      RETURN;
    END;
    
    (* calcurate number of segments in the device *)
    numsegs := LFSSuperBlock.GetNumberOfSegments(mp.blocksInExtent,
						 mp.segInfo.bytesInBlock,
						 blocksInSegment);

    (* the number of bytes for the buffer is the bytes needed to store the
       first segment offset, the freelist, and the imap and the writeLoc of
       the segBuffer *)
    (*calculate offset of first segment in bytes*)
    firstsegoffbytes := LFSSuperBlock.SIZEOFFIXEDSUPERBLOCK +
			numsegs;	(* bytes for Freelist *)
    
    firstsegoffblocks := LFSSuperBlock.RoundUp(firstsegoffbytes,
					       mp.segInfo.bytesInBlock);
    unusedBlocks := mp.blocksInExtent - 
    		    numsegs * blocksInSegment - firstsegoffblocks;
    buffer := NEW(REF ARRAY OF CHAR, firstsegoffbytes);

    (*now set up the char buffer and write the first segment offset
      to disk and write the number of blocks in a segment to disk*)
    header.size := firstsegoffblocks;
    header.blocksInSegment := blocksInSegment;
    header.writeLoc := Segment.DiskAddress{0, 1}; 

    IO.Put("  SuperBlock Info \n");
    IO.Put("    first segment at " & Fmt.Int(firstsegoffblocks) & " blocks\n");
    IO.Put("    " & Fmt.Int(mp.segInfo.bytesInBlock) & " bytes/block\n");
    IO.Put("    " & Fmt.Int(blocksInSegment) & " blocks/segment\n");
    IO.Put("    " & Fmt.Int(numsegs) & " segments/device\n");
    IO.Put("    " & Fmt.Int(unusedBlocks) & " blocks unused\n");
    IO.Put("    " & Fmt.Int(mp.blocksInExtent) & " blocks/device\n");

    (* set superblock header *)
    VIEW(SUBARRAY(buffer^, 0, BYTESIZE(LFSSuperBlock.LFSSBHeader)),
    	 LFSSuperBlock.LFSSBHeader) := header; 

    (* init freelist area *)
    (* now treat the buffer as one block of info and write it to
       disk as many times as necessary to represent the number
       of bytes needed to store the freelist info *)
    FOR i := BYTESIZE(LFSSuperBlock.LFSSBHeader) TO
	BYTESIZE(LFSSuperBlock.LFSSBHeader)+numsegs-1 DO
      buffer[i] := VAL(1,CHAR);
    END;
    
    (* init imap area *)
    (* now write out the block for the imap.  The pattern will be a 6 byte
       sequence of 255,255,255,255,0,0.  So I take the
       bytesInBlock - bytesInBlock MOD 6 and create a pattern to write out
       to disk and repeat that pattern until enough bytes are written to
       represent an empty imap*)
    iMapEntry := IMap.IMapEntry{Segment.DiskAddress{LFSTypes.ILLEGALDISKVALUE,
					            LFSTypes.ILLEGALDISKVALUE},
				0};
    FOR i := BYTESIZE(LFSSuperBlock.LFSSBHeader)+numsegs TO
	(firstsegoffbytes - (BYTESIZE(IMap.IMapEntry)+1)) BY 
	BYTESIZE(IMap.IMapEntry) DO
      SUBARRAY(buffer^, i, BYTESIZE(IMap.IMapEntry)) :=
	VIEW(iMapEntry, ARRAY [0..BYTESIZE(IMap.IMapEntry)-1] OF CHAR);
    END;
    
    (* now write out the pattern *)
    TRY
      numbytes := mp.segInfo.fsextent.write(buffer^,0);
    EXCEPT
    | Error.E(e) =>
      IO.PutError("can't write superblock to extent " & e.message() & "\n");
      IO.PutError("the device is not formatted\n");
    END;
    
    segByteSize := mp.segInfo.bytesInBlock*blocksInSegment;
    initialSegOffset:= firstsegoffblocks * mp.segInfo.bytesInBlock;

    IF format THEN

      (* make a buffer big enough to hold a whole segment *)
      buffer := NEW(REF ARRAY OF CHAR,segByteSize);

      (* fill up buffer with shit *)
      IO.Put("Populating buffer.\n");
      FOR i := 0 TO segByteSize-1 DO
        buffer[i]:=VAL(255,CHAR);
      END;

      IO.Put("Segment initialization beginning, initializing "&Fmt.Int(numsegs)&".\n");
      FOR i := 0 TO numsegs - 1 DO
	TRY
          EVAL mp.segInfo.fsextent.write(buffer^,
			initialSegOffset + i * segByteSize);
	EXCEPT
	| Error.E(e) =>
	  IO.PutError("can't initialize extent" & e.message() & "\n");
	END;

        IO.Put(Fmt.Int(i)&".");
      END;
    
      IO.Put("\nSegment initialization complete.\n");
    
    END;

  END MakeLFS;

 BEGIN
 END MakeFs.

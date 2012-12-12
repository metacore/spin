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
INTERFACE LFSSuperBlock;
IMPORT LFSRep;
IMPORT LFSTypes, IMap;
IMPORT Segment;   

(* LFS's superblock consists of three parts.
   * 1) LFSSBHHeader - offset to first data block (in blocks)
                     - number of blocks in a segment
                     - segment buffer write location
   2) FreeList       - variable size = number of segments on the device.
   3) IMap           - fixed size = IMap.IMAPENTRYSIZE * MAXINODES
 *)

TYPE LFSSBHeader = RECORD
  size : LFSTypes.ShortCard;	(* size of superblock in blocks.
				   ie; offset to the first segment. *)
  blocksInSegment : LFSTypes.ShortCard;	(* number of blocks in a segment. *)
  writeLoc : Segment.DiskAddress;	(* segment buffer write location. *)
END;

CONST
  SIZEOFFIXEDSUPERBLOCK = BYTESIZE(LFSSBHeader) +	(* size of header *)
	IMap.IMAPENTRYSIZE*LFSTypes.MAXINODES;		(* size of IMap *)

PROCEDURE RoundUp (bytes, blocksize: CARDINAL): CARDINAL;

(* Calcurate how many segments can be allocated given the total
 * number of blocks, size of each block.
 *  totalBlocks * bytesInBlock - SBHeader - IMAP
 *		= numSegs * (1 + bytesInBlock*blocksInSegment)                
 *)
PROCEDURE GetNumberOfSegments(
    totalBlocks, bytesInBlock, blocksInSegment: CARDINAL): CARDINAL;

(* Read entire superblock from extent. Note that this may contain
 * extra bytes at the end as this read in blocks *)
PROCEDURE ReadSB(mp: LFSRep.MP; VAR buf: REF ARRAY OF CHAR);

(* Write entire superblock to extent. *)
PROCEDURE WriteSB(mp: LFSRep.MP; READONLY buf: REF ARRAY OF CHAR);

(* Read header part of superblock from extent *)
PROCEDURE ReadSBHeader(mp: LFSRep.MP; VAR header: LFSSBHeader);

(* Read FreeList part of superblock from extent. *)
PROCEDURE ReadSBFreeList(mp: LFSRep.MP; VAR buf: REF ARRAY OF CHAR);

(* Read IMap part of superblock from extent. *)
PROCEDURE ReadSBIMap(mp: LFSRep.MP; VAR buf: REF ARRAY OF CHAR);

(* check superblock header.  add more checks if necessary *)
PROCEDURE CheckSBHeader(READONLY header: LFSSBHeader;
				READONLY mp: LFSRep.MP):BOOLEAN;
END LFSSuperBlock.

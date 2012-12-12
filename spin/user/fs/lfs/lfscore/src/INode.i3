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
 *	Changed addDirEntry() I/F to eliminate a compiler warning.
 *
 * 30-May-96  Scott VanWoudenberg (scottv) at the University of Washington
 *	Created.  Inode stuff, no indirect blocks.
 *
*)

INTERFACE INode;
IMPORT IMap;
IMPORT Segment, INodeInfo, SegBuffer;
FROM Segment IMPORT DiskAddress;
IMPORT LFSTypes;
IMPORT Error;
IMPORT DirEnt;


CONST
  Brand = "INode";
  NDADDR = 12;
  NIADDR = 3;    (* number of levels of indirect blocks we'll support *)
  MAXBLOCKNUM = LAST(LFSTypes.ShortCard);

EXCEPTION
  InvalidBlockRequest;
  IllegalFileType;
  INodeInitError;
  NotDirectory;
  FileExists;
  FileNotFound;
  IllegalFileName;
  FileError;
  NonEmpty;    (* used when attempt to delete non-empty directory inode *)


TYPE
  TimeStamp = INTEGER;

  (*
   * this is the structure that gets serialized to disk.  I put
   * all this stuff into a RECORD to facilitate the use of VIEW
   * when I read the INode block from disk.
   *)
  IGuts = RECORD
    inum : LFSTypes.ShortCard;
    version : LFSTypes.ShortCard;
    linkCount : LFSTypes.ShortCard;
    owner : LFSTypes.ID;
    group : LFSTypes.ID;
    type  : LFSTypes.ID;
    size : INTEGER;
    created : TimeStamp;
    lastModified : TimeStamp;
    lastRead : TimeStamp;
    directBlocks : ARRAY [0..NDADDR-1] OF DiskAddress;
    indirectBlocks : ARRAY [0..NIADDR-1] OF DiskAddress;
  END;

  T <: Public;

  Public = BRANDED OBJECT
    lock : MUTEX;
  METHODS

    (* initialize an inode object with the serial inode data stored at
       diskAddress *)
    init(segBuffer: SegBuffer.Buffer;
	 imap : IMap.T;
	 from : DiskAddress) RAISES{INodeInitError};

    (* create an inode object from scratch instead of initializing it
       from data stored in the log.  *)
    create(segBuffer: SegBuffer.Buffer;
	   imap : IMap.T;
	   info : LFSTypes.INumInfo);

    (* write the inode information into the log via the segment buffer
       and update the memory resident IMap while I'm at it *)
    flush();

    (* get iNum of THIS iNode *)
    getINum(): LFSTypes.ShortCard;

    (* get version of THIS iNode *)
    getVersion() : LFSTypes.ShortCard;

    (* find and return the disk address of the block specified as
       a parameter *)
    getBlockLoc(block : LFSTypes.ShortCard)
	: DiskAddress RAISES{InvalidBlockRequest};

    (* update the INode meta data for the DATA blocks given in the blockList.
       the new DiskAddress for each block is in the INodeBlockInfo struct.  *)
    update(READONLY blockList : ARRAY OF INodeInfo.T;
	   size : LFSTypes.ShortCard) : BOOLEAN;
    updateMeta(READONLY blockList : ARRAY OF INodeInfo.T;
	       size : LFSTypes.ShortCard) : BOOLEAN;

    (* A function mainly for the cleaner.  Verifies that the subject
       is an actual data block being mapped by the INode.  *)
    verifyData(block : LFSTypes.INodeBlockInfo) : BOOLEAN;

    (* verifyData's evil twin.  Verifies that the subject is a meta
       data block being used by the INode.  *)
    verifyMeta(block : LFSTypes.INodeBlockInfo) : BOOLEAN;

    (* version of update for use by the cleaner, for moving iNode meta-data *)
    (*updateMeta(block: LFSTypes.INodeBlockInfo): BOOLEAN;*)

    (* returns the size of the file in bytes *)
    byteSize() : INTEGER;

    (*set the size of the file in bytes*)
    setbyteSize(size : INTEGER);
		
    (************************)
    (* Directory Type Stuff *)

    (* set inode type, must be one of LFSTypes.DIR or LFSTypes.FILE *)
    setFileType(type:LFSTypes.ID) RAISES{IllegalFileType};

    (* query about different file properties *)
    isDir():BOOLEAN;
    isFile():BOOLEAN;

    (* public function, uses internalfunc GetDirObj() to build a DirHandle *)
    getFileArray(pos:INTEGER;
		 VAR buf: ARRAY OF DirEnt.T):INTEGER RAISES{Error.E};

    (* returns number of files in the directory, if it is a directory *)
    dirSize():INTEGER RAISES{Error.E};

    (*  causes tuple of name and iNum to be hashed into directory object *)
    (* XXX to avoid: warning: large parameter passed by value (258 bytes) *)
    addDirEntry((*IN*) VAR dirEnt: LFSTypes.DirectoryData)
	RAISES{FileExists, IllegalFileName, NotDirectory,FileError};

    (*  removes tuple from dir object *)
    rmDirEntry(name:TEXT;iNum:LFSTypes.ShortCard)
	RAISES{FileNotFound, IllegalFileName, NotDirectory, NonEmpty};

    (*  returns iNum of a given file name, if found *)
    getFileINum(name:TEXT):LFSTypes.ShortCard
	RAISES {FileNotFound, IllegalFileName, NotDirectory};

    (* used for incrementing and decrementing link counts.  If link count
       reaches zero then iNode tells system to forget about itself. *)
    incLinkCount();
    decLinkCount();

  END;

END INode.

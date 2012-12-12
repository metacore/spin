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
 *	Improved performance by FastBuffer.
 *	Changed addDirEntry() I/F to eliminate a compiler warning.
 *
 * 21-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Added support for filesize up to 16396 blocks.
 *
 * 06-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Added support for directories, stat.
 *
 * 30-May-96  Scott VanWoudenberg (scottv) at the University of Washington
 *	Created.  INode stuff, no indirect blocks.
 *
 *)

MODULE INode EXPORTS INode, INodePrivate;

IMPORT Segment, SegBuffer, IMap, INodeInfo;
IMPORT Text, IO, Fmt;
IMPORT Word;
IMPORT Error;

IMPORT DirObject;
IMPORT DirEnt;
IMPORT FastBuffer;


IMPORT LFSTypes;
FROM LFSTypes IMPORT ILLEGALDISKVALUE;
FROM LFSTypes IMPORT RCharArray;
FROM LFSTypes IMPORT ShortCard;
FROM LFSTypes IMPORT INodeBlockInfo;
FROM LFSTypes IMPORT INumInfo;
FROM LFSTypes IMPORT ID;
FROM Segment IMPORT DiskAddress;

REVEAL T = Public BRANDED OBJECT
  mu             : MUTEX;
  segment        : Segment.T;
  segBuffer      : SegBuffer.Buffer;
  imap           : IMap.T;
  dirObject      : DirObject.T;
  iguts          : IGuts;
  currentDirectNum  : ShortCard;
  currentDirect     : REF ARRAY OF CHAR;
  currentIndirectNum: ShortCard;
  currentIndirect   : REF ARRAY OF CHAR;
  dirtyFlag      : Word.T;  (* this stores whether inode data has become invalidated and needs to be written to disk *)
OVERRIDES
  init           := Init;
  create         := Create;
  flush          := Flush;
  getINum        := GetINum;
  getVersion     := GetVersion;
  getBlockLoc    := GetBlockLoc;
  update         := Update;
  updateMeta     := UpdateMeta;
  verifyData     := VerifyData;
  verifyMeta     := VerifyMeta;
  byteSize       := ByteSize;
  setbyteSize    := SetByteSize;


  (* for setting file type *)
  setFileType    := SetFileType;
  isDir          := IsDir;
  isFile         := IsFile;


  (* directory stuff *)
  getFileArray   := GetFileArray;
  getFileINum    := GetFileINum;
  addDirEntry    := AddDirEntry;
  rmDirEntry     := RmDirEntry;
  incLinkCount   := IncLinkCount;
  decLinkCount   := DecLinkCount;
  dirSize        := DirSize;

END;

CONST DEBUG = FALSE;
(*
CONST DEBUG = TRUE;
*)
      (* prints out a character when inodes load and save their meta blocks  *)
      INODEDIRTY    = 1; (* if the on-inode direct or indirect blocks
			    change location, then disk inode is invalid *)
      DIRECTDIRTY   = 2; (* if current direct block is invalidated *)
      INDIRECTDIRTY = 4; (* if current indirect block is invalidated *)



(*****************************
  INODE METHODS
*****************************)

  (* returns our inum *)
PROCEDURE GetINum(self : T) : ShortCard = 
  BEGIN
    RETURN self.iguts.inum;
  END GetINum;


  (* initialize an inode object with the serial inode data stored at
     diskAddress
  *)
PROCEDURE Init(self : T; segBuffer : SegBuffer.Buffer; imap : IMap.T; from : DiskAddress)
  RAISES{INodeInitError} =

  VAR
    buf : FastBuffer.T;
    byteCount : CARDINAL;

  BEGIN

    IF DEBUG THEN
      IO.Put("INode init called with segment:"&Fmt.Int(from.segment)&
        " and block:"&Fmt.Int(from.block)&".\n");
    END;

    IF self.lock = NIL THEN self.lock := NEW(MUTEX); END;

    IF self.mu = NIL THEN self.mu := NEW(MUTEX); END;

    self.segment := segBuffer.seginfo();
    self.segBuffer := segBuffer;
    self.imap := imap;

    buf := FastBuffer.Allocate(self.segment.bytesInBlock);
    
    (* read the block we are stored in from the disk.
       NOTE: this does not work so nicely if the size of an INode
             on disk is larger than 1 block.
    *)

    byteCount := ReadBlock(self,from, buf.data);

    IF byteCount < BYTESIZE(IGuts) THEN
      RAISE INodeInitError;
    END;

    (* use VIEW to grab and structure the INode data into the
       RECORD format -- um, seen it done a million times...
    *)

    IF DEBUG = TRUE THEN IO.Put("Viewing buf into IGuts"); END;

    WITH guts = VIEW(SUBARRAY(buf.data^, 0, BYTESIZE(IGuts)), IGuts) DO
      self.iguts := guts;
    END;

    self.currentDirectNum  := ILLEGALDISKVALUE;
    self.currentDirect     := NIL;
    self.currentIndirectNum:= ILLEGALDISKVALUE;
    self.currentIndirect   := NIL;

    (* set initialize inode to clean (this fun inits from disk data) *)
    self.dirtyFlag := 0;

    FastBuffer.Deallocate(buf);

    IF DEBUG THEN IO.Put("--Completed.\n"); END;

  END Init;

  (* create an inode object from scratch instead of initializing it
     from data stored in the log.
  *)
PROCEDURE Create(self : T; segBuffer : SegBuffer.Buffer; imap : IMap.T; info : INumInfo) = 
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("inode.create: Creating INode " &Fmt.Int(info.iNum)&
        " from scratch.\n");
    END;

    IF self.lock = NIL THEN self.lock := NEW(MUTEX); END;

    IF self.mu = NIL THEN self.mu := NEW(MUTEX); END;

    self.segment := segBuffer.seginfo();
    self.segBuffer := segBuffer;
    self.imap := imap;
    self.iguts.inum := info.iNum;
    self.iguts.version := info.version;
    self.dirObject := NIL;

    FOR i := 0 TO NDADDR-1 DO
      self.iguts.directBlocks[i].segment := ILLEGALDISKVALUE;
      self.iguts.directBlocks[i].block := ILLEGALDISKVALUE;
    END;
    
    FOR i := 0 TO NIADDR-1 DO
      self.iguts.indirectBlocks[i].segment := ILLEGALDISKVALUE;
      self.iguts.indirectBlocks[i].block := ILLEGALDISKVALUE;
    END;
    
    (* - set creation timestamp
       - query userID and groupID from some protection-related object
       - 
    *)
    self.iguts.owner := 0;
    self.iguts.group := 0;
    self.iguts.size := 0;
    self.iguts.type := LFSTypes.FILE;  (* iNodes default to being files *)
    self.iguts.created := 0;
    self.iguts.lastModified := 0;
    self.iguts.lastRead := 0;

    self.currentDirectNum  := ILLEGALDISKVALUE;
    self.currentDirect     := NIL;
    self.currentIndirectNum:= ILLEGALDISKVALUE;
    self.currentIndirect   := NIL;

    (* inode block is initially dirty, needed for inode to be written
       out initially *)
    self.dirtyFlag := INODEDIRTY;

  END Create;


(* public flush, with lock *)
PROCEDURE Flush(self:T) =
  BEGIN
    FlushI(self);
  END Flush;


  (* write the inode information into the log via the segment buffer
     and update the memory resident IMap while I'm at it
     Note that this only flushes the first block of the iNode, the
     rest is always written to disk immediately.
  *)
PROCEDURE FlushI(self : T) =

  VAR
    block : FastBuffer.T;
    blockInfo : Segment.SegSummaryEntry;
    dAddr : DiskAddress;

  BEGIN
    IF DEBUG THEN IO.Put("inode.FlushI writing myself to disk .\n"); END;

    SyncINode(self);

    (* if the inode is dirty then write it out *)
    IF Word.And(self.dirtyFlag,INODEDIRTY) # 0 THEN 

      (* increment our version number before writing to disk *)
      INC(self.iguts.version, 1);
      
      block := FastBuffer.Allocate(self.segment.bytesInBlock);
      WITH buf = VIEW(self.iguts, ARRAY [0..BYTESIZE(self.iguts)-1] OF CHAR) DO
        SUBARRAY(block.data^, 0, BYTESIZE(self.iguts)) := buf;
      END;

      blockInfo.iNode   := self.iguts.inum;
      blockInfo.offset  := LFSTypes.INODEOFFSET;
      blockInfo.flag    := LFSTypes.META;

      TRY
        dAddr := self.segBuffer.write(block.data, blockInfo,BYTESIZE(self.iguts));
      EXCEPT
      | Error.E, Segment.NoFreeSegs=>
        IO.Put("Error, INode unable to write to segment buffer.");
      END;

      EVAL(self.imap.setLocation(self.iguts.inum, dAddr));

      IF DEBUG = TRUE THEN
        IO.Put("Inode "&Fmt.Int(self.iguts.inum)&" just flushed to disk, indirectblock[0] is seg:"&
          Fmt.Int(self.iguts.indirectBlocks[0].segment)&" and block:"&
          Fmt.Int(self.iguts.indirectBlocks[0].block)&".\n");
      END;

      self.dirtyFlag := Word.And(self.dirtyFlag,Word.Not(INODEDIRTY));
      FastBuffer.Deallocate(block);

    END;
  END FlushI;





  (* GetBlockLoc traverses the direct and indirect meta data and returns
     the disk address where block is located in the Extent
     If the block passed in by the caller is not valid (e.g., bigger
     than the size of the file), then an InvalidBlockRequest is RAISED.

     CLEANER - you will need to TRY this call because the block you are
     checking might not be valid in which case an exception is raised.  To
     you this means the same thing, the block is dead, but I need to make such
     a distiction for others.
  *)
PROCEDURE GetBlockLoc(self : T; block : ShortCard) : DiskAddress
  RAISES{InvalidBlockRequest} =
  BEGIN

    LOCK self.mu DO

      TRY
        RETURN GetBlockLocI(self,block)
      EXCEPT
      | InvalidBlockRequest =>
        RAISE InvalidBlockRequest;
      END;

    END;
    
  END GetBlockLoc;


PROCEDURE GetBlockLocI(self : T; block : ShortCard) : DiskAddress
  RAISES{InvalidBlockRequest} =
  VAR
    maxValidBlock := self.iguts.size DIV self.segment.bytesInBlock;
    retVal        :  DiskAddress;
    offset        :  INTEGER;
    entriesInBlock:  INTEGER;
    bytesInBlock  := self.segment.bytesInBlock;
    target        :  CARDINAL;

  BEGIN


    target := block;

    IF DEBUG = TRUE THEN
      IO.Put("inode.blockLoc: request for disk address of data block " &Fmt.Int(target)& 
        ".\n");
    END;


    (* check if requested block is larger than the largest valid block
       in the INode *)
    IF target > maxValidBlock THEN
      IO.Put("block number too large in INode.GetBlockLoc\n");
      RAISE InvalidBlockRequest;
    END;


    (* look in the direct blockmap first *)
    IF target < NDADDR THEN
      RETURN self.iguts.directBlocks[block];
    END;
    

    (* OTHERWISE the block's location is stored in the indirect blocks *)
    target:= target-NDADDR;

    entriesInBlock := bytesInBlock DIV BYTESIZE(DiskAddress);

    (* offset is the direct block number *)
    offset := target DIV entriesInBlock;


    LoadDirectBlock(self,offset);

    retVal := VIEW(SUBARRAY(self.currentDirect^, BYTESIZE(DiskAddress)*(target MOD entriesInBlock),BYTESIZE(DiskAddress)),DiskAddress);

    RETURN retVal;

  END GetBlockLocI;  





(* updates on-inode direct block buffer, writing back old
   buffer if needed *)
PROCEDURE LoadDirectBlock(self:T; directNum:CARDINAL) =
  VAR entriesInBlock, bytesInBlock:CARDINAL;
      blockInfo    : Segment.SegSummaryEntry;
      writeLocation:DiskAddress;
  BEGIN


    (* if the current direct block isn't the one we want *)
    IF self.currentDirectNum = directNum THEN
      RETURN;
    END;

    bytesInBlock := self.segment.bytesInBlock;
    entriesInBlock := bytesInBlock DIV BYTESIZE(DiskAddress);

    (* if there is an old direct block to write back to disk *)
    (*IF self.currentDirectNum # ILLEGALDISKVALUE THEN*)

    IF Word.And(self.dirtyFlag,DIRECTDIRTY) # 0 THEN

      blockInfo.iNode   := self.iguts.inum;
      blockInfo.offset  := self.currentDirectNum+LFSTypes.FIRSTDIRECTOFFSET;
      blockInfo.flag    := LFSTypes.META;

      TRY
        writeLocation   :=
	    self.segBuffer.write(self.currentDirect, blockInfo,bytesInBlock);
        self.dirtyFlag  := Word.And(self.dirtyFlag,Word.Not(DIRECTDIRTY));
      EXCEPT
      | Error.E, Segment.NoFreeSegs=>
        IO.PutError("unable to write direct block map to segment buffer.");
      END;

      IF self.currentDirectNum < NIADDR-1 THEN
        self.iguts.indirectBlocks[self.currentDirectNum] := writeLocation;
        self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);
      ELSE
        LoadIndirectBlock(self, NIADDR-1);
        SUBARRAY(self.currentIndirect^,
		 self.currentDirectNum*BYTESIZE(DiskAddress),
		 BYTESIZE(DiskAddress)) :=
            VIEW(writeLocation,ARRAY [0..BYTESIZE(DiskAddress)-1] OF CHAR);

        self.dirtyFlag := Word.Or(self.dirtyFlag,INDIRECTDIRTY);
      END;
    ELSE
      (* need to make the array to hold all future buffered direct blocks for
       * this instance of inode *)
      IF self.currentDirect = NIL THEN
	self.currentDirect := NEW(REF ARRAY OF CHAR,bytesInBlock);
      ELSE
	(* XXX. put a error check code.  Invalidate the contents of buf *)
      END;
    END;

    (* now load new direct block *)

    IF directNum<NIADDR-1 THEN
      (* Sanity check! Looks like ReadBlock was used regardless to
       * the buffer was newly created and data wasn't on disk
       *)
      writeLocation := self.iguts.indirectBlocks[directNum];
      IF writeLocation.segment = ILLEGALDISKVALUE OR
	 writeLocation.block = ILLEGALDISKVALUE THEN
	 (* IO.Put("LoadDirectBlock. looks like you got brand new block\n");*)
      ELSE
        EVAL ReadBlock(self, writeLocation, self.currentDirect);
      END;
    ELSE
      LoadIndirectBlock(self,NIADDR-1);
      (* same sanity check *)
      writeLocation := VIEW(SUBARRAY(self.currentIndirect^,
				     BYTESIZE(DiskAddress)*directNum,
				     BYTESIZE(DiskAddress)),
			    DiskAddress);
      IF writeLocation.segment = ILLEGALDISKVALUE OR
	 writeLocation.block = ILLEGALDISKVALUE THEN
	 (* IO.Put("LoadDirectBlock. looks like you got brand new block\n");*)
      ELSE
        EVAL ReadBlock(self, writeLocation, self.currentDirect);
      END;
    END;

    self.currentDirectNum := directNum;

  END LoadDirectBlock;

(* updates on-inode indirect block buffer, writing back old
   buffer if needed *)
PROCEDURE LoadIndirectBlock(self:T;indirectNum:CARDINAL) =
  VAR blockInfo : Segment.SegSummaryEntry;
      writeLocation:DiskAddress;
  BEGIN


    IF DEBUG = TRUE THEN
      IO.Put("LoadIndirectBlock called; indirectNum:"&Fmt.Int(indirectNum)&".\n");
    END;

    IF self.currentIndirectNum = indirectNum THEN
      RETURN;
    END;

    IF indirectNum < NIADDR-1 THEN
      RETURN;
    END;

    (* otherwise we write the old indirectNum to disk,
       load the new one into the buffer *)

    (* if there is a current indirect buffer, we must flush it *)

    IF Word.And(self.dirtyFlag,INDIRECTDIRTY) # 0 THEN

      (* write loaded indirect-block back to disk *)
      blockInfo.iNode   := self.iguts.inum;
      blockInfo.offset  := LFSTypes.INDIRECTOFFSET;
      blockInfo.flag    := LFSTypes.META;

      TRY
        writeLocation   := self.segBuffer.write(self.currentIndirect,
						blockInfo,
						self.segment.bytesInBlock);
      EXCEPT
      | Error.E, Segment.NoFreeSegs=>
        IO.PutError("unable to write indirect block map to segment buffer.");
      END;

      self.iguts.indirectBlocks[NIADDR-1] := writeLocation;
      (* so now clear the dirty indirect flag *)
      self.dirtyFlag := Word.And(self.dirtyFlag,Word.Not(INDIRECTDIRTY));

      (* but set the inode dirty flag *)
      self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);

    ELSIF self.currentIndirect = NIL THEN
      self.currentIndirect := NEW(REF ARRAY OF CHAR,self.segment.bytesInBlock);
    END;

    (* read new indirect block from disk to buffer *)
    (* sanity check *)
    writeLocation := self.iguts.indirectBlocks[indirectNum];
    IF writeLocation.segment = ILLEGALDISKVALUE OR
       writeLocation.block = ILLEGALDISKVALUE THEN
       (* IO.Put("LoadIndirectBlock. wrong!\n"); *)
    ELSE
      EVAL ReadBlock(self, writeLocation, self.currentIndirect);
    END;

    self.currentIndirectNum := indirectNum;

  END LoadIndirectBlock;

(* writes inode buffers to disk *)
PROCEDURE SyncINode(self:T) =
  VAR blockInfo : Segment.SegSummaryEntry;
      writeLocation:DiskAddress;
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("SyncINode called, dir:"&Fmt.Int(self.currentDirectNum)&
        " ind:"&Fmt.Int(self.currentIndirectNum)&".\n");
    END;

    (* if there is a direct block to write out *)
    (*IF self.currentDirectNum # ILLEGALDISKVALUE THEN*)

    IF Word.And(self.dirtyFlag, DIRECTDIRTY) # 0 THEN


      IF DEBUG = TRUE THEN
        IO.Put("SyncINode writing dirty direct block to disk.\n");
      END;

      (* write loaded direct-block back to disk *)
      blockInfo.iNode   := self.iguts.inum;
      blockInfo.offset  := self.currentDirectNum + LFSTypes.FIRSTDIRECTOFFSET;
      blockInfo.flag    := LFSTypes.META;

      TRY
        writeLocation   := self.segBuffer.write(self.currentDirect,
						blockInfo,
						self.segment.bytesInBlock);
        self.dirtyFlag := Word.And(self.dirtyFlag,Word.Not(DIRECTDIRTY));
      EXCEPT
      | Error.E, Segment.NoFreeSegs=>
        IO.PutError("SyncINode unable to write direct block map to segbuffer.");
      END;

      (* if that direct num requires an update to an off-inode indirect block *)
      IF self.currentDirectNum >= NIADDR-1 THEN
        LoadIndirectBlock(self,NIADDR-1);
        SUBARRAY(self.currentIndirect^,
		 self.currentDirectNum*BYTESIZE(DiskAddress),
		 BYTESIZE(DiskAddress)) :=
            VIEW(writeLocation,ARRAY [0..BYTESIZE(DiskAddress)-1] OF CHAR);

        (* clear the direct dirty *)
        self.dirtyFlag := Word.And(self.dirtyFlag,Word.Not(DIRECTDIRTY));

        (* set the indirect dirty *)
        self.dirtyFlag := Word.Or(self.dirtyFlag,INDIRECTDIRTY)

      ELSE
        self.iguts.indirectBlocks[self.currentDirectNum]:=writeLocation;

        (* clear the direct dirty *)
        self.dirtyFlag := Word.And(self.dirtyFlag,Word.Not(DIRECTDIRTY));

        (* set the inode dirty *)
        self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);
      END;

    END;


    (* if there is an indirect buffer to write out *)
    (*IF self.currentIndirectNum # ILLEGALDISKVALUE THEN*)

    IF Word.And(self.dirtyFlag,INDIRECTDIRTY) # 0 THEN

      IF DEBUG = TRUE THEN
        IO.Put("SyncINode writing dirty indirect block to disk.\n");
      END;

      (* write loaded indirect-block back to disk *)
      blockInfo.iNode   := self.iguts.inum;
      blockInfo.offset  := LFSTypes.INDIRECTOFFSET;
      blockInfo.flag    := LFSTypes.META;

      TRY
        writeLocation   := self.segBuffer.write(self.currentIndirect,
						blockInfo,
						self.segment.bytesInBlock);
      EXCEPT
      | Error.E, Segment.NoFreeSegs=>
        IO.PutError("SyncINode unable to write indirect block map to segbuf.");
      END;

      self.iguts.indirectBlocks[NIADDR-1]:=writeLocation;

      (* clear the indirect dirty flag *)
      self.dirtyFlag := Word.And(self.dirtyFlag,Word.Not(INDIRECTDIRTY));

      (* set the inode dirty flag *)
      self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);

    END;

  END SyncINode;

PROCEDURE RemapBlock(self:T; blockNum:CARDINAL;location:DiskAddress):BOOLEAN =
  VAR
    target, targetB: CARDINAL;
    bytesInBlock   : CARDINAL := self.segment.bytesInBlock;
    entriesInBlock : CARDINAL  :=  bytesInBlock DIV BYTESIZE(DiskAddress);
  BEGIN

    (* simple case: it lies amongst the inode's few direct blocks *)
    IF blockNum < NDADDR THEN
      self.iguts.directBlocks[blockNum] := location;
      self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);
      
    ELSE (* else re-mapped data block is referred to by inode indirect blocks *)

      (* absolute block offset into inode indirect blocks *)
      target := blockNum - NDADDR;

      (* absolute direct block offset into inode indirect blocks *)
      targetB := target DIV entriesInBlock;
      
      (* put the correct direct block into the inode direct block buffer *)
      LoadDirectBlock(self,targetB);

      (* put new mapping into that buffer *)
      SUBARRAY(self.currentDirect^,
	       BYTESIZE(DiskAddress)*(target MOD entriesInBlock),
	       BYTESIZE(DiskAddress)):=
          VIEW(location,ARRAY[0..BYTESIZE(DiskAddress)-1] OF CHAR);

      (* declare current buffer dirty *)
      self.dirtyFlag := Word.Or(self.dirtyFlag,DIRECTDIRTY);

    END;  (* end of indirect update stuff *)

    RETURN TRUE;

  END RemapBlock;

(* new version that uses on-inode meta-data buffers *)
PROCEDURE UpdateI(self :T; READONLY blockList: ARRAY OF INodeInfo.T; size: ShortCard):BOOLEAN =
  VAR
    entriesInBlock   : INTEGER;
    bytesInBlock     : ShortCard := self.segment.bytesInBlock;

  BEGIN
    IF DEBUG THEN IO.Put("inode.update: remapping blocks..."); END;

    (* number of DiskAddresses held in a direct block.*)
    entriesInBlock := bytesInBlock DIV BYTESIZE(DiskAddress);

    FOR i := 0 TO size-1 DO
      IF DEBUG = TRUE THEN IO.Put(Fmt.Int(blockList[i].block) & "."); END;

      (* error cases *)
      IF blockList[i].block >= MAXBLOCKNUM THEN
        IO.Put("update instruction " &Fmt.Int(i)& " is out of inode range.\n");
        RETURN FALSE;
      END;

      EVAL RemapBlock(self,blockList[i].block,blockList[i].diskAddress);
    END;

    (* CLEANUP *)

    (* FlushI(self); *** Don't use flush here, the caller of Update needs to
     * flush the inode when update returns. if flush is called here, the main
     * inode is dirty and will be written out, only to be invalidated by the
     * next write. *)

    SyncINode(self);

    IF DEBUG = TRUE THEN IO.Put("UpdateI exiting.\n"); END;

    RETURN TRUE;  

  END UpdateI;

  (* UPDATE DATA BLOCK LOCATIONS *)
  (* is used by write, but also by the cleaner *)
PROCEDURE Update(self : T; READONLY blockList : ARRAY OF INodeInfo.T; size : ShortCard) : BOOLEAN =
  BEGIN
    LOCK self.mu DO
      RETURN UpdateI(self,blockList,size);
    END;

  END Update;

  (* UPDATE META-DATA BLOCK LOCATIONS *)
PROCEDURE UpdateMeta(self : T; READONLY blockList : ARRAY OF INodeInfo.T; size : ShortCard) : BOOLEAN =
  BEGIN
    LOCK self.mu DO
      RETURN UpdateMetaI(self,blockList,size);
    END;
  END UpdateMeta;


(* update location of inode meta data *)
(* Every block number passed to this function refers to an inode meta data
 * block.  The main inode block (containing name, direct blocks, etc..) is
 * called LFSTypes.INODEOFFSET, the inode indirect block is referred to by
 * LFSTypes.INDIRECTOFFSET.  The rest of the inode blocks are direct blocks
 * (remember there is only one indirect block), and they are referred to in
 * order, with the first block starting at LFSTypes.FIRSTDIRECTOFFSET. *)
PROCEDURE UpdateMetaI(self :T; READONLY blockList: ARRAY OF INodeInfo.T; size: ShortCard):BOOLEAN =
  VAR
    entriesInBlock   : INTEGER;
    bytesInBlock     : ShortCard := self.segment.bytesInBlock;
  BEGIN

    IF DEBUG THEN
      IO.Put("UpdateMeta iNode "&Fmt.Int(self.iguts.inum) &
	     " with "& Fmt.Int(size)&" blocks.\n");
    END;

    (* number of DiskAddresses held in a direct block.*)
    entriesInBlock := bytesInBlock DIV BYTESIZE(DiskAddress);

    FOR i := 0 TO size-1 DO

      WITH block = blockList[i].block DO
        IF DEBUG THEN IO.Put(Fmt.Int(block) & "."); END;

        (* error cases *)
        IF block >= entriesInBlock THEN
          IO.PutError("inode.updateMeta " & Fmt.Int(i) & ": " &
		      Fmt.Int(block) & " > " & Fmt.Int(entriesInBlock) & "\n");
          RETURN FALSE;
        END;

        (* if the block is the main inode block*)
        IF block = LFSTypes.INODEOFFSET THEN
          (* flag inode block as dirty *)
          self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);
	  IF DEBUG THEN IO.Put(" INODEDIRTY.\n"); END;

        (* if the block is the indirect block then tell the inode to load it,
	 * then declare it dirty, the inode will write it out later *)
        ELSIF block = LFSTypes.INDIRECTOFFSET THEN
          LoadIndirectBlock(self,NIADDR-1);
          self.dirtyFlag := Word.Or(self.dirtyFlag,INDIRECTDIRTY);
          IF DEBUG THEN IO.Put(" INDIRECTDIRTY.\n"); END;

        (* if the block is a direct block then tell inode to load it,
         * declare it dirty and inode will write it out later *)
        ELSE
          LoadDirectBlock(self,block-LFSTypes.FIRSTDIRECTOFFSET);
          self.dirtyFlag := Word.Or(self.dirtyFlag,DIRECTDIRTY);
          IF DEBUG THEN IO.Put(" DIRECTDIRTY.\n"); END;
        END;

      END;  (* end of WITH *)

      (* write those fragments back to disk *)
      (*SyncINode(self);*)

    END; (* end of FOR *)

    (* and possibly write the main inode block back to disk too *)
    FlushI(self);

    RETURN TRUE;

  END UpdateMetaI;

  (* A function mainly for the cleaner.  Verifies that the subject
     is an actual data block being mapped by the INode.
  *)
PROCEDURE VerifyData(self : T; blockInfo : INodeBlockInfo) : BOOLEAN =
  VAR
    maxValidBlock := self.iguts.size DIV self.segment.bytesInBlock;
    found : BOOLEAN;
    blocknum := blockInfo.block;
    da : DiskAddress;
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("VerifyData called with blockInfo segment:"&
	Fmt.Int(blockInfo.diskAddress.segment)&" block:"&
        Fmt.Int(blockInfo.diskAddress.block)&".\n");
    END;

    (* XXX maxValidBlock := (self.iguts.size +511) DIV 512 ? *)
    IF blocknum > maxValidBlock THEN
      found := FALSE;
    ELSE

      LOCK self.mu DO
        (* let GetBlockLoc do the work, then compare the returned
           disk address with what the caller thinks it should be
        *)
        TRY
          da := GetBlockLocI(self,blocknum);
          found := da = blockInfo.diskAddress;
        EXCEPT
        | InvalidBlockRequest=>
          IO.PutError("INode sent invalid block request to self.\n");
          found:= FALSE;
        END;
        
      END; (* end LOCK *)
    END;

    IF DEBUG THEN
      IO.Put("VerifyData returning:");
      IF found = TRUE THEN IO.Put("TRUE\n");
      ELSE IO.Put("FALSE\n");
      END;
    END;

    RETURN found;

  END VerifyData;

  (* - verifyData's evil twin.  Verifies that the subject is a meta
       data block being used by the INode.
     - since the inode's disk address is considered meta data and I
       don't keep it in the inode's structure, the caller of this
       routine has to do the check on the disk address of the inode.
  *)
PROCEDURE VerifyMeta(self : T; blockInfo : INodeBlockInfo) : BOOLEAN =
  VAR found: BOOLEAN;

  BEGIN

    IF DEBUG THEN
      IO.Put("VerifyMeta called with blockInfo segment:"&
	Fmt.Int(blockInfo.diskAddress.segment)&" block:"&
        Fmt.Int(blockInfo.diskAddress.block)&".\n");
    END;

    LOCK self.mu DO
      (* there are three cases, the block can be the inode itself
         can be the indirect block or can be one of the inode direct blocks *)

      (* case 1   DIRECT BLOCK *)
      IF blockInfo.block = LFSTypes.INODEOFFSET THEN

        WITH realLocation = self.imap.getLocation(self.iguts.inum) DO
          found := blockInfo.diskAddress = realLocation;
          IF DEBUG AND found THEN
	    IO.Put("Found live meta block, main inode ");
	  END;
        END;

      (* case 2   INDIRECT BLOCK *)
      ELSIF blockInfo.block = LFSTypes.INDIRECTOFFSET THEN
        found := blockInfo.diskAddress = self.iguts.indirectBlocks[NIADDR-1];
        IF DEBUG AND found THEN
	  IO.Put("Found live meta block, indirect block ");
	END;

      (* case 3   block is INDIRECTLY REFERRED DIRECT BLOCK *)
      ELSE

        WITH blockNum = blockInfo.block-LFSTypes.FIRSTDIRECTOFFSET DO

          IF blockNum >=
	     (self.segment.bytesInBlock DIV BYTESIZE(DiskAddress)) THEN
            found := FALSE;
          ELSE
            (* if direct block is pointed to by inode *)
            IF blockNum < NIADDR-1 THEN

              found := blockInfo.diskAddress = self.iguts.indirectBlocks[blockNum];
              IF DEBUG AND found THEN
                IO.Put("Found live meta, direct direct:"&Fmt.Int(blockNum)&" ");
              END;

            ELSE
	      (* otherwise the block is allegedly referred to by the indirect
		 block. gotta load the indirect block, verify the address
		 given *)
              LoadIndirectBlock(self,NIADDR-1);

              found := blockInfo.diskAddress =
		       VIEW(SUBARRAY(self.currentIndirect^,
                                     BYTESIZE(DiskAddress)*blockNum,
                                     BYTESIZE(DiskAddress)),
                            DiskAddress);

              IF DEBUG AND found THEN
                IO.Put("Found live meta, indirect direct:"&Fmt.Int(blockNum)&" ");
              END;

            END;
          END;
        END;  (* WITH *)
      END;

      (*
        IF DEBUG AND found THEN
        IO.Put("at segment:"&
        Fmt.Int(blockInfo.diskAddress.segment)&
        " block:"&Fmt.Int(blockInfo.diskAddress.block)&".\n");
        END;
      *)
    END; (* LOCK *)

    RETURN found;

  END VerifyMeta;

  (* returns the size of the file in bytes *)
PROCEDURE ByteSize(self : T) : INTEGER =
  BEGIN
    RETURN self.iguts.size;
  END ByteSize;


PROCEDURE SetByteSize(self : T; size : INTEGER) =
  BEGIN

    LOCK self.mu DO
      self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);
      self.iguts.size := size;
    END;

  END SetByteSize;


(************************************)
(* Directory Stuff *)

PROCEDURE SetFileType(self:T; type:ID) RAISES {IllegalFileType}= 
  BEGIN

    LOCK self.mu DO
      IF type # LFSTypes.FILE AND type # LFSTypes.DIR THEN
        RAISE IllegalFileType;
      END;

      self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);
      self.iguts.type := type;
    END;
    
  END SetFileType;


PROCEDURE IsDir(self:T):BOOLEAN = 
  BEGIN
    RETURN (self.iguts.type = LFSTypes.DIR);
  END IsDir;


PROCEDURE IsFile(self:T):BOOLEAN = 
  BEGIN
    RETURN (self.iguts.type = LFSTypes.FILE);
  END IsFile;


(* returns an alloced and populated DirInfoArray if iNode is a dir,
   else raises an exception.
   assumes self is directory! *)
PROCEDURE GetDirObj(self: T):DirObject.T RAISES{Error.E} =
  VAR
    bytesInBlock: CARDINAL;
    blockLocation: DiskAddress;
    diskDirEntrySize: CARDINAL;
    block: FastBuffer.T;
    name:TEXT;
    err: LFSTypes.LFSErrorT;
    dirObject :  DirObject.T;
  BEGIN

    IF DEBUG THEN IO.Put("INode.GetDirObj() called.\n"); END;
    
    bytesInBlock := self.segment.bytesInBlock;
    
    (* this is the size of a directory entry on disk*)    
    diskDirEntrySize := LFSTypes.MAXNAMELEN +2;
    
    (* this allocs an initial DirObject that has enough entries 
       for all the files in the dir *)
    dirObject :=
      DirObject.NewDirObject((self.iguts.size+bytesInBlock-1) DIV bytesInBlock);
      
    (* If the dir is empty then we're done and we'll return an empty dir *)
    IF self.iguts.size = 0 THEN
      IF DEBUG THEN IO.Put("INode says dirObject was empty.\n"); END;
      RETURN dirObject;
    END;
      
    (* a useful block sized ref array of chars *)
    block := FastBuffer.Allocate(bytesInBlock);
      
    (* now read entire file into buffer *)
    (* for each block in file... *)
    FOR i:=0 TO (((self.iguts.size+bytesInBlock-1) DIV bytesInBlock)-1) DO
      
      IF DEBUG THEN IO.Put("GetDirObject dirEntry blk:"&Fmt.Int(i)&".\n"); END;

      TRY
        blockLocation := GetBlockLocI(self,i); (* internal version *)
      EXCEPT
      | InvalidBlockRequest =>
        err := NEW(LFSTypes.LFSErrorT);
        err.messageE := "Error, GetBlockLoc recieved request for illegal block "&Fmt.Int(i)&".\n";
	FastBuffer.Deallocate(block);
        RAISE Error.E(err);
      END;

      IF DEBUG = TRUE THEN
        IO.Put("INode.GetDirObject told that block:"&Fmt.Int(i)&
          " is at location:"&Fmt.Int(blockLocation.segment)&","&
	  Fmt.Int(blockLocation.block)&".\n");
      END;

      (* Do I ought to do this? *)
      (*
      IF blockLocation.segment = ILLEGALDISKVALUE OR
	 blockLocation.block= ILLEGALDISKVALUE THEN
	 IO.Put("GetDirObj. ILLEGALDISKVALUE.\n");
      END;
      *)
      EVAL ReadBlock(self, blockLocation, block.data);

      IF DEBUG THEN IO.Put("Read file block:"&Fmt.Int(i)&".\n"); END;

      (* add the directory entry to the array *)
      WITH temp = VIEW(block.data^, LFSTypes.DirectoryData) DO
        TRY
          (* this chops off everything after the null terminator from *)
          (* the filename and makes it into TEXT *)
          name:=Text.FromChars(temp.name);

          WITH i = Text.FindChar(name,VAL(0,CHAR),0) DO
            IF i # -1 THEN
              name := Text.Sub(name,0,i);
            ELSE
              IO.PutError("fetching dirObj for inode:"&Fmt.Int(self.iguts.inum)
			  &", entry "&Fmt.Int(i)&".\n");
	      name := "~";
            END;
          END;

          IF DEBUG THEN
            IO.Put("INode.GetDirObj adding "&name&", "&Fmt.Int(temp.iNum)&"\n");
          END;

          EVAL dirObject.addElement(temp.iNum,name);

        EXCEPT
        | DirObject.IllegalFileName =>
          err := NEW(LFSTypes.LFSErrorT);
          err.messageE := "Error, addElement returns IllegalFileName: "&name;
	  FastBuffer.Deallocate(block);
          RAISE Error.E(err);
        END;
      END;

    END; (* end of for loop*)

    IF DEBUG THEN IO.Put("INode.GetDirObject returning dirObject.\n"); END;

    FastBuffer.Deallocate(block);
    RETURN dirObject;

  END GetDirObj;


(* file array obtains a lock on inode *)
PROCEDURE GetFileArray(self: T; pos: INTEGER;
		       VAR buf: ARRAY OF DirEnt.T):INTEGER RAISES{Error.E} =
VAR       err    : LFSTypes.LFSErrorT;
BEGIN

  IF DEBUG THEN IO.Put("INode.GetFileArray() called.\n"); END;

  LOCK self.mu DO

    IF self.iguts.type # LFSTypes.DIR THEN
      err := NEW(LFSTypes.LFSErrorT);
      err.messageE := "GetFileArray called on non-directory iNode.\n";
      RAISE Error.E(err);
    END;

    IF self.dirObject = NIL THEN
      self.dirObject := GetDirObj(self);
      IF DEBUG THEN IO.Put("iNode.getFileArray recieved dirObject.\n"); END;
    END;

    IF self.dirObject = NIL THEN
       IO.PutError("Error!  getDirObject returned NIL!");
    END;

    RETURN self.dirObject.getFileArray(pos,buf);

  END; (* LOCK *)

END GetFileArray;


PROCEDURE DirSize(self:T):INTEGER RAISES{Error.E} =
  VAR err :LFSTypes.LFSErrorT;
      bytesInBlock: INTEGER;
      retVal : INTEGER;
  BEGIN
    
    IF self.isDir() # TRUE THEN
      err := NEW(LFSTypes.LFSErrorT);
      err.messageE := "DirSize called on non-directory iNode.\n";
      RAISE Error.E(err);
    END;

    bytesInBlock := self.segment.bytesInBlock;

    retVal := ((self.byteSize()+bytesInBlock-1) DIV bytesInBlock);
    
    IF DEBUG THEN IO.Put("INode.DirSize returning:"&Fmt.Int(retVal)&".\n"); END;

    RETURN retVal;

  END DirSize;


(* XXX FIXME.  LOCK is needed? *)
PROCEDURE GetFileINum(self:T; name:TEXT):
	LFSTypes.ShortCard RAISES {FileNotFound, NotDirectory, IllegalFileName}=
  BEGIN
    LOCK self.mu DO
      RETURN GetFileINumI(self, name);
    END;
  END GetFileINum;

PROCEDURE GetFileINumI(self:T; name:TEXT):
	LFSTypes.ShortCard RAISES {FileNotFound, NotDirectory, IllegalFileName}=
  BEGIN

    IF DEBUG THEN IO.Put("INode.GetFileINum called with name:"&name&".\n"); END;

    (* if iNode is not a dir *)
    IF self.iguts.type # LFSTypes.DIR THEN
      IO.PutError("INode.getFileINum called on non-directory iNode.\n");
      RAISE NotDirectory;
    END;

    (* if dirObject is already instantiated for this inode *)
    (* then we can use it right off, otherwise we build it *)
    IF self.dirObject = NIL THEN
      IF DEBUG THEN IO.Put("INode.getFileINum. instantiating dirObject\n"); END;
      self.dirObject := GetDirObj(self); (* loads array into this iNode *)
    END;

    (* return the files iNum *)
    IF DEBUG THEN
        IO.Put("INode.getFileINum. dirObject.getINumWithName("&name&")\n");
    END;
    TRY 
      RETURN self.dirObject.getINumWithName(name);
    EXCEPT
    | DirObject.IllegalFileName => 
      RAISE IllegalFileName;
    | DirObject.FileNotFound => 
      RAISE FileNotFound;
    END;

  END GetFileINumI;    
    

(* add an entry to a directory. *)
(* XXX to avoid: warning: large parameter passed by value (258 bytes) *)
(*   make sure not to change dirEnt. *)
PROCEDURE AddDirEntry(self: T; (* dirEnt: LFSTypes.DirectoryData *) 
			       VAR dirEnt: LFSTypes.DirectoryData)
  RAISES{FileExists, IllegalFileName, NotDirectory, FileError}=
  VAR
    block              : FastBuffer.T;
    blockInfo          : Segment.SegSummaryEntry;
    dAddr              : DiskAddress;
    newBlockNumber     : CARDINAL;
    bytesInBlock       : CARDINAL;
    iNode              : T;

    blockList          := NEW(REF ARRAY OF INodeInfo.T,1);
    name               : TEXT;

  BEGIN

    LOCK self.mu DO

      bytesInBlock := self.segment.bytesInBlock;
      blockList[0]:= NEW(INodeInfo.T);
      
      (* get the file name *)
      name := Text.FromChars(dirEnt.name);
      WITH i = Text.FindChar(name,'\000',0) DO
        name := Text.Sub(name,0,i);
      END;
      
      IF DEBUG = TRUE THEN
        IO.Put("INode.AddDirEntry Called.\n");
        IO.Put("new file name is:"&name&".\n");
      END;
      
      (* if iNode is not a dir *)
      IF self.iguts.type # LFSTypes.DIR THEN
        RAISE NotDirectory;
      END;
      
      (* if dirObject is already instantiated for this inode *)
      (* then we can use it right off, otherwise we build it *)
      IF self.dirObject = NIL THEN
        IF DEBUG THEN IO.Put("INode is instantiating dirObject\n"); END;
        self.dirObject := GetDirObj(self); (* loads array into this iNode *)
      END;
      
      (* check to see if file already exists *)
      TRY
        EVAL self.dirObject.getINumWithName(name);
        RAISE FileExists;
      EXCEPT
      | DirObject.IllegalFileName => RAISE IllegalFileName;
      | DirObject.FileNotFound => 
        (* this ISTYPE what we want, so do nothing *)        
        IF DEBUG THEN IO.Put("Filename doesn't exist in directory.\n");  END;
      END;

      (* now write the new element back to disk *)
      (* 
         We want to be able to add files to the dir, without having to
         re-write the whole directory.  Because of this, I've decided to
         make each dir entry take an entire block.  To add an element to
         a dir, we write the block to the segment buffer-which returns to
         us the location of that block.  We then expand the size of the file
         by one block, and point that block to the location of the new entry.
      *)
      
      (* first find the block offset of the block we'll be adding to the file *)
      newBlockNumber := ((self.byteSize() + bytesInBlock -1) DIV bytesInBlock);

      (* construct blockInfo for segBuffer *)
      blockInfo.iNode  := self.iguts.inum;
      blockInfo.offset := newBlockNumber;
      blockInfo.flag   := LFSTypes.DATA;
      
      (* now fill in the block array with the dirEnt *)
      block := FastBuffer.Allocate(self.segment.bytesInBlock);
      WITH buf = VIEW(dirEnt, ARRAY [0..BYTESIZE(dirEnt)-1] OF CHAR) DO
        SUBARRAY(block.data^, 0, BYTESIZE(dirEnt)) := buf;
      END;
      
      (* write the block to the segBuffer *)
      TRY
        dAddr := self.segBuffer.write(block.data, blockInfo, bytesInBlock);
      EXCEPT
      | Error.E, Segment.NoFreeSegs=>
        IO.PutError("Error, INode unable to write to segment buffer.");
	FastBuffer.Deallocate(block);
        RAISE FileError;
      END;
      FastBuffer.Deallocate(block);

      (* add new element to the dirObject *)
      IF DEBUG THEN
        IO.Put("Adding element to dirObject. Name: "&name&
		" iNum:"&Fmt.Int(dirEnt.iNum)&".\n");
      END;
      TRY
        EVAL self.dirObject.addElement(dirEnt.iNum,name);
      EXCEPT
      | DirObject.IllegalFileName => 
        RAISE IllegalFileName;
      END;

      (* now increase the file size and update the iNode to point to the new block *)
      self.iguts.size := (1+newBlockNumber)*bytesInBlock;

      blockList[0].iNode := self.getINum();
      blockList[0].block := newBlockNumber;
      blockList[0].diskAddress := dAddr;

      self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);

      (* here we ask the inode to update its meta data.  If something
         goes wrong in there, then we'll reduce the file size back to 
         what it was, and remove the element from the dirObject *)
      IF UpdateI(self,blockList^,1) = FALSE THEN
        self.iguts.size := newBlockNumber*bytesInBlock;
        TRY
          EVAL self.dirObject.rmElement(dirEnt.iNum,name);
        EXCEPT
        | DirObject.IllegalFileName =>
          IO.PutError("*************************************************\n");
          IO.PutError(" in INode.AddDirEntry, You should never get here.\n");
          IO.PutError("*************************************************\n");
          RAISE IllegalFileName;
        END;
        RAISE FileError;
      END;

      (* must now flush iNode to disk as location of meta-info blocks may
	 have changed, and inode must keep itself recorded on disk.*)
      FlushI(self);

      (* XXX. Why can't we do self.incLinkCount() ? *)
      (* finally we update the link count to the new file *)
      TRY
        iNode := self.imap.getINode(dirEnt.iNum);
      EXCEPT
      | IMap.InvalidINum => RAISE IllegalFileName;
      END;

      iNode.incLinkCount();
       
    END;  (* end of lock *)

  END AddDirEntry;


(* Internal file removal method *)
(* removes an entry from a directory.*)
(* method requires the callee to call this on the iNode that
   allegedly contains the file to be removed, this function
   doesn't recurse directory paths.
   method further requires the user to have the doomed file's
   dirEnt, this can be obtained from the doomed files parent directory.
   name should be name of file to remove
   iNum should be iNum of file to remove

   The method will not allow removal of directorys that
   still contain files.
*)
(* XXX FIXME.  LOCK is needed? *)
PROCEDURE RmDirEntry(self: T; name:TEXT;iNum:LFSTypes.ShortCard)
  RAISES{FileNotFound, IllegalFileName, NotDirectory, NonEmpty}=
  BEGIN
    LOCK self.mu DO
      RmDirEntryI(self, name, iNum);
    END;
  END RmDirEntry;

PROCEDURE RmDirEntryI(self: T; name: TEXT; iNum: LFSTypes.ShortCard)
  RAISES{FileNotFound, IllegalFileName, NotDirectory, NonEmpty}=
  VAR
    blockList          : REF ARRAY OF INodeInfo.T;
    bytesInBlock       : CARDINAL;
    iNumToRemove       : CARDINAL;
    movedBlockNumber   : CARDINAL;
    removedBlockNumber : CARDINAL;
    victimINode        : T;
    blockListIndex     : CARDINAL;

  BEGIN

    bytesInBlock := self.segment.bytesInBlock;

    IF DEBUG THEN IO.Put("In INode.RmDirEntry, to remove :"&name&".\n"); END;

    (* if iNode is not a dir *)
    IF self.iguts.type # LFSTypes.DIR THEN
      RAISE NotDirectory;
    END;

    (* if dirObject is already instantiated for this inode *)
    (* then we can use it right off, otherwise we build it *)
    IF self.dirObject = NIL THEN
      IF DEBUG THEN
        IO.Put("INode is instantiating dirObject for rmDirEntry\n");
      END;
      self.dirObject := GetDirObj(self); (* loads array into this iNode *)
    END;

    IF DEBUG THEN IO.Put("INode dirObject instantiated for rmDirEntry.\n"); END;

    (* check to see that file exists *)
    TRY 
      iNumToRemove := self.dirObject.getINumWithName(name);
    EXCEPT
    | DirObject.IllegalFileName => RAISE IllegalFileName;
    | DirObject.FileNotFound => 
      IF DEBUG THEN IO.PutError("INode can't find file to remove.\n"); END;
      RAISE FileNotFound;
    END;
    IF DEBUG THEN
      IO.Put("Found file to remove, iNum :"&Fmt.Int(iNumToRemove)&".\n");
    END;

    (* check out the inode and make sure it isn't a directory
       with files inside it *)
    TRY
      victimINode := self.imap.getINode(iNumToRemove);
    EXCEPT
    | IMap.InvalidINum => RAISE IllegalFileName;
    END;

    IF victimINode.isDir() = TRUE THEN
      IF victimINode.dirSize()>0 THEN
        RAISE NonEmpty;
      END;
    END;

    (* remove the element *)
    IF DEBUG THEN
        IO.Put("Removing element from dirObject. Name: "&name&
		"iNum:"&Fmt.Int(iNum)&".\n");
      END;
    TRY
      (* returns index of block that was removed *)
      removedBlockNumber := self.dirObject.rmElement(iNum,name);
    EXCEPT
    | DirObject.IllegalFileName => RAISE IllegalFileName;
    END;
    IF DEBUG THEN
      IO.Put("Removing block number:"&Fmt.Int(removedBlockNumber)&".\n");
    END;

    (* now update the iNode to lose the gap in the file that removing this
       entry caused *)
    WITH size = self.byteSize() DO
      IF size < 1 THEN
        IO.PutError("Inconsistancy in INode.m3, dir file size is zero.\n");
        RAISE NotDirectory;
      END;

      (* first find the block offset of the dirs final block *)
      movedBlockNumber := ((size + bytesInBlock -1) DIV bytesInBlock)-1;
    END;

    IF DEBUG THEN
      IO.Put("Final block in directory is :"&Fmt.Int(movedBlockNumber)&".\n");
    END;

    (* if we didn't just delete the last entry in the list *)
    IF removedBlockNumber<movedBlockNumber THEN
      blockList := NEW(REF ARRAY OF INodeInfo.T, 1);
      
      IF DEBUG THEN
        IO.Put("removedBlockNumber:"&Fmt.Int(removedBlockNumber)&
	       " lastFileBlock:"&Fmt.Int(movedBlockNumber)&".\n");
      END;
      
      (* build iNode update array object *)
      blockList[0] := NEW(INodeInfo.T);
      blockList[0].iNode := self.getINum();     (* iNum of directory iNode *)
      blockList[0].block := removedBlockNumber; (* Block number to re-map *)

      TRY
        blockList[0].diskAddress := self.getBlockLoc(movedBlockNumber);
	(* store disk address of remapped block *)
      EXCEPT
      | InvalidBlockRequest => RAISE NotDirectory;
      END;

      IF DEBUG THEN
        IO.Put("Finished initializing array :"&Fmt.Int(blockListIndex)&".\n");
      END;

      (* remap the name->block hash so block := block - 1 *)
      TRY
        self.dirObject.mvElement(movedBlockNumber,removedBlockNumber);
      EXCEPT
      | DirObject.IllegalFileName => RAISE IllegalFileName;
      END;

      EVAL self.update(blockList^,1);

    ELSE
      IF DEBUG THEN IO.Put("No remapping needed.\n"); END;
    END;

    (* now decrease the file size *)
    self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);  (* just in case *)
    self.iguts.size := movedBlockNumber * bytesInBlock;

    IF DEBUG THEN
      IO.Put("Decreased file size to:"&Fmt.Int(movedBlockNumber)&" blocks.\n");
    END;

    (* and write the parent directory inode out *)
    FlushI(self);

    IF DEBUG THEN IO.Put("flushed iNode.\n"); END;

    DecLinkCountI(victimINode);

    IF DEBUG THEN IO.Put("Exiting iNode.rmDirEntry.\n"); END;

  END RmDirEntryI;


PROCEDURE IncLinkCount(self:T) =
  BEGIN
    
    LOCK self.mu DO
      self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);
      INC(self.iguts.linkCount);
    END; (* end lock *)

  END IncLinkCount;


PROCEDURE DecLinkCount(self:T) =
  BEGIN
    LOCK self.mu DO
      DecLinkCountI(self);
    END;
  END DecLinkCount;

(* internal version of decLinkCount.  called from RmDirEntryI *)
PROCEDURE DecLinkCountI(self:T) =
  BEGIN
    IF self.iguts.linkCount < 1 THEN
      IO.PutError("iNode decLinkCount, iNode already had count of 0.\n");
    ELSE
      DEC(self.iguts.linkCount);
    END;

    (* if linkCount reaches zero then noone points to it, iNode can
       destroy itself *)
    IF self.iguts.linkCount < 1 THEN
      IF DEBUG THEN IO.Put("INode destroying itself.\n"); END;
      self.imap.rmINode(self.iguts.inum);
    ELSE
      self.dirtyFlag := Word.Or(self.dirtyFlag,INODEDIRTY);
    END;

  END DecLinkCountI;

(* end of directory code *)
(**********************************************)

  (* yeah, it returns the version *)
PROCEDURE GetVersion(self:T): ShortCard =
  BEGIN
    RETURN self.iguts.version;
  END GetVersion;

  (* reads the most current version of a given block.  If the block is
     in the SegBuffer, we get it from there, otherwise, we go to disk *)
PROCEDURE ReadBlock(self : T; from : DiskAddress; buf : RCharArray) : CARDINAL =
  VAR
    byteCount : CARDINAL;

  BEGIN

    (* if the segBuffer is buffering the segment containing the Block
       then read the block from the segBuffer, otherwise, read it
       from the disk.  Don't rely on segBuffer.getCurrentSegment()!
       current segment may change after fetching it.
    *)
    
    byteCount := self.segment.bytesInBlock;
    
    TRY
      EVAL self.segBuffer.read(buf^, from, 0);
    EXCEPT
    | SegBuffer.BlockOutOfBounds =>
      IO.PutError("INode "&Fmt.Int(self.iguts.inum) & 
	     " tried to read out of bounds segBuf block:" &
	     Fmt.Int(from.segment)&" block:"&Fmt.Int(from.block)&".\n");
      RETURN 0;
    END;

    RETURN byteCount;
  END ReadBlock;

(****************************
  INodePrivate interface
****************************)

PROCEDURE ConstructINode(segBuffer : SegBuffer.Buffer; imap : IMap.T;
  from : DiskAddress) : ROOT=
  VAR
    inode : T;
  BEGIN
    IF DEBUG THEN IO.Put("Construct INode called.\n"); END;

    inode := NEW(T);

    TRY
      inode.init(segBuffer, imap, from);
    EXCEPT
    | INodeInitError =>
      IO.PutError("INode init error.  Construct inode returning NIL.\n");
    END;

    IF DEBUG THEN IO.Put("Construct INode exiting.\n"); END;
    RETURN inode;
  END ConstructINode;

PROCEDURE NewINode(segBuffer : SegBuffer.Buffer; imap : IMap.T; info: INumInfo)
	: ROOT =
  VAR
    inode : T;
  BEGIN
    inode := NEW(T);
    inode.create(segBuffer, imap, info);

    RETURN inode;
  END NewINode;

BEGIN
END INode.

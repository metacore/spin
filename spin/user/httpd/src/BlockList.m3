(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 25-Sep-97  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the error messages to be easily identifiable.
 *
 * 07-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed MaxBlocksInFile to equal MaxBlockNo.
 *
 * 01-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added size argument to addblock and retrieveblock.
 *
 * 03-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added swapping and pageout onto an extent based storage device.
 *
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. List of blocks in a file.
 *
 *)
MODULE BlockList;
IMPORT Buffer, Error, Extent;
IMPORT ReaderWriterLock;
IMPORT Fmt;
IMPORT IO;
IMPORT SpinException, AccessMode, SecurityManager, SecurityManagerPrivate;
FROM Httpd IMPORT debug;

TYPE BlockDesc = RECORD
  buffer : Buffer.T; (* Actual data *)
  size   : INTEGER;
  lock   : ReaderWriterLock.T;
  extent : Extent.T; (* the extent that is allocated for this block. *)
  ondisk : BOOLEAN := FALSE;
END;

REVEAL T = Public BRANDED Brand OBJECT
    key  : TEXT;               (* The key for this block list (e.g.filename) *)
    block: ARRAY [FIRST(BlockNo)..LAST(BlockNo)] OF BlockDesc;
    totsize: INTEGER;          (* total amount of memory consumed by data *)
    maxsize: INTEGER;          (* max amount of memory for data *)
                               (* overflow triggers a spill to secondary *)
                               (* storage *)
    maxblockno: INTEGER := -1;  (* max number of blocks in the list *)
    flushpolicy: Policy := DefaultPolicy;
    swap: Extent.T;            (* the swap extent onto which we page out *)
    devName : TEXT;
  OVERRIDES
    destroy := Destroy;
    addBlock := AddBlock;
    tryRetrieveBlock := TryRetrieveBlock;
    retrieveBlock := RetrieveBlock;
    retrieveData := RetrieveData;
    unlockBlock := UnlockBlock;
    setPolicy := SetPolicy;
    setMaxSize := SetMaxSize;
    pageout := PageOut;
    size := Size;
    registerSwap := RegisterSwap;
END;

CONST
  ConsistencyCheck = TRUE; (* do some sanity checking *)
  SECURITY = FALSE;
  READ     = AccessMode.SimpleT{ AccessMode.READ };
  WRITE    = AccessMode.SimpleT{ AccessMode.WRITE };
  BURP     = SpinException.ExceptionInfo{
                 code := SpinException.ExceptionCode.UnknownException,
                 msg  := SpinException.ExceptionNames[
                         SpinException.ExceptionCode.UnknownException]};

PROCEDURE New(<*UNUSED*>sizehint: INTEGER; maxsize: INTEGER := 0) : T =
  VAR bl: T;
  BEGIN
    bl := NEW(T);
    bl.maxsize := maxsize;
    RETURN bl;
  END New;

PROCEDURE Destroy(self: T) =
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,WRITE) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    FOR i := FIRST(self.block) TO self.maxblockno DO
      IF self.block[i].ondisk THEN
        (* XXX the data is on disk, deallocate it. *)
        self.block[i].ondisk := FALSE;
      END;
      (* dealloc the data refs *)
      Buffer.Deallocate(self.block[i].buffer);
      self.block[i].buffer := NIL;
      (* dealloc the mutex *)
      self.block[i].lock := NIL;
    END;
    self.maxblockno := -1;
    self.maxsize := 0;
    self.flushpolicy := DefaultPolicy;
  END Destroy;

PROCEDURE AddBlock(self: T; blockno: BlockNo; block: Block; size: CARDINAL) =
  VAR
    datasize : CARDINAL := MIN(size,NUMBER(block.data^));
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,WRITE) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    IF debug THEN
      IO.Put("Adding block " & Fmt.Int(blockno) & " to the cache.\n");
    END;
    INC(self.totsize, datasize);
    IF self.maxblockno < blockno THEN
      self.maxblockno := blockno;
    END;
    IF ConsistencyCheck AND self.block[blockno].buffer # NIL THEN
      IO.PutError("BlockList: reinserting block #" &
        Fmt.Int(blockno) & "multiply.\n");
    END;
    self.block[blockno].lock := ReaderWriterLock.Allocate();
    self.block[blockno].buffer := block;
    self.block[blockno].ondisk := FALSE;
    self.block[blockno].size := datasize;

    Pack(self);
  END AddBlock; 

PROCEDURE RetrieveBlock(self: T; blockno: BlockNo; VAR size: CARDINAL) : Block =
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,READ) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    IF blockno > self.maxblockno THEN RETURN NIL; END;
    IF debug THEN
      IO.Put("Retrieving block " & Fmt.Int(blockno) & " from the cache.\n");
    END;
    LOOP
      IF self.block[blockno].ondisk = FALSE THEN
        IF ConsistencyCheck AND self.block[blockno].buffer = NIL THEN
          IO.PutError("BlockList: RetrieveBlock returning a buffer that is NIL\n");
        END;
        ReaderWriterLock.ReaderLock(self.block[blockno].lock);
        size := self.block[blockno].size;
        RETURN self.block[blockno].buffer;
      ELSE
        VAR nbytes: CARDINAL;
        BEGIN
          (* read block from disk *)
          IF debug THEN
            IO.Put("Reading cached block from swap.\n");
          END;
          ReaderWriterLock.WriterLock(self.block[blockno].lock);
          TRY
            IF self.block[blockno].buffer = NIL THEN
              self.block[blockno].buffer := Buffer.Allocate(self.block[blockno].size);
              nbytes := self.block[blockno].size;
              IF debug THEN
                IO.Put("Reading from offset " &
                       Fmt.Int(self.block[blockno].extent.getBaseOffset()) & 
                       " on swap disk " &
                       Fmt.Int(nbytes) &
                       " many bytes\n");
              END;
              TRY
		nbytes := self.block[blockno].extent.read(
		      SUBARRAY(self.block[blockno].buffer.data^, 0, nbytes),0);
                self.block[blockno].extent.deallocate();
                self.block[blockno].extent := NIL;
              EXCEPT
              | Error.E(e) =>
		IO.PutError("BlockList: swap error - cannot read(" & e.message() & ").\n");
		EXIT;
              END;
              IF nbytes # self.block[blockno].size THEN
                IO.PutError("BlockList: short read from swap.\n");
                IO.Put("asked for " & Fmt.Int(self.block[blockno].size) &
                  " bytes, got " & Fmt.Int(nbytes) & "!\n");
              END;
              self.totsize := self.totsize + self.block[blockno].size;
              self.block[blockno].ondisk := FALSE;
            ELSE
              (* someone else already paged it in *)
              IO.PutError("BlockList: someone else paged it in, no problem.\n");
            END;
          FINALLY
            ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
          END;
        END;
      END;
    END;
    IO.PutError("BlockList: retrieve block is failing.\n");
    size := 0;
    RETURN NIL;
  END RetrieveBlock;

PROCEDURE TryRetrieveBlock(self: T;
                           blockno: BlockNo; 
                           VAR size: CARDINAL; 
                           VAR b: Block) : BOOLEAN =
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,READ) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    IF blockno > self.maxblockno THEN RETURN FALSE; END;
    IF debug THEN
      IO.Put("Trying to retrieve block " & Fmt.Int(blockno) & " from the cache.\n");
    END;
    IF self.block[blockno].ondisk = FALSE THEN
      IF ConsistencyCheck AND self.block[blockno].buffer = NIL THEN
        IO.PutError("BlockList: TryRetrieveBlock returning a buffer that is NIL\n");
      END;
      IF ReaderWriterLock.ReaderTryLock(self.block[blockno].lock) THEN
        size := self.block[blockno].size;
        b := self.block[blockno].buffer;
        RETURN TRUE;
      ELSE
        size := 0;
        RETURN FALSE;
      END;
    ELSE
      size := 0;
      RETURN FALSE;
    END;
  END TryRetrieveBlock;

PROCEDURE RetrieveData(self: T; blockno: BlockNo; block: REF ARRAY OF CHAR; VAR size: CARDINAL) =
  VAR dblock: Block;
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,READ) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    IF debug THEN
      IO.Put("Retrieving data " & Fmt.Int(blockno) & " from the cache.\n");
    END;
    dblock := RetrieveBlock(self, blockno,size);
    IF dblock # NIL THEN
      SUBARRAY(block^, FIRST(block^), NUMBER(dblock.data^)) := dblock.data^;
      UnlockBlock(self, blockno);
    END;
  END RetrieveData;

PROCEDURE UnlockBlock(self: T; blockno: BlockNo) =
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,WRITE) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    IF debug THEN
      IO.Put("Unlocking block " & Fmt.Int(blockno) & " in the cache.\n");
    END;
    IF blockno > self.maxblockno THEN RETURN; END;
    IF ConsistencyCheck AND self.block[blockno].lock = NIL THEN
      IO.PutError("BlockList: Lock is NIL in UnlockBlock\n");
    END;
    ReaderWriterLock.ReaderUnlock(self.block[blockno].lock);
  END UnlockBlock;

PROCEDURE Size(self: T) : INTEGER =
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,READ) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    RETURN self.totsize;
  END Size;

PROCEDURE SetPolicy(self: T; policy: Policy) =
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,WRITE) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    IF policy # Policy.Swap THEN
      (*
       * XXX The only implemented policy right now is a full swap
       * to secondary storage.
       *)
      IO.PutError("BlockList: unimplemented policy\n");
      RETURN;
    END;
    self.flushpolicy := policy;
  END SetPolicy;

PROCEDURE SetMaxSize(self: T; newsize: INTEGER) =
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,WRITE) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    IF debug THEN
      IO.Put("Setting a maxsize of " & Fmt.Int(newsize) & ".\n");
    END;
    self.maxsize := newsize;
    Pack(self);
  END SetMaxSize;

(*
 * Flush out the in-memory cache to disk.
 *)
PROCEDURE Pack(self: T) =
  BEGIN
    IF self.maxsize = 0 OR self.totsize < self.maxsize THEN 
      RETURN;
    END;
    PageOut(self);
  END Pack;

PROCEDURE PageOut(self: T) =
  VAR
    nbytes: CARDINAL;
  CONST
    flags = Extent.FlagSet{Extent.Flags.ExactSize};
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,WRITE) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    IF debug THEN
      IO.Put("Pageout.\n");
    END;
    IF self.swap = NIL THEN
      IO.PutError("BlockList: Pageout called with swap not set!\n");
      RETURN;
    END;
    FOR i := FIRST(self.block) TO self.maxblockno DO
      IF NOT self.block[i].ondisk THEN
        (* FIXME: this should never happen *)
        IF self.block[i].lock = NIL THEN
          IO.PutError("BlockList: NIL lock in PageOut\n");
        END;
        IF ReaderWriterLock.WriterTryLock(self.block[i].lock) THEN
          TRY
            (* write the data out to disk *)
            IF debug THEN IO.Put("Writing a block to disk.\n"); END;
            nbytes := self.block[i].size;

            TRY
              (* allocate a new extent for this block *)
              self.block[i].extent := NEW(Extent.T).init();

              (* allocate the extent on the disk.
                 NOTE that "nbytes" will be rounded up to the nearest
                 multiple the extent's blocksize. *)
              self.block[i].extent.allocate(self.devName,nbytes,flags:=flags);
              
            EXCEPT
            | Error.E(e) => 
              IO.PutError("BlockList: Allocating Swap file failed (");
              IO.Put(e.message());
              IO.Put(")\n");
              self.block[i].extent := NIL;
              RETURN; (* XXX temporary fix *)
            END;

            TRY
              WITH data = SUBARRAY(self.block[i].buffer.data^, 0, self.block[i].size) 
                DO
                 nbytes := self.block[i].extent.write(data,0);
               END;
              IF nbytes < self.block[i].size THEN
                IO.PutError("BlockList: short swap write.\n");
              END;
            EXCEPT
            | Error.E(e) => 
              IO.PutError("BlockList: Swap write failed (");
              IO.Put(e.message());
              IO.Put(")\n");
            END;
            self.block[i].ondisk := TRUE;
            self.totsize := self.totsize - self.block[i].size;
            Buffer.Deallocate(self.block[i].buffer);
            self.block[i].buffer := NIL;
          FINALLY
            ReaderWriterLock.WriterUnlock(self.block[i].lock);
          END;
        END;
      END;
    END;
  END PageOut;

PROCEDURE RegisterSwap(self: T; swap: Extent.T; devName: TEXT;) =
  BEGIN
    IF SECURITY THEN
      IF NOT SecurityManager.CheckObjectSimplePermissions(self,WRITE) THEN
        RAISE SpinException.Exception(BURP);
      END;
    END;
    self.swap := swap;
    self.devName := devName;
  END RegisterSwap;

BEGIN  
  SecurityManagerPrivate.SetTypeSecurity( TYPECODE(T), TRUE );
END BlockList.

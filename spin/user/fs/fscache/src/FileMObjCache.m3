(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)

(*
 * FileMObjCache is the container for memory objects which represent
 * cached files.
 *)
MODULE FileMObjCache;

IMPORT VMTypes, PhysAddr, VMError, File, Error, FileStat, Protection;
IMPORT ErrnoDep;
IMPORT Buffer, FileDataCache, BaseMObjCache, Victim, CacheFile;
IMPORT IO, Fmt;
IMPORT Thread, ReaderWriterLock;

TYPE BlockDesc = RECORD
  buf: Buffer.T;    (* data buffer *)
  size: INTEGER;    (* size of this block - may be incomplete if last *)
  lock: ReaderWriterLock.T;
  ondisk: BOOLEAN := TRUE;
  dirty: BOOLEAN := FALSE;
END;

TYPE State = { AccessAllowed, NoAccessAllowed };

REVEAL 
  T = Public BRANDED OBJECT
    state: State := State.NoAccessAllowed;
    stateMu: MUTEX;
    stateCond: Thread.Condition;
    accessCount: INTEGER := 0;
    size: VMTypes.PageCount;       (* capacity number of pages *)
    cursize: VMTypes.PageCount;    (* current number of pages resident *)
    block: REF ARRAY OF BlockDesc; (* container for page entries *)
    totsize: INTEGER;              (* data cached in bytes *)
    fsize: INTEGER;                (* size of file in bytes *)
    file: CacheFile.T;             (* file which backs this obj *)
    swapout: FileDataCache.SwapOutProc;
    swapin: FileDataCache.SwapInProc;
    lock: MUTEX;                   (* for cache internal data *)
    baseCache: BaseMObjCache.T;    (* base mem obj cache for this file cache *)
    (* stats *)
    hits: INTEGER;   (* hits in this file *)
    misses: INTEGER; (* misses in this file *)
    steals: INTEGER; (* blocks this file has stolen from other files *)
    stolen: INTEGER; (* blocks this file has had stolen by other files *)
  OVERRIDES
    (* memory object cache methods *)
    lookup := Lookup;
    update := Update;
    invalidate := Invalidate;
    chooseVictim := ChooseVictim;
    frameCount := FrameCount;
    print := Print;
    (* new methods *)
    init := Init;
    writeBlock := WriteBlock;
    tryWriteBlock := TryWriteBlock;
    retrieveData := RetrieveData;
    tryRetrieveData := TryRetrieveData;
    releaseBlock := ReleaseBlock;
    tryReleaseBlock := TryReleaseBlock;
    flush := Flush;
    filesize := FileSize;
    dealloc := Dealloc;
    stat := Stat;
    id := Id;
  END;

CONST KeepStats = TRUE;

PROCEDURE Lookup(self: T; pagenum: VMTypes.PageNumber; 
  <*UNUSED*>type: INTEGER; VAR frame: PhysAddr.T; VAR prot: Protection.T) 
  : BOOLEAN =
  BEGIN
    frame := NIL;
    TRY
      StartAccess(self);
    EXCEPT
    | Error.E(ec) => <*NOWARN*>
      RETURN FALSE;
    END;

    IF pagenum >= 0 AND pagenum < self.size 
      AND self.block[pagenum].buf # NIL THEN
      frame := self.block[pagenum].buf.data;
      (* XXX - do not currently maintain protection! *)
      prot := Protection.All;
    END;

    EndAccess(self);
    RETURN frame # NIL;
  END Lookup;

PROCEDURE Update(self: T; pagenum: VMTypes.PageNumber; frame: PhysAddr.T) =
  BEGIN
    IO.Put("FileMObjCache.Update called but not implemented.\n");
    TRY
      StartAccess(self);
    EXCEPT
    | Error.E(ec) => <*NOWARN*>
      RETURN;
    END;
    <*ASSERT pagenum >= 0 AND pagenum < self.size*>

    (* discard any current frame at this location *)
    (* update any metadata??? *)
    ReaderWriterLock.WriterLock(self.block[pagenum].lock);
    self.block[pagenum].buf.data := frame;
    ReaderWriterLock.WriterUnlock(self.block[pagenum].lock);

    EndAccess(self);
  END Update;

PROCEDURE Invalidate(self: T; pagenum: VMTypes.PageNumber) RAISES {VMError.E} =
  BEGIN
    IO.Put("FileMObjCache.Invalidate called.  Why???\n");
    TRY
      StartAccess(self);
    EXCEPT
    | Error.E(ec) => <*NOWARN*>
      RETURN;
    END;

    IF pagenum < 0 OR pagenum >= self.size OR 
      self.block[pagenum].buf = NIL THEN
      RAISE VMError.E(VMError.CachePageNotFound);
    ELSE
      ReaderWriterLock.WriterLock(self.block[pagenum].lock);
      self.block[pagenum].buf := NIL;
      self.block[pagenum].ondisk := TRUE;
      self.block[pagenum].dirty := FALSE;
      ReaderWriterLock.WriterUnlock(self.block[pagenum].lock);
    END;

    EndAccess(self);
  END Invalidate;

PROCEDURE ChooseVictim(<*UNUSED*>self: T; <*UNUSED*>VAR frame: PhysAddr.T) =
  BEGIN
    (* call central cache to get the next page to be evicted *)
    IO.Put("FileMObjCache.ChooseVictim called.  Why???\n");
  END ChooseVictim;

PROCEDURE FrameCount(self: T) : CARDINAL =
  BEGIN
    LOCK self.lock DO
      RETURN self.cursize;
    END;
  END FrameCount;

PROCEDURE Print(self: T): TEXT =
  VAR
    string: TEXT;
  BEGIN
    LOCK self.lock DO
      string := "FileMObjCache("&Fmt.Int(self.size)&"blocks)\n";
    END;
    RETURN string;
  END Print;

PROCEDURE Init(self: T; file: CacheFile.T; 
  swapout: FileDataCache.SwapOutProc; swapin: FileDataCache.SwapInProc;
  baseCache: BaseMObjCache.T; 
  blkcount: VMTypes.PageCount := DefaultCacheFileSize) : T RAISES { Error.E } =
  VAR
    fstat: FileStat.T;
  BEGIN
    self.stateMu := NEW(MUTEX);
    self.stateCond := NEW(Thread.Condition);
    ProhibitAccess(self);
    self.cursize := 0;
    IF blkcount > MaxBlockNo THEN
      IO.Put("FileMObjCache.Init: reducing file size " & Fmt.Int(blkcount) &
        " to " & Fmt.Int(MaxBlockNo) & "\n");
      blkcount := MaxBlockNo;
    END;
    (* determine the size of the file *)
    TRY
      file.stat(fstat);
      self.fsize := fstat.size;
    EXCEPT
    | Error.E(ec) =>
      IO.Put("FileDataCache.AddFile: stat failed.\n");
      RAISE Error.E(ec);
    END;
    self.size := blkcount;
    self.block := NEW(REF ARRAY OF BlockDesc, blkcount);
    self.lock := NEW(MUTEX);
    self.totsize := 0;
    self.swapin := swapin;
    self.swapout := swapout;
    self.file := file;
    self.baseCache := baseCache;
    self.hits := 0;
    self.misses := 0;
    FOR i := FIRST(self.block^) TO LAST(self.block^) DO
      (* these initializations are probably overkill *)
      WITH block = self.block[i] DO
        block.buf := NIL;
        (* it would be nice to lazily allocate rwlocks *)
        block.lock := ReaderWriterLock.Allocate();
        block.ondisk := TRUE;
        block.dirty := FALSE;
      END;
    END;
    AllowAccess(self);
    RETURN self;
  END Init;

PROCEDURE WriteBlockInternal(self: T; blockno: BlockNo; reloffset: BlockOff;
  READONLY data: ARRAY OF CHAR; wait: BOOLEAN := TRUE) : INTEGER
  RAISES { Error.E } =
  VAR
    sizediff, fsizediff: INTEGER;
    hit: BOOLEAN := TRUE;

  PROCEDURE Callback(VAR page: PhysAddr.Content) =
    BEGIN
      SUBARRAY(page, reloffset, NUMBER(data)) := data;
    END Callback;

  BEGIN
    (* sanity check the arguments *)
    IF blockno > LAST(self.block^) THEN
      (* ideally, expand the self.block array *)
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.EFBIG));
    END;
    IF reloffset + NUMBER(data) > Buffer.BlockSize THEN
      IO.Put("FileMObjCache.WriteBlock: bad parameters: reloffset = " &
        Fmt.Int(reloffset) & " datalen = " & Fmt.Int(NUMBER(data)) & 
        " blocksize = " & Fmt.Int(Buffer.BlockSize) & "\n");
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.EINVAL));
    END;

    (* writelock the block. only block if wait is true *)
    IF NOT ReaderWriterLock.WriterTryLock(self.block[blockno].lock) THEN
      IF NOT wait THEN
        RAISE Error.E(NEW(Error.T).init(ErrnoDep.EWOULDBLOCK));
      ELSE
        ReaderWriterLock.WriterLock(self.block[blockno].lock);
      END;
    END;

    (* see if the block must be fetched from the FS *)
    IF self.block[blockno].ondisk THEN
      hit := FALSE;
      TRY
        FetchBlock(self, blockno);
      EXCEPT
      | Error.E(ec) =>
        ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
        RAISE Error.E(ec);
      END;
    END;

    (* write the data into the cache block *)
    TRY
      PhysAddr.Access(self.block[blockno].buf.data, Callback);
    EXCEPT
    | VMError.E(ec) =>
      IO.Put("FileMObjCache.WriteBlock: unable to access block " &
        Fmt.Int(blockno) & ": " & VMError.Message(ec) & "\n");
      ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.ENOMEM));
    END;

    (* update cache block metadata *)
    sizediff := reloffset + NUMBER(data) - self.block[blockno].size;
    IF sizediff > 0 THEN
      INC(self.block[blockno].size, sizediff);
    END;
    fsizediff := blockno * Buffer.BlockSize + reloffset + NUMBER(data)
                 - self.fsize;
    self.block[blockno].dirty := TRUE;

    (* register reference to this block *)
    Victim.Ref(self.baseCache, self, blockno, self.block[blockno].buf, 
               Victim.RefType.Write);

    (* release lock *)
    ReaderWriterLock.WriterUnlock(self.block[blockno].lock);

    (* update file metadata *)
    IF KeepStats OR sizediff > 0 OR fsizediff > 0 THEN
      LOCK self.lock DO
        IF sizediff > 0 THEN
          INC(self.totsize, sizediff);
        END;
        IF fsizediff > 0 THEN
          INC(self.fsize, fsizediff);
        END;
        IF hit THEN
          INC(self.hits);
        ELSE
          INC(self.misses);
        END;
      END;
    END;

    RETURN NUMBER(data);
  END WriteBlockInternal;

PROCEDURE WriteBlock(self: T; blockno: BlockNo; reloffset: BlockOff;
  READONLY data: ARRAY OF CHAR) : INTEGER RAISES { Error.E } =
  VAR ret: INTEGER;
  BEGIN
    StartAccess(self);
    TRY
      ret := WriteBlockInternal(self, blockno, reloffset, data, TRUE);
    FINALLY
      EndAccess(self);
    END;
    RETURN ret;
  END WriteBlock;

PROCEDURE TryWriteBlock(self: T; blockno: BlockNo; reloffset: BlockOff;
  READONLY data: ARRAY OF CHAR) : INTEGER RAISES { Error.E } =
  VAR ret: INTEGER;
  BEGIN
    StartAccess(self);
    TRY
      ret := WriteBlockInternal(self, blockno, reloffset, data, FALSE);
    FINALLY
      EndAccess(self);
    END;
    RETURN ret;
  END TryWriteBlock;

PROCEDURE RetrieveDataInternal(self: T; blockno: BlockNo; reloffset: BlockOff;
  VAR data: ARRAY OF CHAR; wait: BOOLEAN := TRUE) : INTEGER 
  RAISES { Error.E } =
  VAR
    readsize: INTEGER;
    lockiswriter: BOOLEAN;
    hit: BOOLEAN := TRUE;

  PROCEDURE Callback(VAR page: PhysAddr.Content) =
    BEGIN
      SUBARRAY(data, 0, readsize) := SUBARRAY(page, reloffset, readsize);
    END Callback;

  BEGIN
    (* readlock the block. only block if wait is true *)
    IF NOT ReaderWriterLock.ReaderTryLock(self.block[blockno].lock) THEN
      IF NOT wait THEN
        RAISE Error.E(NEW(Error.T).init(ErrnoDep.EWOULDBLOCK));
      ELSE
        ReaderWriterLock.ReaderLock(self.block[blockno].lock);
      END;
    END;
    lockiswriter := FALSE;

    (* see if the block must be fetched from the FS *)
    IF self.block[blockno].ondisk THEN
      hit := FALSE;
      (* check if waiting is an option *)
      IF NOT wait THEN
        ReaderWriterLock.ReaderUnlock(self.block[blockno].lock);
        RAISE Error.E(NEW(Error.T).init(ErrnoDep.EWOULDBLOCK));
      END;
      (* must obtain a writerlock in order to get the block from disk.
         this would be easier if readerlocks could be upgraded to
         writerlocks, but we will go the long way for now. *)
      ReaderWriterLock.ReaderUnlock(self.block[blockno].lock);
      ReaderWriterLock.WriterLock(self.block[blockno].lock);
      (* must make sure block was not fetched in the meantime *)
      IF self.block[blockno].ondisk THEN
        TRY
          FetchBlock(self, blockno);
        EXCEPT
        | Error.E(ec) =>
          ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
          RAISE Error.E(ec);
        END;
      END;
      (* to be unselfish, the writerlock should really be downgraded to
         a readerlock, but for now skip it. *)
      lockiswriter := TRUE;
    END;

    (* sanity check the arguments *)
    IF reloffset < 0 OR reloffset > self.block[blockno].size THEN
      IO.Put("FileMObjCache.WriteBlock: bad parameters: reloffset = " &
        Fmt.Int(reloffset) & " blocksize = " & 
        Fmt.Int(self.block[blockno].size) & "\n");
      IF lockiswriter THEN
        ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
      ELSE
        ReaderWriterLock.ReaderUnlock(self.block[blockno].lock);
      END;
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.EINVAL));
    END;

    (* read the data from the cache block *)
    readsize := MIN(NUMBER(data), self.block[blockno].size - reloffset);
    IF readsize > 0 THEN
      TRY
        PhysAddr.Access(self.block[blockno].buf.data, Callback);
      EXCEPT
      | VMError.E(ec) =>
        IO.Put("FileMObjCache.WriteBlock: unable to access block " &
          Fmt.Int(blockno) & ": " & VMError.Message(ec) & "\n");
        IF lockiswriter THEN
          ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
        ELSE
          ReaderWriterLock.ReaderUnlock(self.block[blockno].lock);
        END;
        RAISE Error.E(NEW(Error.T).init(ErrnoDep.ENOMEM));
      END;
    END;

    (* register reference to this block *)
    Victim.Ref(self.baseCache, self, blockno, self.block[blockno].buf,
               Victim.RefType.Read);

    (* release lock *)
    IF lockiswriter THEN
      ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
    ELSE
      ReaderWriterLock.ReaderUnlock(self.block[blockno].lock);
    END;

    (* update file metadata *)
    IF KeepStats THEN
      LOCK self.lock DO
        IF hit THEN
          INC(self.hits);
        ELSE
          INC(self.misses);
        END;
      END;
    END;

    RETURN readsize;
  END RetrieveDataInternal;

PROCEDURE RetrieveData(self: T; blockno: BlockNo; reloffset: BlockOff;
  VAR data: ARRAY OF CHAR) : INTEGER RAISES { Error.E } =
  VAR ret: INTEGER;
  BEGIN
    StartAccess(self);
    TRY
      ret := RetrieveDataInternal(self, blockno, reloffset, data, TRUE);
    FINALLY
      EndAccess(self);
    END;
    RETURN ret;
  END RetrieveData;

PROCEDURE TryRetrieveData(self: T; blockno: BlockNo; reloffset: BlockOff;
  VAR data: ARRAY OF CHAR) : INTEGER RAISES { Error.E } =
  VAR ret: INTEGER;
  BEGIN
    StartAccess(self);
    TRY
      ret := RetrieveDataInternal(self, blockno, reloffset, data, FALSE);
    FINALLY
      EndAccess(self);
    END;
    RETURN ret;
  END TryRetrieveData;

PROCEDURE ReleaseBlockInternal(self: T; blockno: BlockNo; 
  wait: BOOLEAN := TRUE) : Buffer.T RAISES { Error.E } =
  VAR
    offset: File.OffsetT;
    nwritten: INTEGER;
    buf: Buffer.T;

  PROCEDURE Callback(VAR page: PhysAddr.Content) =
    BEGIN
      (* propogate exceptions to caller *)
      <*NOWARN*>nwritten := self.swapout(self.file, offset, 
                               SUBARRAY(page, 0, self.block[blockno].size));
    END Callback;

  BEGIN
    (* writelock the block. only block if wait is true *)
    IF NOT ReaderWriterLock.WriterTryLock(self.block[blockno].lock) THEN
      IF NOT wait THEN
        RAISE Error.E(NEW(Error.T).init(ErrnoDep.EWOULDBLOCK));
      ELSE
        ReaderWriterLock.WriterLock(self.block[blockno].lock);
      END;
    END;

    (* make sure the block has a frame allocated to it *)
    IF self.block[blockno].ondisk OR self.block[blockno].buf = NIL THEN
      IO.Put("FileMObjCache.ReleaseBlock: block " & Fmt.Int(blockno) &
        " has no physical page.\n");
      ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.ENOBUFS));
    END;

    (* if the block is dirty, write it out using the swapout procedure *)
    IF self.block[blockno].dirty THEN
      IF NOT wait THEN
        ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
        RAISE Error.E(NEW(Error.T).init(ErrnoDep.EWOULDBLOCK));
      END;
      offset := blockno * Buffer.BlockSize;
      TRY
        PhysAddr.Access(self.block[blockno].buf.data, Callback);
      EXCEPT
      | VMError.E(ec) => (* raised by PhysAddr.Access *)
        IO.Put("FileMObjCache.ReleaseBlock: unable to access PhysAddr " &
          "for block " & Fmt.Int(blockno) & ": " & VMError.Message(ec) & "\n");
        ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
        (* XXX - what to do?  for now, do not return the buffer *)
        RAISE Error.E(NEW(Error.T).init(ErrnoDep.ENOMEM));
      | Error.E(ec) => (* raised by FS through Callback *) <*NOWARN*>
        IO.Put("FileMObjCache.ReleaseBlock: unable to write block " & 
          Fmt.Int(blockno) & " to storage: " & ec.message() & "\n");
        ReaderWriterLock.WriterUnlock(self.block[blockno].lock);
        (* XXX - what to do?  for now, do not return the buffer.  this may
           actually succeed later if the disk is temporarily off-line now *)
        RAISE Error.E(ec);
      END;
    ELSE
      nwritten := self.block[blockno].size;
    END;

    (* update block metadata *)
    self.block[blockno].ondisk := TRUE;

    (* transfer buf *)
    buf := self.block[blockno].buf;
    self.block[blockno].buf := NIL;

    (* unlock block *)
    ReaderWriterLock.WriterUnlock(self.block[blockno].lock);

    (* update file metadata *)
    (* XXX could grabbing this lock lead to deadlock? it should not if
       nobody grabs a block rwlock while holding the file lock *)
    LOCK self.lock DO
      DEC(self.totsize, nwritten);  (* total number of bytes cached *)
      DEC(self.cursize, 1);     (* current number of pages for this file *)
      IF KeepStats THEN
        INC(self.stolen, 1);
      END;
    END;

    RETURN buf;
  END ReleaseBlockInternal;

PROCEDURE ReleaseBlock(self: T; blockno: BlockNo) : Buffer.T 
  RAISES { Error.E } =
  VAR buf: Buffer.T;
  BEGIN
    StartAccess(self);
    TRY
      buf := ReleaseBlockInternal(self, blockno, TRUE);
    FINALLY
      EndAccess(self);
    END;
    RETURN buf;
  END ReleaseBlock;

PROCEDURE TryReleaseBlock(self: T; blockno: BlockNo) : Buffer.T 
  RAISES { Error.E } =
  VAR buf: Buffer.T;
  BEGIN
    StartAccess(self);
    TRY
      buf := ReleaseBlockInternal(self, blockno, FALSE);
    FINALLY
      EndAccess(self);
    END;
    RETURN buf;
  END TryReleaseBlock;

(* The caller of FetchBlock must hold the writer lock for this block *)
PROCEDURE FetchBlock(self: T; blockno: BlockNo) RAISES { Error.E } =
  VAR
    buf: Buffer.T;
    offset: File.OffsetT;
    nread: INTEGER;

  PROCEDURE Callback(VAR page: PhysAddr.Content) =
    BEGIN
      (* propogate exceptions to caller *)
      <*NOWARN*>nread := self.swapin(self.file, offset, page);
    END Callback;

  BEGIN
    (* some sanity checks *)
    IF self.block[blockno].buf # NIL THEN
      IO.Put("FileMObjCache.FetchBlock: buffer exists for block " &
        Fmt.Int(blockno) & "\n");
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.EINVAL));
    END;

    (* verify that locks are held *)
    IF ReaderWriterLock.ReaderTryLock(self.block[blockno].lock) THEN
      IO.Put("FileMObjCache.FetchBlock: writelock not held for block " &
        Fmt.Int(blockno) & "\n");
      ReaderWriterLock.ReaderUnlock(self.block[blockno].lock);
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.ENOLCK));
    END;

    (* get a buffer to read data into.  first try the free pool in the
       base cache.  if none available, get a victim. *)
    buf := self.baseCache.getBuffer(self, blockno);
    IF buf = NIL THEN
      buf := Victim.StealBuffer(self.baseCache, self, blockno);
      IF buf = NIL THEN
        IO.Put("FileMObjCache.FetchBlock: victim not available for block " &
          Fmt.Int(blockno) & "\n");
        RAISE Error.E(NEW(Error.T).init(ErrnoDep.ENOBUFS));
      ELSE
        IF KeepStats THEN
          LOCK self.lock DO
            INC(self.steals);
          END;
        END;
      END;
    END;
    self.block[blockno].buf := buf;

    (* read in the block, using the swapin procedure *)
    offset := blockno * Buffer.BlockSize;
    TRY
      PhysAddr.Access(self.block[blockno].buf.data, Callback);
    EXCEPT
    | VMError.E(ec) => (* raised by PhysAddr.Access *)
      IO.Put("FileMObjCache.FetchBlock: unable to access PhysAddr " &
        "for block " & Fmt.Int(blockno) & ": " & VMError.Message(ec) & "\n");
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.ENOMEM));
    | Error.E(ec) => (* raised by FS through Callback *) <*NOWARN*>
      IO.Put("FileMObjCache.FetchBlock: unable to read block " & 
        Fmt.Int(blockno) & " from storage: " & ec.message() & "\n");
      RAISE Error.E(ec);
    END;

    (* update block metadata *)
    self.block[blockno].ondisk := FALSE;
    self.block[blockno].size := nread;
    self.block[blockno].dirty := FALSE; (* should not be necessary to set *)

    (* update file metadata *)
    (* XXX could grabbing this lock lead to deadlock? it should not if
       nobody grabs a block rwlock while holding the file lock *)
    LOCK self.lock DO
      INC(self.totsize, nread);  (* total number of bytes cached *)
      INC(self.cursize, 1);      (* current number of pages for this file *)
    END;
  END FetchBlock;

PROCEDURE Flush(self: T; startblock: BlockNo := 0; numblocks: INTEGER := -1) 
  : CARDINAL RAISES { Error.E } =
  VAR
    nbytes, nwritten: INTEGER;
    offset: File.OffsetT;
    lastblock: BlockNo;
    blockno: BlockNo;

  PROCEDURE Callback(VAR page: PhysAddr.Content) =
    BEGIN
      (* propogate exceptions to caller *)
      <*NOWARN*>nwritten := self.swapout(self.file, offset, 
                               SUBARRAY(page, 0, self.block[blockno].size));
    END Callback;

  BEGIN
    StartAccess(self);

    IF startblock > LAST(self.block^) THEN
      startblock := FIRST(self.block^);
      lastblock := LAST(self.block^);
    END;
    IF numblocks = -1 OR startblock + numblocks - 1 > LAST(self.block^) THEN
      lastblock := LAST(self.block^);
    ELSE
      lastblock := startblock + numblocks - 1;
    END;
      
    (* iterate through the specified blocks *)
    (* XXX assumes that size of self.block array does not change for 
       duration of call *)
    blockno := startblock;
    nbytes := 0;
    WHILE blockno <= lastblock DO
      (* can quit if we are past the end of the file *)
      IF blockno * Buffer.BlockSize > self.fsize THEN
        EXIT;
      END;

      (* lock the block - actually only need a readlock *)
      ReaderWriterLock.ReaderLock(self.block[blockno].lock);

      (* if the block is dirty, write it out using the swapout procedure *)
      IF self.block[blockno].dirty THEN
        offset := blockno * Buffer.BlockSize;
        nwritten := 0;
        TRY
          PhysAddr.Access(self.block[blockno].buf.data, Callback);
        EXCEPT
        | VMError.E(ec) => (* raised by PhysAddr.Access *)
          IO.Put("FileMObjCache.Flush: unable to access PhysAddr " &
            "for block " & Fmt.Int(blockno) & ": " & 
            VMError.Message(ec) & "\n");
        | Error.E(ec) => (* raised by FS through Callback *) <*NOWARN*>
          IO.Put("FileMObjCache.Flush: unable to write block " & 
            Fmt.Int(blockno) & " to storage: " & ec.message() & "\n");
        END;

        (* update block metadata *)
        (* XXX possible race condition since only reader lock is held
           and there can be multiple readers *)
        self.block[blockno].dirty := FALSE;

        (* keep track of total bytes written *)
        INC(nbytes, nwritten);
      END;
    
      (* release lock *)
      ReaderWriterLock.ReaderUnlock(self.block[blockno].lock);

      (* move onto next block *)
      INC(blockno);
    END;

    EndAccess(self);

    RETURN nbytes;
  END Flush;

PROCEDURE FileSize(self: T) : INTEGER =
  BEGIN
    LOCK self.lock DO
      RETURN self.size;
    END;
  END FileSize;

PROCEDURE Dealloc(self: T; flush: BOOLEAN := TRUE) : INTEGER 
  RAISES { Error.E }=
  VAR
    nwritten: CARDINAL;
  BEGIN
    ProhibitAccess(self);

    IF flush THEN
      nwritten := self.flush();
    END;

    (* iterate through all blocks, deallocating and returning
       buffers as necessary *)
    FOR i := FIRST(self.block^) TO LAST(self.block^) DO
      self.baseCache.freeBuffer(self.block[i].buf, self, i);
      self.block[i].buf := NIL;
      self.block[i].lock := NIL;
    END;
    self.block := NIL;
    self.baseCache := NIL;

    RETURN nwritten;
  END Dealloc;

PROCEDURE StartAccess(self: T) RAISES { Error.E } =
  BEGIN
    LOCK self.stateMu DO
      IF self.state = State.NoAccessAllowed THEN
        RAISE Error.E(NEW(Error.T).init(ErrnoDep.EDEADLK));
      END;
      INC(self.accessCount);
    END;
  END StartAccess;

PROCEDURE EndAccess(self: T) =
  BEGIN
    LOCK self.stateMu DO
      DEC(self.accessCount);
      IF self.state = State.NoAccessAllowed AND self.accessCount = 0 THEN
        Thread.Signal(self.stateCond);
      END;
    END;
  END EndAccess;

PROCEDURE ProhibitAccess(self: T) =
  BEGIN
    LOCK self.stateMu DO
      self.state := State.NoAccessAllowed;
      IF self.accessCount > 0 THEN
        Thread.Wait(self.stateMu, self.stateCond);
      END;
    END;
  END ProhibitAccess;

PROCEDURE AllowAccess(self: T) =
  BEGIN
    LOCK self.stateMu DO
      self.state := State.AccessAllowed;
    END;
  END AllowAccess;

PROCEDURE Stat(self: T; VAR capacity: VMTypes.PageCount; 
  VAR occupied: VMTypes.PageCount; VAR hits: INTEGER; 
  VAR misses: INTEGER; VAR steals: INTEGER; VAR stolen: INTEGER; 
  VAR id: INTEGER) =
  BEGIN
    id := self.file.hash();
    IF KeepStats THEN
      LOCK self.lock DO
        capacity := self.size;
        occupied := self.cursize;
        hits := self.hits;
        misses := self.misses;
        steals := self.steals;
        stolen := self.stolen;
      END;
    ELSE
      IO.Put("FileMObjCache.Stat: stats disabled in file id " & 
        Fmt.Int(id) & "\n");
    END;
  END Stat;

PROCEDURE Id(self: T) : INTEGER =
  BEGIN
    RETURN self.file.hash();
  END Id;

BEGIN
END FileMObjCache.

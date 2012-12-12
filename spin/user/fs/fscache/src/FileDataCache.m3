(*
 *
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)

MODULE FileDataCache;

IMPORT CacheFile, FileMObjCache, Buffer, BaseMObjCache, Victim;
IMPORT IO, Fmt, Error;
IMPORT File, FileStat, VMTypes, VMError;
IMPORT FileMObjCacheTbl;
IMPORT FileDataCacheInterface;

REVEAL T = BRANDED OBJECT
  filetable: FileMObjCacheTbl.Default;
  filetablelock: MUTEX;
  baseCache: BaseMObjCache.T;
END;

VAR
  centralCache: T;

PROCEDURE Create(numpages: VMTypes.PageCount := DefaultCacheSize) : T =
  VAR newCache: T;
  BEGIN
    newCache := NEW(T);
    newCache.filetable := NEW(FileMObjCacheTbl.Default).init();
    newCache.filetablelock := NEW(MUTEX);
    (* XXX - rather than creating memory object here, only create 
       memory object cache since we are not yet paging *)
    TRY
      newCache.baseCache := NEW(BaseMObjCache.T).init(numpages, 
                                                      "Central Cache");
    EXCEPT
    | VMError.E(ec) =>
      IO.Put("FileDataCache.Create: unable to allocate base cache: " &
        VMError.Message(ec) & "\n");
      newCache := NIL;
    END;
    RETURN newCache;
  END Create;

PROCEDURE AddFile(cache: T; file: CacheFile.T; swapout: SwapOutProc; 
  swapin: SwapInProc; startpre: INTEGER := 0;  numpre: INTEGER := -1) 
  RAISES {Error.E,CacheError} =
  VAR
    fcache: FileMObjCache.T;
    fstat: FileStat.T;
    fsize: CARDINAL;
    nblocks: CARDINAL;
    error: Error.T := NIL;
    dummybuf: ARRAY [0..0] OF CHAR;
  BEGIN
    (* first make sure this file is not already cached *)
    LOCK cache.filetablelock DO
      IF cache.filetable.get(file, fcache) THEN
        RAISE CacheError(EC.FileAlreadyInCache);
      END;
    END;

    (* determine the size of the file from the file system *)
    TRY
      file.stat(fstat);
      fsize := fstat.size;
    EXCEPT
    | Error.E(ec) =>
      IO.Put("FileDataCache.AddFile: stat failed.\n");
      RAISE Error.E(ec);
    END;
    nblocks := RoundToCacheBlock(fsize);

    (* XXX - this means that until FileMObjCaches are dynamically expandable,
       files greater than the default size cannot grow! *)
    WITH initblocks = MAX(RoundToCacheBlock(fsize), 
                          FileMObjCache.DefaultCacheFileSize) DO
      (* create a memory object for this file *)
      (* XXX - for now, just create the memory object cache *)
      TRY
        fcache := NEW(FileMObjCache.T).init(file, swapout, swapin, 
                                            cache.baseCache, initblocks);
      EXCEPT
      | Error.E(ec) =>
        IO.Put("FileDataCache.AddFile: file mem obj cache init failed.\n");
        RAISE Error.E(ec);
      END;
    END;

    (* determine the block at which to start prefetching.  if it is
       invalid, i.e. not within the file, use 0 *)
    IF startpre < 0 OR nblocks < startpre THEN
      startpre := 0;
    END;

    (* prefetch as many blocks as specified - if -1, prefetch entire file *)
    IF numpre = -1 THEN
      numpre := nblocks;
    END;

    FOR blockno := startpre TO startpre + nblocks - 1 DO
      TRY
        EVAL fcache.retrieveData(blockno, 0, dummybuf);
      EXCEPT
      | Error.E(ec) =>
        IO.Put("FileDataCache.AddFile: prefetch of block " &
          Fmt.Int(blockno) & " failed.\n");
        error := ec;
      END;
    END;

    (* cache the blocklist *)
    LOCK cache.filetablelock DO
      IF cache.filetable.put(file, fcache) THEN
        RAISE CacheError(EC.FileAlreadyInCache);
      END;
    END;

    IF error # NIL THEN
      RAISE Error.E(error);
    END;
  END AddFile;

PROCEDURE FileIsCached(cache: T; file: CacheFile.T) : BOOLEAN =
  VAR fcache: FileMObjCache.T;
  BEGIN
    LOCK cache.filetablelock DO
      RETURN cache.filetable.get(file, fcache);
    END;
  END FileIsCached;

PROCEDURE Read(cache: T; file: CacheFile.T; offset: CARDINAL;
  VAR data: ARRAY OF CHAR) : CARDINAL RAISES {Error.E,CacheError} =
  VAR
    fcache: FileMObjCache.T;
    blockno: FileMObjCache.BlockNo;
    bytes, rem, nread, totalread, bufoffset, blockoffset: INTEGER;
  BEGIN
    LOCK cache.filetablelock DO
      IF NOT cache.filetable.get(file, fcache) THEN
        RAISE CacheError(EC.FileNotInCache);
      END;
    END;

    bytes := NUMBER(data);
    bufoffset := 0;
    totalread := 0;
    WHILE bytes > 0 DO
      blockno := offset DIV BlockSize;
      blockoffset := offset MOD BlockSize;
      rem := MIN(bytes, BlockSize - blockoffset);
      TRY
        nread := fcache.retrieveData(blockno, blockoffset,
                                     SUBARRAY(data, bufoffset, rem));
      EXCEPT
      | Error.E(ec) =>
        IO.Put("FileDataCache.Read: exception reading block " &
          Fmt.Int(blockno) & " block offset " & Fmt.Int(blockoffset) &
          " size " & Fmt.Int(rem) & ": " & ec.message() & "\n");
        RAISE Error.E(ec);
      END;
      INC(totalread, nread);
      IF nread < rem THEN
        (* got less than expected - must be incomplete block and EOF *)
        EXIT;
      ELSE
        INC(offset, rem);
        INC(bufoffset, rem);
        DEC(bytes, rem);
      END;
    END;

    RETURN totalread;
  END Read;

PROCEDURE Write(cache: T; file: CacheFile.T; offset: CARDINAL;
  READONLY data: ARRAY OF CHAR) : CARDINAL RAISES {Error.E,CacheError} =
  VAR
    fcache: FileMObjCache.T;
    blockno: FileMObjCache.BlockNo;
    bytes, rem, nwritten, totalwritten, bufoffset, blockoffset: INTEGER;
    fsize: INTEGER;
  BEGIN
    LOCK cache.filetablelock DO
      IF NOT cache.filetable.get(file, fcache) THEN
        RAISE CacheError(EC.FileNotInCache);
      END;
    END;

    (* determine where this write is in relation to the file. if it starts
       past EOF, must fill intermediate space with zeros *)
    fsize := fcache.filesize();
    IF offset > fsize THEN
      ZeroFill(fcache, offset, fsize);
    END;

    bytes := NUMBER(data);
    bufoffset := 0;
    totalwritten := 0;
    WHILE bytes > 0 DO
      blockno := offset DIV BlockSize;
      blockoffset := offset MOD BlockSize;
      rem := MIN(bytes, BlockSize - blockoffset);
      TRY
        nwritten := fcache.writeBlock(blockno, blockoffset,
                                      SUBARRAY(data, bufoffset, rem));
      EXCEPT
      | Error.E(ec) =>
        IO.Put("FileDataCache.Write: exception writing block " &
          Fmt.Int(blockno) & " block offset " & Fmt.Int(blockoffset) &
          " size " & Fmt.Int(rem) & ": " & ec.message() & "\n");
        RAISE Error.E(ec);
      END;
      INC(totalwritten, nwritten);
      INC(offset, rem);
      INC(bufoffset, rem);
      DEC(bytes, rem);
    END;

    RETURN totalwritten;
  END Write;

(* zero file contents from start to finish.  rely on some file
   system locking - the entire file is not locked down for this operation,
   only individual cache blocks as they are written.  also, this 
   procedure will cause all affected blocks to be brought into the cache. *)
PROCEDURE ZeroFill(fcache: FileMObjCache.T; start, finish: File.OffsetT) 
  RAISES { Error.E } =
  VAR
    bytes, rem, offset, blockoffset: INTEGER;
    blockno: FileMObjCache.BlockNo;
    zerobuf: ARRAY [0..BlockSize-1] OF CHAR;
  BEGIN
    (* initialize the zero buf - there must be a more efficient way *)
    WITH zerobufInt = 
         VIEW(zerobuf, ARRAY [0..BlockSize DIV BYTESIZE(INTEGER)-1] OF INTEGER)
     DO
      FOR i := FIRST(zerobufInt) TO LAST(zerobufInt) DO
        zerobufInt[i] := 0;
      END;
    END;
    bytes := finish - start;
    offset := start;
    WHILE bytes > 0 DO
      blockno := offset DIV BlockSize;
      blockoffset := offset MOD BlockSize;
      rem := MIN(bytes, BlockSize - blockoffset);
      (* propogate exceptions out *)
      EVAL fcache.writeBlock(blockno, blockoffset, 
                             SUBARRAY(zerobuf, 0, rem));
      INC(offset, rem);
      DEC(bytes, rem);
    END;
  END ZeroFill;

PROCEDURE Flush(cache: T; file: CacheFile.T := NIL; 
  blockno: INTEGER := 0; numblocks: INTEGER := -1) : CARDINAL 
  RAISES {Error.E,CacheError} =
  VAR
    fcache: FileMObjCache.T;
    it: FileMObjCacheTbl.Iterator;
    nwritten: INTEGER := 0;
    totalwritten: INTEGER := 0;
  BEGIN
    IF file = NIL THEN
      LOCK cache.filetablelock DO
        (* flush all files in cache *)
        it := cache.filetable.iterate();
        WHILE it.next(file, fcache) DO
          nwritten := fcache.flush();
          INC(totalwritten, nwritten);
        END;
      END;
    ELSE
      LOCK cache.filetablelock DO
        IF NOT cache.filetable.get(file, fcache) THEN
          RAISE CacheError(EC.FileNotInCache);
        END;
      END;
      totalwritten := fcache.flush(blockno, numblocks);
    END;

    RETURN totalwritten;
  END Flush;
    
PROCEDURE Delete(cache: T; file: CacheFile.T := NIL; flush: BOOLEAN) 
  : CARDINAL RAISES {Error.E,CacheError} =
  VAR
    fcache: FileMObjCache.T;
    it: FileMObjCacheTbl.Iterator;
    nwritten: INTEGER;
    totalwritten: INTEGER := 0;
  BEGIN
    IF file = NIL THEN
      (* delete all files in cache *)
      LOCK cache.filetablelock DO
        LOOP
          it := cache.filetable.iterate();
          IF it.next(file, fcache) THEN
            nwritten := fcache.dealloc(flush);
            INC(totalwritten, nwritten);
            (* delete the file from the cache table *)
            EVAL cache.filetable.delete(file, fcache);
          ELSE
            EXIT;
          END;
        END;
      END;
    ELSE
      LOCK cache.filetablelock DO
        IF NOT cache.filetable.delete(file, fcache) THEN
          RAISE CacheError(EC.FileNotInCache);
        END;
      END;
      totalwritten := fcache.dealloc(flush);
    END;

    RETURN totalwritten;
  END Delete;

PROCEDURE RoundToCacheBlock(n: CARDINAL) : CARDINAL =
  VAR nblocks: CARDINAL;
  BEGIN
    nblocks := n DIV Buffer.BlockSize;
    IF n MOD Buffer.BlockSize # 0 THEN
      INC(nblocks);
    END;
    RETURN nblocks;
  END RoundToCacheBlock;

PROCEDURE GetCentralCache() : T =
  BEGIN
    RETURN centralCache;
  END GetCentralCache;

PROCEDURE PrintStat(cache: T) =
  VAR
    it: FileMObjCacheTbl.Iterator;
    file: CacheFile.T;
    fcache: FileMObjCache.T;
    capacity, current: VMTypes.PageCount;
    hits, misses, steals, stolen, id: INTEGER;
    nextvic, notvic: FileMObjCache.T;
    nextvicindex, notvicindex, nextvicid, notvicid: INTEGER;
  BEGIN
    IO.Put("\nStatistics for cache " & cache.baseCache.print() & ":\n");

    (* find capacity and number of unallocated blocks *)
    cache.baseCache.stat(capacity, current);
    IO.Put("    Capacity: " & Fmt.Int(capacity) & " pages of size " &
      Fmt.Int(BlockSize) & "\n");
    IO.Put("    Free: " & Fmt.Int(current) & "\n");

    (* determine which file has the next victim block and the least 
       likely victim block *)
    Victim.Stat(cache.baseCache, nextvic, nextvicindex, notvic, notvicindex);
    IF nextvic # NIL THEN
      nextvicid := nextvic.id();
    ELSE
      nextvicid := 0;
    END;
    IF notvic # NIL THEN
      notvicid := notvic.id();
    ELSE
      notvicid := 0;
    END;

    (* print data for each file *)
    LOCK cache.filetablelock DO
      it := cache.filetable.iterate();
      WHILE it.next(file, fcache) DO
        fcache.stat(capacity, current, hits, misses, steals, stolen, id);
        IO.Put("  File Id " & Fmt.Int(id) & ": capacity " & 
          Fmt.Int(capacity) & ", occupied " & Fmt.Int(current) &
          ", hits " & Fmt.Int(hits) & ", misses " & Fmt.Int(misses) & 
          ", steals " & Fmt.Int(steals) & ", stolen " & Fmt.Int(stolen));
        IF id = nextvicid THEN
          IO.Put(", -(" & Fmt.Int(nextvicindex) & ")");
        END;
        IF id = notvicid THEN
          IO.Put(", +(" & Fmt.Int(notvicindex) & ")");
        END;
        IO.Put("\n");
      END;
    END;
    IF nextvic # NIL THEN
      IO.Put("\n  -(n): indicates file block n is next victim candidate\n");
    END;
    IF notvic # NIL THEN
      IO.Put("  +(n): indicates file block n is last victim candidate\n");
    END;
    IO.Put("\n");
  END PrintStat;

BEGIN
  EVAL FileDataCacheInterface.Export(NIL);
  centralCache := Create(DefaultCacheSize);
END FileDataCache.

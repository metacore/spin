(*
 * Copyright 1994 - 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	MaxBlockNo intermediate computation overflows 32 bits, add parens
 *
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)
INTERFACE FileMObjCache;

IMPORT CacheObject;
IMPORT VMTypes, Error;
IMPORT Buffer, BaseMObjCache, FileDataCache, CacheFile;
IMPORT DoubleList;

CONST Brand = "FileMObjCache";

CONST
  (* set max file at 4 GB *)
  MaxBlockNo = 1024 * (1024 * 1024 * 4 DIV Buffer.BlockSize);

  (* set default file size at 1 MB *)
  DefaultCacheFileSize = 1024 * 1024 DIV Buffer.BlockSize;

TYPE
  BlockNo = [0..MaxBlockNo-1];
  BlockOff = [0..Buffer.BlockSize-1];

TYPE FileMObjListEltT = DoubleList.EltT OBJECT
  filemobj: T;
END;

TYPE
  T <: Public;
  Public = CacheObject.T OBJECT
    victimlist: DoubleList.T := NIL;    (* allows per-file victim list *)
    victimlistElt: FileMObjListEltT := NIL; (* file can be in a victim list *)
  METHODS
    (* initialize a new FSCacheObj *)
    init(file: CacheFile.T; swapout: FileDataCache.SwapOutProc;
         swapin: FileDataCache.SwapInProc; baseCache: BaseMObjCache.T; 
         blkcount: VMTypes.PageCount := DefaultCacheFileSize) : T
         RAISES { Error.E };

    (* copy data into a block at specified offset relative to beginning
       of block.  will wait if block is locked.  return bytes written. *)
    (* XXX - need error reporting *)
    writeBlock(blockno: BlockNo; reloffset: BlockOff; 
               READONLY data: ARRAY OF CHAR) : INTEGER RAISES { Error.E };

    (* copy data into a block at specified offset relative to beginning
       of block.  fail if block is locked.  return bytes written. *)
    (* XXX - need error reporting *)
    tryWriteBlock(blockno: BlockNo; reloffset: BlockOff;
                  READONLY data: ARRAY OF CHAR) : INTEGER RAISES { Error.E };

    (* copy data from block at specified offset relative to beginning of
       block to beginning of array.  wait if block is locked. return 
       bytes read. *)
    (* XXX - need error reporting *)
    retrieveData(blockno: BlockNo; reloffset: BlockOff;
                 VAR data: ARRAY OF CHAR) : INTEGER RAISES { Error.E };

    (* copy data from block at specified offset relative to beginning of
       block to beginning of array.  fail if block is locked.  return
       bytes read. *)
    (* XXX - need error reporting *)
    tryRetrieveData(blockno: BlockNo; reloffset: BlockOff;
                    VAR data: ARRAY OF CHAR) : INTEGER RAISES { Error.E };

    (* release a page.  if dirty, write it to disk.  wait if block is locked *)
    releaseBlock(blockno: BlockNo) : Buffer.T RAISES { Error.E };

    (* release a page.  if dirty, write it to disk.  fail if block is locked *)
    tryReleaseBlock(blockno: BlockNo) : Buffer.T RAISES { Error.E };

    (* flush a file to storage without releasing any blocks. writes one
       block at a time, and does not lock entire file for duration of
       flush operation. if blockno and numblocks are provided, they define
       a range of blocks to be flushed. if blockno or numblocks are invalid,
       entire file will be flushed. returns number of bytes written. *)
    flush(blockno: BlockNo := 0; numblocks: INTEGER := -1) : 
          CARDINAL RAISES { Error.E };

    (* return the file size, according to the cache *)
    filesize() : INTEGER;

    (* deallocate this file cache. flushes dirty blocks if flush is true *)
    dealloc(flush: BOOLEAN := TRUE) : INTEGER RAISES { Error.E };

    (* get cache stats for this file *)
    stat(VAR capacity: VMTypes.PageCount; VAR occupied: VMTypes.PageCount;
         VAR hits: INTEGER; VAR misses: INTEGER; VAR steals: INTEGER;
         VAR stolen: INTEGER; VAR id: INTEGER);

    (* get the unique(?) id for this file - hash value used for tables *)
    id() : INTEGER;

    (* return a descriptive string *)
    print() : TEXT;
  END;

END FileMObjCache.

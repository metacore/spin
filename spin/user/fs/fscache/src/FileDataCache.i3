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

(* FileDataCache is a programmer-friendly API to the potentially shared
   file block cache, which is stolen and slightly modified from the
   Web cache written by Emin Gun Sirer.  There is one major point
   that reduces the usability of this interface: the generic File.T
   does not come with Hash and Equal procedures to allow it to be the
   key for a table.  Adding these procedures would not be hard, but
   until then this cache is specific to files who inherit from 
   CacheFile.T. *)

INTERFACE FileDataCache;

IMPORT CacheFile, Error, File, Buffer, VMTypes;

TYPE 
  T <: REFANY;
  SwapOutProc = PROCEDURE(file: CacheFile.T; offset: File.OffsetT;
                          READONLY data: ARRAY OF CHAR) : CARDINAL
                          RAISES {Error.E};
  SwapInProc = PROCEDURE(file: CacheFile.T; offset: File.OffsetT;
                         VAR data: ARRAY OF CHAR) : CARDINAL
                         RAISES {Error.E};
  EC = { FileNotInCache, FileAlreadyInCache };

EXCEPTION CacheError(EC);

CONST
  BlockSize = Buffer.BlockSize;
  O_USECACHE = 16_10000000; (* supplements the open mode flags *)
  (* arbitrarily set to 2 MB, low for caching but good for debugging *)
  DefaultCacheSize = 2 * 1024 * 1024 DIV BlockSize;

PROCEDURE Create(numpages: VMTypes.PageCount := DefaultCacheSize) : T;

PROCEDURE AddFile(cache: T; file: CacheFile.T; swapout: SwapOutProc; 
  swapin: SwapInProc; startpre: INTEGER := 0; numpre: INTEGER := -1) 
  RAISES {Error.E, CacheError};
  (* startpre is the block at which to start prefetching; numpre is
     the number of blocks to prefetch - -1 indicates entire file *)

PROCEDURE FileIsCached(cache: T; file: CacheFile.T) : BOOLEAN;

PROCEDURE Read(cache: T; file: CacheFile.T; offset: CARDINAL;
  VAR data: ARRAY OF CHAR) : CARDINAL RAISES {Error.E, CacheError};

PROCEDURE Write(cache: T; file: CacheFile.T; offset: CARDINAL;
  READONLY data: ARRAY OF CHAR) : CARDINAL RAISES{Error.E,CacheError};

PROCEDURE Flush(cache: T; file: CacheFile.T := NIL; 
  blockno: INTEGER := 0; numblocks: INTEGER := -1) : CARDINAL 
  RAISES {Error.E,CacheError};

PROCEDURE Delete(cache: T; file: CacheFile.T; flush: BOOLEAN) : CARDINAL
  RAISES {Error.E,CacheError};

PROCEDURE RoundToCacheBlock(n: CARDINAL) : CARDINAL;

PROCEDURE GetCentralCache() : T;

PROCEDURE PrintStat(cache: T);
  (* print some cache statistics *)

END FileDataCache. 

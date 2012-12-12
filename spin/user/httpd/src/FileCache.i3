(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added swapping and pageout onto an extent based storage device.
 *
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Cache of files, each of which consists of lists of
 *      data blocks.
 *
 *)
INTERFACE FileCache;
IMPORT BlockList, FilenameBlockListTbl;

EXCEPTION SwapNotFound;

CONST Brand = "FileCache";

(*
 * A filecache is a structure that holds filename to blocklist 
 * translations. It provides fast access methods to the blocks
 * of a file. get, put, delete and iterate are identical to
 * what tables do. flush flushes a specific file out of the
 * cache. An argument of nil to flush means flush the entire
 * file cache. pack triggers a check to determine the size of
 * the total in-memory cache, and a flush out to disk of least
 * frequently used files.
 *)
TYPE 
  Public = OBJECT 
    METHODS
      get(READONLY k: TEXT; VAR bl: BlockList.T): BOOLEAN;
      put(READONLY k: TEXT; v: BlockList.T): BOOLEAN;
      delete(READONLY k: TEXT; VAR v: BlockList.T): BOOLEAN;
      flush(READONLY k: TEXT := NIL);
      setMaxSize(maxsize: INTEGER);
      registerSwapDevice(devname: TEXT) RAISES {SwapNotFound};
      iterate(): Iterator;
      pack();
    END;      
  T <: Public;
  Iterator = FilenameBlockListTbl.Iterator;

(* maxsize of zero means there is no limit *)
PROCEDURE New(maxsize: INTEGER := 0) : T;

END FileCache.

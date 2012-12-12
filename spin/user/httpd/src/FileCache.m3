(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 03-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added swapping and pageout onto an extent based storage device.
 *
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. 
 *
 *)

(* "FileCache" implements a cache of files, each of which consists of
   lists of data blocks. *)

MODULE FileCache;
IMPORT FilenameBlockListTbl, BlockList, Extent;
IMPORT Error, Device;
(* IMPORT NameServer; *)
IMPORT IO, Fmt;
FROM Httpd IMPORT debug;

REVEAL T = Public BRANDED Brand OBJECT 
  table   : FilenameBlockListTbl.Default;
  maxsize : INTEGER := 100;
  swap    : Extent.T;
  devName : TEXT;
OVERRIDES
  get := Get;
  put := Put;
  pack := Pack;
  flush := Flush;
  delete := Delete;
  iterate := Iterate;
  setMaxSize := SetMaxSize;
  registerSwapDevice := RegisterSwapDevice;
END;

PROCEDURE Get(self: T; READONLY k: TEXT; VAR bl: BlockList.T) : BOOLEAN =
  VAR found: BOOLEAN;
  BEGIN
    found := self.table.get(k, bl);
    IF found THEN
      INC(bl.refcount);
    END;
    RETURN found;
  END Get;

PROCEDURE Put(self: T; READONLY k: TEXT; bl: BlockList.T) : BOOLEAN =
  VAR found: BOOLEAN;
  BEGIN
    bl.registerSwap(self.swap, self.devName);
    bl.refcount := 1;
    found := self.table.put(k, bl);
    Pack(self);
    RETURN found;
  END Put;

PROCEDURE Delete(self: T; READONLY k: TEXT; VAR bl: BlockList.T) : BOOLEAN =
  BEGIN
    RETURN self.table.delete(k, bl);
  END Delete;

PROCEDURE Iterate(self: T) : Iterator =
  BEGIN
    RETURN self.table.iterate();
  END Iterate;

(*
 * Compress a given file that is in the file cache.
 *)
PROCEDURE Flush(self: T; READONLY k: TEXT := NIL) =
  VAR
    it: Iterator;
    fname: TEXT;
    candidate: BlockList.T;
  BEGIN
    IF k = NIL THEN
      (* This means we should flush everything. *)
      it := self.table.iterate();
      WHILE it.next(fname, candidate) DO
        IF debug THEN
          IO.Put("Paging " & fname & " out...\n");
        END;
        candidate.pageout();
      END;
    ELSE
      IF self.table.get(k, candidate) THEN
        IF debug THEN
          IO.Put("Paging " & k & " out...\n");
        END;
        candidate.pageout();
      END;
    END;        
  END Flush;

PROCEDURE Pack(self: T) =
  VAR
    it: Iterator;
    fname, candidatename: TEXT;
    candidate, bl: BlockList.T;
    score, minscore, size, fsize: INTEGER;
  BEGIN
    IF self.maxsize = 0 THEN
      RETURN;
    END;
    it := self.table.iterate();
    candidate := NIL;
    minscore := LAST(INTEGER);
    size := 0;
    (* Pick the guy with the least accesses to page out *)
    WHILE it.next(fname, bl) DO
      fsize := bl.size(); (* size of block cache *)
      size := size + fsize;
      IF debug THEN
        IO.Put("Considering " & fname & " for pageout, score is " & 
               Fmt.Int(bl.refcount) & " size is " & Fmt.Int(fsize) & ".\n");
      END;
      score := bl.refcount;
      IF score < minscore AND fsize > 0 THEN
        minscore := score;
        candidate := bl;
        candidatename := fname;
      END;
    END;
    IF size < self.maxsize OR candidate = NIL THEN
      RETURN;
    END;
    IF debug THEN
      IO.Put("Paging " & candidatename & " out...\n");
    END;
    candidate.pageout();
  END Pack;

PROCEDURE SetMaxSize(self: T; maxsize: INTEGER) =
  BEGIN
    self.maxsize := maxsize;
    Pack(self);
  END SetMaxSize;

PROCEDURE RegisterSwapDevice(self: T; devName: TEXT) RAISES {SwapNotFound} =
  VAR
    dev : Device.T;
  BEGIN
    TRY 
      dev := Device.Lookup(devName);
      TYPECASE dev OF
      | NULL =>
        IO.PutError("FileCache.RegisterSwapDevice ");
        IO.Put(devName);
        IO.Put(" not found.");
      | Extent.T(ext) =>
        self.devName := devName;
        self.swap    := ext;
      ELSE
        IO.PutError("FileCache.RegisterSwapDevice ");
        IO.Put(devName);
        IO.Put(" not an extent.");
      END;
    EXCEPT
    | Error.E(e) =>
      IO.PutError("FileCache.RegisterSwapDevice (");
      IO.Put(e.message());
      IO.Put(")\n");
      RAISE SwapNotFound;
    END;      
  END RegisterSwapDevice;

PROCEDURE New(maxsize: INTEGER := 0) : T =
  VAR cache: T;
  BEGIN
    cache := NEW(T);
    cache.table := NEW(FilenameBlockListTbl.Default).init();
    cache.maxsize := maxsize;
    RETURN cache;
  END New; 

BEGIN
END FileCache.

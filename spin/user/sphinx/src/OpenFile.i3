(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added mmap support.
 * 23-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Renamed from DlibFile.
 *	
 *)
INTERFACE OpenFile;
IMPORT Dispatcher;
IMPORT File; <*NOWARN*>
IMPORT Ctypes;
IMPORT MemoryObject;

TYPE T = MUTEX OBJECT
  path: TEXT; (* Just for debugging *)
  h: REFANY; (* File.T; taken out by mef *)
  memObj: MemoryObject.T; (* the memobject backed up by the inode pager.
		      "m" is NIL unless mmap is called on this file. *)
  binding: Dispatcher.Binding; (* This holds the binding for
				the MemoryObject.Destroy event
				for "memObj". *)

  offset: INTEGER; (* offset for read/write *) 
  flags: Ctypes.unsigned_int; (* Open mode, O_RDONLY etc *)
  seekable: BOOLEAN; (* TRUE for block devices, FALSE for streams.
		         "offset" is meaningful only on seekable files *)
  refCount: CARDINAL; (* How many fds point this? Note:
		       "refCount" is incremented when the memory object
		       is created for this file(i.e. when "memObj"
		       becomes non NIL.*)
  next, prev: T; (* free queue *)
END;

(* This module maintains a table that maps memory object to T.
 "RegisterMemObj" adds an entry to that table,
 "FineMemObj" looks up the table, ande
 "DeleteMemObj" deletes an entry from the table. *)
  
PROCEDURE Register(t: T): BOOLEAN;
  (* Register returns true when the entry for the memory object is
     newly created in the table. False if "t" is just merely appended to
     the existing entry.

     I know this is a hack. We need to store multiple Ts for a single
     memory object because there is no notion of "file identity" right now in
     sphinx; so if you open same file twice, we end up having two Ts, while
     the inode pager keeps its own table to create only one memory object
     per one file.

     Ultimately, the inode pager should be integrated with the file
     caching mechanism, and File.T should contain ptr to the memory object.
     Then we don't have to include memObj field in T.
  *)
  
PROCEDURE Find(memObj: MemoryObject.T): REF ARRAY OF T;
PROCEDURE Delete(memObj: MemoryObject.T);
CONST Brand = "OpenFile";
    
END OpenFile.

(*
  Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Added lock to MP for synchronization.
 *
 * 04-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	Added file open flags to File.  Needed for open for truncate.
 *
 * 25-May-96  Tim Bradley (tbradley) at the University of Washington
 *	 File holds filesystem, file types, user interface to lfs.  Whisted.
 *
 *)
INTERFACE LFSRep;

IMPORT File, Directory,FileStat;
IMPORT FileSystem, Error;
IMPORT SegBuffer;
IMPORT Segment;
IMPORT IMap;
IMPORT INode;
IMPORT DirEnt;   (* directory entry object defined in fscore/src/DirEnt.i3 *)
IMPORT Cleaner;
IMPORT LFSLock;

(***********************
	TYPES
 ***********************)
EXCEPTION OffsetOutOfBounds;


(* for nameserver *)
TYPE FileTPublic = File.T BRANDED OBJECT
METHODS
  getMP():MP;    (* this is needed to reveal the mp to the cleaner, is temporary *)
END;

TYPE FileT      <: FileTPublic;

TYPE DirectoryT <: Directory.T;

TYPE FileSystemT <: FileSystem.T; 


TYPE
  MP = OBJECT
    blocksInExtent : CARDINAL;
    segBuff        : SegBuffer.Buffer;
    segInfo        : Segment.T;   (* generic info about filesystem *)
    imap           : IMap.T;
    cleaner        : Cleaner.T;
    demoCurrentSegment:CARDINAL;  (* a variable used by the graphical lfs demo *)
    lock           : LFSLock.T;
  METHODS
    (*openDir(path:TEXT)   := OpenDir;*)
    open  (mode: INTEGER; path: TEXT):T;
    unmount();
    sync();
    mkDir(mp:MP;dirPath:TEXT);
    rmDir(mp:MP;filePath:TEXT);
    unlink(mp:MP;filePath:TEXT);
  END;



  (* this is the LFS file object *)
  T <: Public;

  Public = BRANDED OBJECT
    mu            : MUTEX;
    mp            : MP;
    inode         : INode.T;
    flags         : BITS 8 FOR [0..255];  (* used for remembering if we're open
                                             for truncation. *)
  METHODS
    read  (offset: File.OffsetT;VAR data: ARRAY OF CHAR): CARDINAL RAISES {Error.E};
    write (offset: File.OffsetT;READONLY data  : ARRAY OF CHAR):CARDINAL RAISES {Error.E};
    close ();
    getDirEntries (pos:INTEGER; VAR fileArray:ARRAY OF DirEnt.T): CARDINAL RAISES {Error.E};
    stat (VAR s: FileStat.T);
  END;
  
  DH = OBJECT
    fileArray :  REF ARRAY OF DirEnt.T;
    offset    :  INTEGER;  (* the number of the file to read next *)
  METHODS
    readDir (dh: DH): TEXT;
    closeDir (dh: DH);
  END;



(****************************
	PROCEDURES
 ****************************)

PROCEDURE Init(verbose:BOOLEAN);

END LFSRep.








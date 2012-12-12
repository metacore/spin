(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made compatible with UNIX struct stat.
 *
 * 19-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* "FileStat" defines a Unix like file information. *)

INTERFACE FileStat;
IMPORT Ctypes;

TYPE DevT   = Ctypes.int;
TYPE SizeT  = Ctypes.int;
TYPE NlinkT = Ctypes.unsigned_short;
TYPE UidT   = Ctypes.unsigned_int;
TYPE GidT   = Ctypes.unsigned_int;
TYPE InoT   = Ctypes.unsigned_int;
TYPE ModeT  = Ctypes.unsigned_int;
TYPE OffT   = Ctypes.unsigned_long;
TYPE TimeT  = Ctypes.int;
TYPE MajorT = Ctypes.unsigned_int;
TYPE MinorT = Ctypes.unsigned_int;

(* FileStat.T is really an imitation of UNIX struct stat. *)
TYPE T = RECORD
  dev     : DevT;
  ino     : InoT;
  mode    : ModeT;
  nlink   : NlinkT;
  uid     : UidT;
  gid     : GidT;
  rdev    : DevT;
  size    : OffT;
  atime   : TimeT;
  space1  : Ctypes.int;
  mtime   : TimeT;
  space2  : Ctypes.int;
  ctime   : TimeT;
  space3  : Ctypes.int;
  blksize : Ctypes.unsigned_int; (* size of block in file *)
  blocks  : Ctypes.int;          (* # of blocks in file *)
  flags   : Ctypes.unsigned_int; (* user defined flags *)
  gen     : Ctypes.unsigned_int; (* generation number *)
END;

PROCEDURE Init((*OUT*)VAR t: T);
  (* Sets default values into all the fields in "t".
     All the file system "stat" methods MUST call this procedure.
     User procs will fail is very obscure way if you forget to fill
     some of the fields in T. Ask yasushi about all the weird
     stories of how neglecting stat hurts you.*)
  
END FileStat.

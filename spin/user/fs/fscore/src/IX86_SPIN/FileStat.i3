(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Dec-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

(* "FileStat" defines a Unix like file information. *)

INTERFACE FileStat;
IMPORT Ctypes;

TYPE DevT   = Ctypes.unsigned_long;
TYPE SizeT  = Ctypes.unsigned_long;
TYPE NlinkT = Ctypes.unsigned_short;
TYPE UidT   = Ctypes.unsigned_long;
TYPE GidT   = Ctypes.unsigned_long;
TYPE InoT   = Ctypes.unsigned_long;
TYPE ModeT  = Ctypes.unsigned_short;
TYPE OffT   = INTEGER;
TYPE TimeT  = Ctypes.unsigned_long;
TYPE TimeSpecT = RECORD
  ts_sec: Ctypes.long;
  ts_nsec: Ctypes.long;
END;
(*
TYPE MajorT = Ctypes.unsigned_int;
TYPE MinorT = Ctypes.unsigned_int;
*)

(* FileStat.T is really an imitation of UNIX struct stat. *)
TYPE T = RECORD
  dev	: Ctypes.unsigned_long;
  ino	: Ctypes.unsigned_long;
  mode	: Ctypes.unsigned_short;
  nlink	: Ctypes.unsigned_short;
  uid	: Ctypes.unsigned_long;
  gid	: Ctypes.unsigned_long;
  rdev	: Ctypes.unsigned_long;
(*
  atime : TimeSpecT;
  mtime	: TimeSpecT;
  ctime	: TimeSpecT;
 *)
  atime : Ctypes.long;
  atime_dummy : Ctypes.long;
  mtime : Ctypes.long;
  mtime_dummy : Ctypes.long;
  ctime : Ctypes.long;
  ctime_dummy : Ctypes.long;

  (* XXX how should I specify long long type? *)
  size	: INTEGER;
  size_dummy: INTEGER;	(* dummy field *)
  blocks: INTEGER;
  blocks_dummy: INTEGER;	(* dummy field *)
  blksize: Ctypes.unsigned_long;
  flags	: Ctypes.unsigned_long;
  gen	: Ctypes.unsigned_long;
  lspare: Ctypes.int;
(*
  qspare: ARRAY [0..1] OF INTEGER;
 *)
END;

PROCEDURE Init((*OUT*)VAR t: T);
  (* Sets default values into all the fields in "t".
     All the file system "stat" methods MUST call this procedure.
     User procs will fail is very obscure way if you forget to fill
     some of the fields in T. Ask yasushi about all the weird
     stories of how neglecting stat hurts you.*)
  
END FileStat.

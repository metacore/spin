(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added Init.
 * 08-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* struct statfs, usually defined in sys/mount.h *)

INTERFACE StatFs;
FROM Ctypes IMPORT short, int;

CONST MNAMELEN = 90;
  
TYPE T = RECORD
  type: short; (* type of filesystem (see below) *)
  flags: short; (* copy of mount flags *)
  fsize: int; (* fundamental filesystem block size *)
  bsize: int; (* optimal transfer block size *)
  blocks: int; (* total data blocks in file system, 
		note: may not represent fs size. *)
  bfree: int; (* free blocks in fs *)
  bavail: int; (* free blocks avail to non-su *)
  files: int; (* total file nodes in file system *)
  ffree: int; (* free file nodes in fs *)
  fsid: ARRAY [0..1] OF int; (* file system id *)
  spare: ARRAY [0..8] OF int; (* spare for later *)
  mntonname: ARRAY [0..MNAMELEN-1] OF CHAR; (* directory on which mounted *)
  mntfromname: ARRAY [0..MNAMELEN-1] OF CHAR; (* mounted filesystem *)
END;
  
PROCEDURE Init((*OUT*)VAR t: T);
  (* Sets default values into all the fields in "t". *)
  
END StatFs.

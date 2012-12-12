(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 21-Oct-95  Yasushi Saito (yasushi) at the University of Washington
 *	Many changes. Really works now.
 *
 * 02-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created from defs.h.
 *)
INTERFACE FileDefs;
FROM Ctypes IMPORT int, unsigned_int, unsigned_long;
IMPORT Word;

(*
 * Common definitions for Berkeley Fast File System.
 *
 * Compatibility definitions for disk IO.
 *)

(*
 * Disk devices do all IO in 512-byte blocks.
 *)
CONST DEV_BSIZE = 512;

(*
 * Conversion between bytes and disk blocks.
 *)
PROCEDURE btodb(byte_offset: Word.T): Word.T;
PROCEDURE dbtob(block_number: Word.T):Word.T;

CONST NBBY = 8;

(*
 * The file system is made out of blocks of at most MAXBSIZE units,
 * with smaller units (fragments) only in the last direct block.
 * MAXBSIZE primarily determines the size of buffers in the buffer
 * pool.  It may be made larger without any effect on existing
 * file systems; however, making it smaller may make some file
 * systems unmountable.
 *
 * Note that the disk devices are assumed to have DEV_BSIZE "sectors"
 * and that fragments must be some multiple of this size.
 *)
CONST MAXBSIZE = 8192;
CONST MAXFRAG = 8;

(*
 * MAXPATHLEN defines the longest permissible path length
 * after expanding symbolic links.
 *
 * MAXSYMLINKS defines the maximum number of symbolic links
 * that may be expanded in a path name.  It should be set
 * high enough to allow all legitimate uses, but halt infinite
 * loops reasonably quickly.
 *)
CONST MAXPATHLEN = 1024;
CONST MAXSYMLINKS = 8;

TYPE vm_offset_t = unsigned_long;
TYPE vm_size_t = unsigned_long;
TYPE daddr_t = int;
TYPE quad = ARRAY [0..1] OF unsigned_int;
TYPE time_t = int;
TYPE ino_t = int;
  
END FileDefs.


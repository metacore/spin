(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

INTERFACE DirEnt;
IMPORT Ctypes;

TYPE T = RECORD
  nextPos : INTEGER; (* a cookie to retrieve the next entry *)
  ino : Ctypes.unsigned_int;   (* inode number of entry *)
  name : TEXT;
END;
  
END DirEnt.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Ufs inode block file size field for little endian machines *)
INTERFACE UfsFsSize;
IMPORT Ctypes;

TYPE T = RECORD
  size : Ctypes.unsigned_int;
  unused : Ctypes.unsigned_int;
END;
  
  
END UfsFsSize.

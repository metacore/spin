(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE UdpPktFormat;
IMPORT Ctypes;

(* 
   Harbison p258: packed types only need to be packed in records,
   objects, and arrays.
*)
TYPE Header = RECORD
  sport: BITS 16 FOR Ctypes.unsigned_short;
  dport: BITS 16 FOR Ctypes.unsigned_short;
  len:   BITS 16 FOR Ctypes.unsigned_short;
  check: BITS 16 FOR Ctypes.unsigned_short;
END;

<* OBSOLETE *> TYPE T = UNTRACED REF Header;
TYPE NewT = Header;
END UdpPktFormat.

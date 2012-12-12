(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

(* Untrusted *) 
INTERFACE AtmPktFormat;
IMPORT Ctypes;
TYPE AtmType = Ctypes.unsigned_short;
CONST
  ATMTYPE_IP     : AtmType= 16_0800; (* IP protocol *)

TYPE Header = RECORD
  type : AtmType;
END;
TYPE T = UNTRACED REF Header;

END AtmPktFormat.


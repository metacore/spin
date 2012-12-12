(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support for new spin commands.
 *
 *)

INTERFACE UdpDefault;
IMPORT Ctypes;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "udpdefault";
      CommandHelp = "-- -debug level| -packets";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

(* 
   Harbison p258: packed types only need to be packed in records,
   objects, and arrays.
*)
TYPE Header = RECORD
  sport: Ctypes.unsigned_short;
  dport: Ctypes.unsigned_short;
  len:   Ctypes.unsigned_short;
  check: Ctypes.unsigned_short;
END;

TYPE T = UNTRACED REF Header;

PROCEDURE Init(verbose:BOOLEAN);

END UdpDefault.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 21-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Active message implementation on top of T3net.  Packet format
 *	is the same as an t3net packet, with an additional 2byte
 *	header that determines which handler gets invoked.
 *
 *)

(* Untrusted *) INTERFACE T3AM;
IMPORT Ctypes;
IMPORT T3PktFormat;

CONST
  T3TYPE_AM: T3PktFormat.T3Type = 16_FF00;
  GET: T3PktFormat.T3Type = 16_01;
  PUT: T3PktFormat.T3Type = 16_02;

TYPE Header = RECORD
  hndlr:Ctypes.unsigned_int;
  len:Ctypes.unsigned_int;
  timestamp:INTEGER;
END;

TYPE T = UNTRACED REF Header;

PROCEDURE Init();
END T3AM.

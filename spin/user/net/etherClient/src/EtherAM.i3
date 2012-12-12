(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 21-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Active message implementation on top of Ethernet.  Packet format
 *	is the same as an ethernet packet, with an additional 2byte
 *	header that determines which handler gets invoked.
 *
 *)

(* Untrusted *) INTERFACE EtherAM;
IMPORT Ctypes;
IMPORT EtherPktFormat;

CONST
  ETHERTYPE_AM: EtherPktFormat.EtherType = 16_FF00;
  GET: EtherPktFormat.EtherType = 16_01;
  PUT: EtherPktFormat.EtherType = 16_02;

TYPE Header = RECORD
  hndlr:Ctypes.unsigned_int;
  len:Ctypes.unsigned_int;
  timestamp:INTEGER;
END;

<* OBSOLETE *> TYPE T = UNTRACED REF Header;
TYPE NewT = Header;

PROCEDURE Init();
END EtherAM.

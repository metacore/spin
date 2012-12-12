(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Trent fixed up types for new ip reassembly code.
 *
 * 16-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	This module implements the IP reassembly routine and passes the
 *	reconstructed ip packet up the protocol stack.
 *
 *)

(* Untrusted *)
INTERFACE IpFrag;
IMPORT IpPktFormat;
IMPORT Mbuf;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "ipfrag";
      CommandHelp = "-- -debug level";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

TYPE IpFragPacket = RECORD
  data      : Mbuf.T;
  next      : REF IpFragPacket;
  offset    : CARDINAL;
  length    : CARDINAL;
END;

TYPE IpFrag = RECORD
  packets   : REF IpFragPacket;
  tot_len   : CARDINAL;
  exp_len   : CARDINAL;
  timestamp : INTEGER;
END;
TYPE T = REF IpFrag;

TYPE NewT = IpPktFormat.T;
PROCEDURE Init(verbose: BOOLEAN);

CONST Brand = "IpFrag";

END IpFrag.

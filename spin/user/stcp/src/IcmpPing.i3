(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created to test EtherPacket.m3.  Test only.
 *)
INTERFACE IcmpPing;
IMPORT Ctypes, StcpMbuf;
IMPORT StcpIpPktFormat;

VAR
  debug : BOOLEAN := FALSE;

TYPE Header = RECORD
  type: BITS 8 FOR Ctypes.unsigned_char;
  code: BITS 8 FOR Ctypes.unsigned_char;
  csum: BITS 16 FOR Ctypes.unsigned_short;
  id: BITS 16 FOR Ctypes.unsigned_short;
  seq: BITS 16 FOR Ctypes.unsigned_short;
END;

TYPE T = Header;

PROCEDURE PacketArrived(m, c : StcpMbuf.T ; offset : CARDINAL ; ipHdr:StcpIpPktFormat.T) : StcpMbuf.T ;

PROCEDURE SendRequest();

END IcmpPing.

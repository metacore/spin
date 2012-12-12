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
 *	Created for simple tcp.
 *)

INTERFACE StcpIpPacket;

IMPORT StcpIpPktFormat;
IMPORT StcpMbuf;
IMPORT StcpSocketAddr;

(* ---------------------------------------------------------------- *)
VAR
  debug : BOOLEAN := FALSE;

(* ---------------------------------------------------------------- *)
PROCEDURE PacketArrived(m, cur : StcpMbuf.T ; offset : CARDINAL) : StcpMbuf.T;
PROCEDURE PacketGuard(m, cur : StcpMbuf.T ; offset : CARDINAL) : BOOLEAN;

(* ---------------------------------------------------------------- *)
PROCEDURE PacketSend(m : StcpMbuf.T ; ip : StcpIpPktFormat.T ; so : StcpSocketAddr.T);

END StcpIpPacket.

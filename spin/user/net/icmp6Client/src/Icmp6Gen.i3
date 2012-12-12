(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 21-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Interface to send icmp packet over the network.  Client sets the
 *	icmp type and icmp code. The module computes the check sum over
 *	the data and the icmp header.
 *
 *)

INTERFACE Icmp6Gen;
IMPORT Ip6PktFormat;
IMPORT Icmp6PktFormat;
IMPORT Mbuf, Net;
VAR
  debug_level := Net.oLevel.NODEBUG;
PROCEDURE PacketSend(	
	READONLY ip     : Ip6PktFormat.Header;
	READONLY icmp  	: Icmp6PktFormat.Header;
	READONLY data	: Mbuf.T);
END Icmp6Gen.

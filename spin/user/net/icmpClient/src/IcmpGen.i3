(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Interface to send icmp packet over the network.  Client sets the
 *	icmp type and icmp code. The module computes the check sum over
 *	the data and the icmp header.
 *)

INTERFACE IcmpGen;
IMPORT IpPktFormat, IcmpPktFormat, Mbuf, Net;
VAR debug_level : Net.Level := Net.oLevel.NODEBUG;
PROCEDURE PacketSend(	
    READONLY ip   : IpPktFormat.Header;
    READONLY icmp : IcmpPktFormat.T;
    READONLY data : Mbuf.T);
PROCEDURE Init(verbose:BOOLEAN);
END IcmpGen.

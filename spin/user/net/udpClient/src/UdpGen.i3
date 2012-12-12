(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Interface to send udp packet over the network.  The module computes the
 *      check sum over the data and the udp header.
 *)

INTERFACE UdpGen;
IMPORT IpPktFormat;
IMPORT UdpPktFormat;
IMPORT Mbuf;

PROCEDURE PacketSend(	
	READONLY ip     : IpPktFormat.Header;
	READONLY udp  	: UdpPktFormat.Header;
	READONLY data	: Mbuf.T;
        dochecksum      : BOOLEAN:=TRUE);
END UdpGen.

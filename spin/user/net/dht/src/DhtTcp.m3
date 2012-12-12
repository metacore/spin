(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 17-Nov-97  Marc Fiuczynski (mef) at the University of Washington
 *	Based on Vinh Lam's DHT sources from Spring'97.
 *)

MODULE DhtTcp;
IMPORT IpPktFormat, Ip6PktFormat, TcpPktFormat; (* plexus *)
IMPORT Dht;
IMPORT Mbuf; (* urt *)
IMPORT IO; (* spin/libm3 *)

CONST
  tcp_hdr_len = BYTESIZE(TcpPktFormat.Header);

PROCEDURE IP6toIP4(
    new_packet : Mbuf.T;
    READONLY ip6 : Ip6PktFormat.T; (* fields in network order *)
    READONLY ip4 : IpPktFormat.T;  (* fields in host order *)
    ): Mbuf.T = 
  BEGIN
    IF FALSE THEN
      IO.Put("got to dht tcp 4 handler\n");
    END;
    WITH tcpHeaderBuf = SUBARRAY(Mbuf.Array(new_packet)^,0,tcp_hdr_len),
         tcpHeader = VIEW(tcpHeaderBuf,TcpPktFormat.NewT) 
     DO
      tcpHeader.check := Dht.IP6toIP4Checksum(ip6,ip4,tcpHeader.check);
    END;
    RETURN new_packet; 
  END IP6toIP4;

PROCEDURE IP4toIP6(
    new_packet : Mbuf.T;
    READONLY ip4 : IpPktFormat.T; (* fields in network order *)
    READONLY ip6 : Ip6PktFormat.T;  (* fields in host order *)
    ): Mbuf.T = 
  BEGIN
    IF FALSE THEN
      IO.Put("got to dht tcp 4 handler\n");
    END;
    WITH tcpHeaderBuf = SUBARRAY(Mbuf.Array(new_packet)^,0,tcp_hdr_len),
         tcpHeader = VIEW(tcpHeaderBuf,TcpPktFormat.NewT) 
     DO
      tcpHeader.check := Dht.IP4toIP6Checksum(ip4,ip6,tcpHeader.check);
    END;
    RETURN new_packet; 
  END IP4toIP6;

BEGIN
END DhtTcp.

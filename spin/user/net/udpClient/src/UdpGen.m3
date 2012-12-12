(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Can now conditionally use spy timers.  Can also conditionally
 *	checksum udp packet.
 *
 * 01-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to compute correct udp checksum using ip overlay
 *	header.  Also compute checksum over all of mbuf data.
 *	
 * 15-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *      Rewritten to use mbufs.
 *
 * 15-Mar-95  Marc Fiuczynski (mef) at the University of Washington
 *      Created.
 *)

MODULE UdpGen;

(* IMPORT IO; *)
IMPORT Ctypes;
(* IMPORT Fmt; *)
IMPORT IpGen;
IMPORT IpPktFormat;
IMPORT Mbuf;
IMPORT Net;
(* IMPORT UdpDefault; *)
IMPORT UdpPktFormat;
(* IMPORT Word; *)
IMPORT Spy;
(* IMPORT SAL; *)

CONST 
  timing = FALSE;

VAR
  send_timer: Spy.T;

PROCEDURE PacketSend(
	READONLY ip	: IpPktFormat.Header;
	READONLY udp	: UdpPktFormat.Header;
	READONLY data	: Mbuf.T;
        dochecksum      : BOOLEAN:=TRUE) =
  CONST 
    header_len = BYTESIZE(UdpPktFormat.Header);
    ip_hdr_len = BYTESIZE(IpPktFormat.Header);
    protocol   = IpPktFormat.IPPROTO_UDP;
  VAR 
    size      : Ctypes.unsigned_short;
    packet    : Mbuf.T;
    ipovl_buf : ARRAY [1 .. BYTESIZE(IpPktFormat.OverlayHeader)] OF Net.BYTE;
    ipovl_csum: Ctypes.unsigned_short;
  BEGIN
    IF timing THEN Spy.Enter(send_timer); END;

    (* compute the length of the ip packet *)
    IF ip.tot_len # 0 THEN 
      size := ip.tot_len - ip_hdr_len;
    ELSE
      size :=  Mbuf.m_length (data) + header_len;
    END;

    (* 
       Need to check the alignment of the data pointer
       in the mbuf. If its aligned, then we could just
       Mbuf.M_PREPEND the data, which doesn't have the
       overhead of allocating an mbuf structure.
    *)
    IF data # NIL THEN
      packet := data; (* XXX don't lose the data pointer in m_prepend *)
      packet := Mbuf.M_PREPEND(packet,header_len,Mbuf.M_WAIT);
    ELSE
      packet := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
      packet.mh_hdr.mh_len := header_len;
      Mbuf.M_ALIGN(packet,header_len);      
    END;

    (* set the udp header fields *)
    WITH header_buf = Mbuf.Array(packet) DO
      (* CAST OPERATION *)
      (* WITH header = VIEW(header_buf[0],IcmpPktFormat.T) DO *)
      WITH header = VIEW(header_buf^,UdpPktFormat.NewT) DO
        header.sport := udp.sport;
        header.dport := udp.dport;
        header.len   := Net.htons(udp.len);
        header.check := 0;

        IF dochecksum THEN

          (* cons up ip overlay header for checksum *)
          WITH ipovl = VIEW(ipovl_buf,IpPktFormat.OverlayHeader) DO
            ipovl.fill[0]:= 0;
            ipovl.fill[1]:= 0;
            ipovl.ih_x1  := 0;
            ipovl.ih_pr  := protocol;
            ipovl.ih_len := Net.htons(size);
            ipovl.ih_src := ip.saddr;
            ipovl.ih_dst := ip.daddr;
          END;
          ipovl_csum := Net.checksum(ipovl_buf);

          (* compute new checksum for udp packet with ip overlay header *)
          header.check := Mbuf.Checksum(packet,ipovl_csum,size);
        END;
      END;
    END;        

    IF timing THEN Spy.Exit(send_timer); END;

    (* send as ip packet - no options *)
    IpGen.PacketSend(ip, packet);
  END PacketSend;

BEGIN
  send_timer:= Spy.Create("udp_output");
END UdpGen. 


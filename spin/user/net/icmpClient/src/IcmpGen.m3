(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new spin shell command style.  
 *	Converted from obsolete Clib interface to IO.
 *
 * 01-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Implements icmp packet send.  Sets the icmp checksum and send via IP.
 *	Fixed to use an mbuf aware checksum routine.  Packet data may now
 *	be distributed over several mbufs.
 *)

MODULE IcmpGen;

IMPORT IcmpPktFormat;
IMPORT IpPktFormat, IpGen;
IMPORT Net;
IMPORT Mbuf;
IMPORT Ctypes;

CONST 
  header_len : Ctypes.unsigned_short = BYTESIZE(IcmpPktFormat.T);
  debug = FALSE;

PROCEDURE PacketSend(
    READONLY ip   : IpPktFormat.Header;
    READONLY icmp : IcmpPktFormat.T;
    READONLY data : Mbuf.T) =
  VAR 
    packet : Mbuf.T;
  BEGIN
    (* grab an mbuf for the icmp header.  Since we will have to
       prepend link and network layer data as well, we will want to
       leave space at the beginning of the buffer and then use the
       sys/mbuf.h M_PREPEND macro to prepend data.  It is smart enough
       to allocate another mbuf if there is not enough leading space.

       XXX: for now just prepend header by allocating mbuf.  
    *)
    packet := Mbuf.m_prepend(data,header_len,Mbuf.M_WAIT);

    (* set the icmp header fields *)
    WITH header_buf = Mbuf.Array(packet) DO

      WITH header = VIEW(header_buf^,IcmpPktFormat.T) DO
        header.type := icmp.type;
        header.code := icmp.code;
        (* compute new checksum for icmp packet *)
        header.csum := 0;
        header.csum := Mbuf.Checksum(packet);
      END;
    END;

    IF debug AND  debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"IcmpGen sending ip packet\n");
    END;

    (* send as ip packet - no options *)
    IpGen.PacketSend(ip, packet);
  END PacketSend; 

PROCEDURE Init(<*UNUSED*> verbose:BOOLEAN) = 
  BEGIN
  END Init;

BEGIN
END IcmpGen.

(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

MODULE Icmp6Gen;
IMPORT Icmp6PktFormat, Ip6PktFormat, Ip6Gen, Net, Mbuf, Ctypes;

PROCEDURE PacketSend(
	READONLY ip	: Ip6PktFormat.Header;
	READONLY icmp	: Icmp6PktFormat.Header;
	READONLY data	: Mbuf.T) =

  CONST 
    header_len : Ctypes.unsigned_short = BYTESIZE(Icmp6PktFormat.Header);

  VAR
    packet   : Mbuf.T;
    paylen   : Ctypes.unsigned_int;

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

      (* CAST OPERATION *)
      (* WITH header = VIEW(header_buf[0],Icmp6PktFormat.T) DO *)
      WITH header = VIEW(header_buf^,Icmp6PktFormat.T) DO

        header.type := icmp.type;
        header.code := icmp.code;

        (* compute checksum over overlay header + icmp packet *)
        (* RFC 1883 section 8.1 *)

        paylen := Mbuf.m_length(packet);

        header.csum := 0;
        header.csum := Mbuf.Checksum(packet, Ip6Gen.OverlayChecksum(ip, paylen, Ip6PktFormat.IPPROTO_ICMP6));

      END;
    END;

    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"Icmp6Gen sending ip packet\n");
    END;

    (* send as ip packet - no options *)

    Ip6Gen.PacketSend(ip, packet);
  END PacketSend; 

BEGIN
END Icmp6Gen.

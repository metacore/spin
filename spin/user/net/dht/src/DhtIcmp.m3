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

MODULE DhtIcmp;
IMPORT IpPktFormat, Ip6PktFormat, IcmpPktFormat, Icmp6PktFormat;
IMPORT Ip6Gen;
IMPORT Mbuf, Net;
IMPORT DhtIp;
IMPORT IO;
IMPORT Word, Ctypes; <*NOWARN*>

CONST
  ip_hdr_len      = BYTESIZE(IpPktFormat.T);
  ip6_hdr_len     = BYTESIZE(Ip6PktFormat.T);
  ip6_frg_hdr_len = BYTESIZE(Ip6PktFormat.FragmentHeader);
  icmp_hdr_len    = BYTESIZE(IcmpPktFormat.T);
  icmp6_hdr_len   = BYTESIZE(Icmp6PktFormat.T);

PROCEDURE IP6toIP4(
    new_packet : Mbuf.T;
    <*UNUSED*>
    READONLY ip6 : Ip6PktFormat.T; (* fields in network order *)
    <*UNUSED*>
    READONLY ip4 : IpPktFormat.T;  (* fields in host order *)
    ): Mbuf.T = 
  CONST 
    verbose = FALSE;
  VAR 
    offset           : CARDINAL := 0;
    len              : CARDINAL;
    icmp_error       : BOOLEAN;
    curr             : Mbuf.T := new_packet;
    icmp_packet      : Mbuf.T := new_packet;
    tmp_icmp_packet  : Mbuf.T;
    icmp_err_pkt     : Mbuf.T;
    icmp_err_ip_hdr  : IpPktFormat.T;

  BEGIN

    IF verbose THEN
      IO.Put("DhtIcmp.IP6toIP4 ");
    END;

    tmp_icmp_packet := icmp_packet;
    icmp_packet := NIL;

    (* update icmp type, checksum *)
    (* XXX: does this SUBARRAY work across multiple mbufs? *)
    (* 4 extra bytes to account for field after icmp hdr *)
    WITH icmpBuf = SUBARRAY(Mbuf.Array(tmp_icmp_packet)^,0,icmp6_hdr_len+4),
         icmpHeader = VIEW(icmpBuf,IcmpPktFormat.T),
         icmp6Header = VIEW(icmpBuf,Icmp6PktFormat.T) 
     DO
      (* ICMP error message or non-translated message *)
      icmp_error := TRUE;
      CASE icmp6Header.type OF
      | Icmp6PktFormat.ECHO =>
        IF verbose THEN IO.Put("echo request"); END;
        icmpHeader.type := IcmpPktFormat.ECHO;
        icmp_error := FALSE;
      | Icmp6PktFormat.ECHOREPLY =>
        IF verbose THEN IO.Put("echo reply"); END;
        icmpHeader.type := IcmpPktFormat.ECHOREPLY;
        icmp_error := FALSE;
      | Icmp6PktFormat.DEST_UNREACH =>
        IF verbose THEN IO.Put("dest unreach"); END;
        icmpHeader.type := IcmpPktFormat.DEST_UNREACH;
        CASE icmp6Header.code OF
        | Icmp6PktFormat.NO_ROUTE, Icmp6PktFormat.ADDR_UNREACH =>
          icmpHeader.code := IcmpPktFormat.HOST_UNREACH;
        | Icmp6PktFormat.COMM_PROHIBITED =>
          icmpHeader.code := IcmpPktFormat.HOST_ANO;
        | Icmp6PktFormat.NOT_NEIGHBOR =>
          icmpHeader.code := IcmpPktFormat.SR_FAILED;
        ELSE
          icmpHeader.code := IcmpPktFormat.PORT_UNREACH;
        END;

      | Icmp6PktFormat.PACKET_TOO_BIG =>
        IF verbose THEN IO.Put("packet too big"); END;
        icmpHeader.type := IcmpPktFormat.DEST_UNREACH;
        icmpHeader.code := IcmpPktFormat.FRAG_NEEDED;
        WITH errBuf = SUBARRAY(Mbuf.Array(tmp_icmp_packet)^,icmp6_hdr_len,4),
             err = VIEW(errBuf,IcmpPktFormat.DestinationUnreachable),
             err6 = VIEW(errBuf,Icmp6PktFormat.PacketTooBig)
         DO
          (* adjust MTU by difference between 
             (v6 hdr & v6 frghdr) and ip4 hdr ... should be 28 *)
          err.unused := Net.htonl(Net.nltoh(err6.MTU)-((ip6_hdr_len+ip6_frg_hdr_len)-ip_hdr_len));
        END;

      | Icmp6PktFormat.TIME_EXCEEDED =>
        IF verbose THEN IO.Put("time exceeded"); END;
        icmpHeader.type := IcmpPktFormat.TIME_EXCEEDED;

      | Icmp6PktFormat.PARAMETERPROB =>
        IF verbose THEN IO.Put("parameter prob"); END;
        IF icmp6Header.code = Icmp6PktFormat.ERR_NEXT_HDR THEN
          icmpHeader.type := IcmpPktFormat.DEST_UNREACH;
          icmpHeader.code := IcmpPktFormat.PROT_UNREACH;
        ELSE
          icmpHeader.type := IcmpPktFormat.PARAMETERPROB;
          icmpHeader.code := 0;
          (* update pointer field *)
          WITH errBuf = SUBARRAY(Mbuf.Array(tmp_icmp_packet)^,icmp6_hdr_len,4),
               err = VIEW(errBuf,IcmpPktFormat.ParameterProblemOnDatagram),
               err6 = VIEW(errBuf,Icmp6PktFormat.ParamProblem)
            DO
             (* XXX not complete *)
             VAR pointer := Net.nltoh(err6.pointer);
             BEGIN
               IF pointer >= ip6_hdr_len THEN
                 err.pointer := Net.htons(pointer-(ip6_hdr_len-ip_hdr_len));
               ELSE
                 err.pointer := 0;
               END;
             END;
           END;
        END;
      ELSE
        (* silently drop packet *)
        IF verbose THEN
          IO.Put("drop icmp6 packet \n");
        END;
        Mbuf.m_freem(tmp_icmp_packet);
        RETURN NIL;
      END;

      IF icmp_error = TRUE THEN
        IF verbose THEN IO.Put(" icmp6 error"); END;

        (* translate error invoking packet *)

        (* update curr, offset to point to error invoking packet *)
        WITH currBuf = Mbuf.Array(curr)^
         DO          
          (* 4 extra bytes to account for field between icmp hdr and error packet *)
          INC(offset,icmp6_hdr_len+4);

          (* compute the length of the error packet *)          
          len := Mbuf.m_length(curr);
          DEC(len, offset);
          
          (* XXX need better way to find actual mbuf  *)
          IF offset >= BYTESIZE(currBuf) THEN
            curr := curr.mh_hdr.mh_next;
            <* ASSERT(curr # NIL) *>
            offset := 0;
          END;
        END;

        (* we assume that error invoking packet must have an ip hdr *)
        icmp_err_pkt := DhtIp.IP6toIP4(curr,offset,icmp_err_ip_hdr,TRUE);
        (* icmp_err_pkt contains some of the offending data *)
        IF icmp_err_pkt # NIL THEN
          len := Mbuf.m_length(icmp_err_pkt);        
          (* only include up to ip_hdr_len + 64bits of error packet *)
          len := MIN(len,8);

          (* allocate icmp packet that contains the translated
             offending ip packet. *)
          icmp_packet := Mbuf.m_gethdr(Mbuf.M_WAIT, Mbuf.M_PKTHDR);
          icmp_packet.mh_hdr.mh_len := len+ip_hdr_len+icmp_hdr_len+4;
          (* Mbuf.M_ALIGN(icmp_packet,len+ip_hdr_len+icmp_hdr_len+4); *)

          (* set the fields of the translated offending ip packet *)
          WITH icmp_packet_buf = Mbuf.Array(icmp_packet)^,
               icmp_err_pkt_buf = Mbuf.Array(icmp_err_pkt)^,
               offending_ip_header_buf = SUBARRAY(icmp_packet_buf,icmp_hdr_len+4,ip_hdr_len),
               offending_ip_header = VIEW(offending_ip_header_buf, IpPktFormat.T)
           DO
            offending_ip_header := icmp_err_ip_hdr;
            offending_ip_header.hlen := ip_hdr_len DIV 4; (* we are not setting ip options *)
            offending_ip_header.vers := 4; (* version compatibility *)
            offending_ip_header.tot_len := Net.htons(len+ip_hdr_len);
            (* compute checksum for the ip header packet *)
            offending_ip_header.check := 0;
            offending_ip_header.check := Net.checksum(offending_ip_header_buf,ip_hdr_len,0);

            (* write the 8 bytes at the end of the translated icmp packet *)
            SUBARRAY(icmp_packet_buf, icmp_hdr_len+4 +ip_hdr_len,len) :=
                SUBARRAY(icmp_err_pkt_buf,0,len);
          END;
          Mbuf.m_freem(icmp_err_pkt); (* XXX this will change once DhtIp.IP6toIP4 changes *)
        ELSE
          len := 0;
          icmp_packet := Mbuf.m_gethdr(Mbuf.M_WAIT, Mbuf.M_PKTHDR);
          icmp_packet.mh_hdr.mh_len := len + ip_hdr_len + icmp_hdr_len + 4;
          (* Mbuf.M_ALIGN(icmp_packet,len + ip_hdr_len + icmp_hdr_len + 4); *)
          (* set the fields of the translated offending ip packet *)
          WITH icmp_packet_buf = Mbuf.Array(icmp_packet)^,
               offending_ip_header_buf = SUBARRAY(icmp_packet_buf,icmp_hdr_len+4,ip_hdr_len),
               offending_ip_header = VIEW(offending_ip_header_buf, IpPktFormat.T)
           DO
            offending_ip_header := icmp_err_ip_hdr;
            offending_ip_header.hlen := ip_hdr_len DIV 4; (* we are not setting ip options *)
            offending_ip_header.vers := 4; (* version compatibility *)
            offending_ip_header.tot_len := Net.htons(len+ip_hdr_len);
            (* compute checksum for the ip header packet *)
            offending_ip_header.check := 0;
            offending_ip_header.check := Net.checksum(offending_ip_header_buf,ip_hdr_len,0);
          END;
        END;

        WITH icmp_hdr_buf = SUBARRAY(Mbuf.Array(icmp_packet)^,0,icmp_hdr_len),
             icmp_hdr = VIEW(icmp_hdr_buf, IcmpPktFormat.T),
             top_icmp_hdr_buf = SUBARRAY(Mbuf.Array(tmp_icmp_packet)^,0,icmp_hdr_len),
             top_icmp_hdr = VIEW(top_icmp_hdr_buf, IcmpPktFormat.T) 
         DO
          icmp_hdr.type := top_icmp_hdr.type;
          icmp_hdr.code := top_icmp_hdr.code;
          (* compute new checksum for icmp packet *)
          icmp_hdr.csum := 0;
          icmp_hdr.csum := Mbuf.Checksum(icmp_packet,0);
        END;
        Mbuf.m_freem(tmp_icmp_packet);
      ELSE
        (* XXX must all be in one mbuf. *)
        icmp_packet := tmp_icmp_packet;
        IF verbose THEN IO.Put(" icmp6 msg"); END;
        (* XXX: does this SUBARRAY work across multiple mbufs? *)
        WITH icmpBuf = SUBARRAY(Mbuf.Array(icmp_packet)^,0,icmp_hdr_len),
             icmpHeader = VIEW(icmpBuf,IcmpPktFormat.T)
         DO
          (* compute new checksum for icmp packet *)
          icmpHeader.csum := 0;
          icmpHeader.csum := Mbuf.Checksum(icmp_packet, 0);
        END;
      END;      
    END;
    IF verbose THEN IO.Put("\n"); END;
    RETURN icmp_packet;
  END IP6toIP4; 

PROCEDURE IP4toIP6(
    new_packet : Mbuf.T;
    <*UNUSED*>
    READONLY ip4 : IpPktFormat.T;  (* fields in host order *)
    READONLY ip6 : Ip6PktFormat.T; (* fields in network order *)
    ): Mbuf.T = 
  VAR
    offset           : CARDINAL;
    len              : CARDINAL;
    icmp_error       : BOOLEAN;
    curr             : Mbuf.T := new_packet;
    icmp6_packet     : Mbuf.T := new_packet;
    tmp_icmp_packet  : Mbuf.T;
    icmp_err_pkt     : Mbuf.T;
    icmp_err_pkt_tmp : Mbuf.T;
    icmp_err_ip_hdr  : Ip6PktFormat.T;

  BEGIN
      
    IF FALSE THEN
      IO.Put("got to dht icmp 4 handler\n");
    END;

    tmp_icmp_packet := icmp6_packet;
    icmp6_packet := NIL;

    (* update type and checksum,  *)
    (* XXX: does this SUBARRAY work across multiple mbufs? *)
    WITH icmpBuf = SUBARRAY(Mbuf.Array(tmp_icmp_packet)^,0,icmp_hdr_len),
         icmp6Header = VIEW(icmpBuf,Icmp6PktFormat.T),
         icmpHeader = VIEW(icmpBuf,IcmpPktFormat.T) 
     DO
      (* ICMP error message or non-translated message *)
      icmp_error := TRUE;
      CASE icmpHeader.type OF
        | IcmpPktFormat.ECHO =>
          icmp6Header.type := Icmp6PktFormat.ECHO;
          icmp_error := FALSE;
        | IcmpPktFormat.ECHOREPLY =>
          icmp6Header.type := Icmp6PktFormat.ECHOREPLY;
          icmp_error := FALSE;
        | IcmpPktFormat.DEST_UNREACH =>
          icmp6Header.type := Icmp6PktFormat.DEST_UNREACH;

          (* ZZZ below probably faster with a range check *)
          CASE icmpHeader.code OF
          | IcmpPktFormat.NET_UNREACH, IcmpPktFormat.HOST_UNREACH, 
            IcmpPktFormat.NET_UNKNOWN, IcmpPktFormat.HOST_UNKNOWN,
            IcmpPktFormat.HOST_ISOLATED, IcmpPktFormat.NET_UNR_TOS,
            IcmpPktFormat.HOST_UNR_TOS =>
            icmp6Header.code := Icmp6PktFormat.NO_ROUTE;
          | IcmpPktFormat.PROT_UNREACH =>
            (* translate to v6 ParamProb with Pointer = v6 Next Hdr *)
            icmp6Header.type := Icmp6PktFormat.PARAMETERPROB;
            (* update pointer field *)
            WITH errBuf = SUBARRAY(Mbuf.Array(tmp_icmp_packet)^,icmp_hdr_len,4),
                 err6 = VIEW(errBuf,Icmp6PktFormat.ParamProblem)
             DO
              (* point to next hdr field *)
              err6.pointer := Net.htonl(6);
            END;

          | IcmpPktFormat.PORT_UNREACH =>
            icmp6Header.code := Icmp6PktFormat.PORT_UNREACH;
          | IcmpPktFormat.FRAG_NEEDED =>
            (* translate to ICMPv6 Packet Too Big with Code = 0 *)
            icmp6Header.type := Icmp6PktFormat.PACKET_TOO_BIG;
            icmp6Header.code := 0;
            (* set MTU field *)
            WITH errBuf = SUBARRAY(Mbuf.Array(tmp_icmp_packet)^,icmp_hdr_len,4),
                 err6 = VIEW(errBuf,Icmp6PktFormat.PacketTooBig)
             DO
              (* XXX unclear what to set MTU to since a v4 node doesn't have to implement path MTU discovery *)
              err6.MTU := 1500-(ip6_hdr_len-ip_hdr_len);
            END;
          | IcmpPktFormat.SR_FAILED =>
            icmp6Header.code := Icmp6PktFormat.NOT_NEIGHBOR;
          ELSE
            icmp6Header.code := Icmp6PktFormat.COMM_PROHIBITED;
          END;
        | IcmpPktFormat.TIME_EXCEEDED =>
          icmp6Header.type := Icmp6PktFormat.TIME_EXCEEDED;
          
        | IcmpPktFormat.PARAMETERPROB =>
          icmp6Header.type := Icmp6PktFormat.PARAMETERPROB;
          (* update pointer field *)
          WITH errBuf = SUBARRAY(Mbuf.Array(tmp_icmp_packet)^,icmp_hdr_len,4),
               err = VIEW(errBuf,IcmpPktFormat.ParameterProblemOnDatagram),
               err6 = VIEW(errBuf,Icmp6PktFormat.ParamProblem)
           DO
            (* XXX not complete *)
            IF err.pointer >= ip_hdr_len THEN
              err6.pointer := err.pointer+(ip6_hdr_len-ip_hdr_len);
            ELSE
              err6.pointer := 0;
            END;
          END;
        ELSE
          (* silently drop *)
          Mbuf.m_freem(tmp_icmp_packet);
          RETURN NIL;
        END;

      IF icmp_error = TRUE THEN

        (* translate error invoking packet *)

        (* update curr, offset to point to error invoking packet *)
        WITH currBuf = Mbuf.Array(curr)^
         DO          
          (* 4 extra bytes to account for field between icmp hdr and error packet *)
          INC(offset,icmp6_hdr_len+4);

          (* compute the length of the error packet *)          
          len := Mbuf.m_length(curr);
          DEC(len, offset);
          
          (* XXX need better way to find actual mbuf  *)
          IF offset >= BYTESIZE(currBuf) THEN
            curr := curr.mh_hdr.mh_next;
            <* ASSERT(curr # NIL) *>
            offset := 0;
          END;
        END;

        (* we assume that error invoking packet must have an ip hdr *)
        icmp_err_pkt_tmp := DhtIp.IP4toIP6(curr,offset,icmp_err_ip_hdr,TRUE);
        IF icmp_err_pkt_tmp = NIL THEN
          IO.Put("DhtIcmp.IP4toIP6 failure.\n");
          Mbuf.m_freem(tmp_icmp_packet);
          RETURN NIL;
        END;
        
        len := Mbuf.m_length(icmp_err_pkt_tmp);        
        (* include as much of error packet such that total icmp packet <= 576 bytes *)
        (* XXX this doesn't account for extension headers! *)
        IF (len+ip6_hdr_len+icmp6_hdr_len+4) > 576 THEN
          len := 576-ip6_hdr_len-icmp6_hdr_len-4;
        END;

        icmp_err_pkt_tmp := Mbuf.M_PREPEND(icmp_err_pkt_tmp,ip6_hdr_len,Mbuf.M_WAIT);

        WITH header_buf = SUBARRAY(Mbuf.Array(icmp_err_pkt_tmp)^,0,ip6_hdr_len),
             header = VIEW(header_buf, Ip6PktFormat.T)
         DO
          (* set the version number *)
          header.vers := 16_6;
          (* set priority *)
          header.prio := icmp_err_ip_hdr.prio;
          (* set flow *)
          header.flow := icmp_err_ip_hdr.flow;        
          (* set payload length *)
          header.payload := Net.htons(len);
          (* set next header type *)
          header.next_head := icmp_err_ip_hdr.next_head;
          (* set hop limit *)
          header.hop_limit := icmp_err_ip_hdr.hop_limit;
          
          (* set the src/dst fields for the ip packet *)
          header.daddr := icmp_err_ip_hdr.daddr;
          header.saddr := icmp_err_ip_hdr.saddr;
          
        END;
        
        icmp_err_pkt := Mbuf.m_copym(icmp_err_pkt_tmp,0,len+ip6_hdr_len,Mbuf.M_WAIT); 
        icmp6_packet := Mbuf.M_PREPEND(icmp_err_pkt, icmp6_hdr_len+4, Mbuf.M_WAIT);

        WITH icmp_hdr_buf = SUBARRAY(Mbuf.Array(icmp6_packet)^,0,icmp6_hdr_len),
             icmp_hdr = VIEW(icmp_hdr_buf, Icmp6PktFormat.T) DO
          WITH top_icmp_hdr_buf = SUBARRAY(Mbuf.Array(tmp_icmp_packet)^,0,icmp6_hdr_len),
               top_icmp_hdr = VIEW(top_icmp_hdr_buf, Icmp6PktFormat.T) DO
            icmp_hdr.type := top_icmp_hdr.type;
            icmp_hdr.code := top_icmp_hdr.code;
            (* compute new checksum for icmp packet *)
            icmp_hdr.csum := 0;
            icmp_hdr.csum := 
                Mbuf.Checksum(icmp6_packet, 
                              Ip6Gen.OverlayChecksum(ip6, 
                                                     Mbuf.m_length(icmp6_packet), 
                                                     Ip6PktFormat.IPPROTO_ICMP6));
          END;
        END;
        
        RETURN icmp6_packet;

      END;

      icmp6_packet := tmp_icmp_packet;
      (* XXX: does this SUBARRAY work across multiple mbufs? *)
      WITH icmp6Buf = SUBARRAY(Mbuf.Array(icmp6_packet)^,0,icmp6_hdr_len),
           icmp6Header = VIEW(icmp6Buf,Icmp6PktFormat.T)
       DO
        (* compute new checksum for icmp packet *)
        icmp6Header.csum := 0;
        icmp6Header.csum := 
            Mbuf.Checksum(icmp6_packet, 
                          Ip6Gen.OverlayChecksum(ip6, 
                                                 Mbuf.m_length(icmp6_packet), 
                                                 Ip6PktFormat.IPPROTO_ICMP6));
      END;

    END;

    RETURN icmp6_packet; 
  END IP4toIP6;

BEGIN
END DhtIcmp.





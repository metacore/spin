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

MODULE DhtIp;
IMPORT IpPktFormat, Ip6PktFormat, TcpPktFormat, UdpPktFormat;
IMPORT Dht, DhtMapping, DhtIcmp;
IMPORT Mbuf, Net;
IMPORT NetText;
IMPORT IO, Ctypes, Word, Fmt;

CONST
  ip_hdr_len   = BYTESIZE(IpPktFormat.Header);
  ip6_hdr_len  = BYTESIZE(Ip6PktFormat.T);
  frag_hdr_len = BYTESIZE(Ip6PktFormat.FragmentHeader);
  tcp_hdr_len  = BYTESIZE(TcpPktFormat.Header);
  udp_hdr_len  = BYTESIZE(UdpPktFormat.Header);

PROCEDURE IP6toIP4(
    VAR curr       : Mbuf.T; 
    VAR offset     : CARDINAL;
    VAR ip4        : IpPktFormat.T;
    reverse_lookup : BOOLEAN) : Mbuf.T =
  VAR
    newPkt         : Mbuf.T;
    payload        : Ctypes.unsigned_short;
    ip6_H_frag_off : Ctypes.unsigned_short;
    proto          : Ctypes.unsigned_char;
    len            : CARDINAL;
  BEGIN
    IF FALSE THEN
      IO.Put("got to dht ip 6 handler\n");
    END;

    WITH currBuf = Mbuf.Array(curr)^,
         ip6Data = SUBARRAY(currBuf,offset,ip6_hdr_len),
         ip6 = VIEW(ip6Data,Ip6PktFormat.T)
     DO

      (* save protocol for later *)
      proto := ip6.next_head;        
      payload := Net.nstoh(ip6.payload);

      (* set ip info *)
      ip4.hlen    := 5; (* 20 byte ip header *)
      ip4.tos     := 16_00; (* XXX map from v6 prio/flow field? *)
      ip4.ttl     := ip6.hop_limit;


      (* icmp error packet translation needs reverse lookup *)
      IF reverse_lookup # TRUE THEN

        (* check if v6 saddr a IPv4-compatible IPv6 address *)
        IF ip6.saddr[0] = 0 AND 
           ip6.saddr[1] = 0 AND
           ip6.saddr[2] = 0 THEN
          ip4.saddr := VIEW(ip6.saddr[3],IpPktFormat.Address);
        ELSIF DhtMapping.ip6node6TOip4node6.get(ip6.saddr,ip4.saddr) = FALSE THEN
            IO.Put("DhtIp.IP6PA can't map src ADR ");
            IO.Put(Dht.FmtIp(ip6.saddr));
            IO.Put("\n");
            RETURN NIL;
        END;
        
        IF FALSE THEN
        IO.Put(Fmt.Unsigned(ip6.daddr[3]));
        IO.Put(":");
        IO.Put(Fmt.Unsigned(ip6.daddr[2]));
        IO.Put(":");
        IO.Put(Fmt.Unsigned(ip6.daddr[1]));
        IO.Put(":");
        IO.Put(Fmt.Unsigned(ip6.daddr[0]));
        IO.Put("\n");
        END;
        (* check if v6 daddr a IPv4-mapped IPv6 address *)
        IF ip6.daddr[0] = 0 AND
           ip6.daddr[1] = 0 AND
           ip6.daddr[2] = 16_FFFF0000 THEN
          ip4.daddr := VIEW(ip6.daddr[3],IpPktFormat.Address);
        ELSIF DhtMapping.ip6node4TOip4node4.get(ip6.daddr,ip4.daddr) = FALSE THEN
            IO.Put("DhtIp.IP6PA can't map dst adr ");
            IO.Put(Dht.FmtIp(ip6.daddr));
            IO.Put("\n");
            RETURN NIL;
        END;


      ELSE
        (* check if v6 saddr a IPv4-compatible IPv6 address *)
        IF ip6.saddr[0] = 0 AND 
           ip6.saddr[1] = 0 AND
           ip6.saddr[2] = 0 THEN
          ip4.saddr := VIEW(ip6.saddr[3],IpPktFormat.Address);
        ELSIF DhtMapping.ip6node4TOip4node4.get(ip6.saddr,ip4.saddr) = FALSE THEN
            IO.Put("DhtIp.IP6PA can't map src ADR ");
            IO.Put(Dht.FmtIp(ip6.saddr));
            IO.Put("\n");
            RETURN NIL;
          END;

        IF ip6.daddr[0] = 0 AND
           ip6.daddr[1] = 0 AND
           ip6.daddr[2] = 16_FFFF0000 THEN
          ip4.daddr := VIEW(ip6.daddr[3],IpPktFormat.Address);
        ELSIF DhtMapping.ip6node6TOip4node6.get(ip6.daddr,ip4.daddr) = FALSE THEN
          IO.Put("DhtIp.IP6PA can't map dst adr ");
          IO.Put(Dht.FmtIp(ip6.daddr));
          IO.Put("\n");
          RETURN NIL;
        END;
      END;

      (* increment offset beyond ip6 header *)
      INC(offset,ip6_hdr_len);

      (* translate fragment fields if this packet is a fragment *)
      IF proto = Ip6PktFormat.IPPROTO_EXT_FRAG THEN
        (* compute the new ip6_H_frag_off value and set the ipv4 id and
           ip6_H_frag_off value. *)
        WITH frag = SUBARRAY(Mbuf.Array(curr)^,offset,frag_hdr_len),
             frag_hdr = VIEW(frag,Ip6PktFormat.FragmentHeader)
         DO
          VAR 
            frag_flags : Ctypes.unsigned_short;
          BEGIN
            (* only lower 16 bits of v6 id is carried over to v4 id *)
            ip4.id := Net.htons(Word.And(Net.nltoh(frag_hdr.id),16_0000ffff));

            ip6_H_frag_off := Net.nstoh(frag_hdr.offset);
            frag_flags  := Word.LeftShift(Word.And(ip6_H_frag_off,16_0007),13);
            ip4.frag_off := Net.htons(Word.Or(Word.RightShift(ip6_H_frag_off,3),frag_flags));

            (* point to the real lower layer packet *)
            proto := frag_hdr.next_head;
          END;
        END;
        ip4.tot_len := payload - frag_hdr_len + ip_hdr_len;
        (* increment offset beyond the fragment header *)
        INC(offset,frag_hdr_len);
      ELSE
        ip4.id         := 16_00; (* 0 should be ok because we set DF bit *)
        ip4.frag_off   := Net.htons(IpPktFormat.IP_DF); (* set dont fragment bit *)
        ip4.tot_len    := payload + ip_hdr_len;
        ip6_H_frag_off := 0;
      END;

      (* update curr, offset to point to lower layer packet *)
      (* XXX need better way to find actual mbuf  *)
      IF offset >= BYTESIZE(currBuf) THEN
        curr := curr.mh_hdr.mh_next;
        <* ASSERT(curr # NIL) *>
        offset := 0;
      END;

      (* use the length from the packet *)
      len := ip4.tot_len - (ip4.hlen * 4);
      len := MIN(len,Mbuf.m_length(curr)-offset);
      (* copy data into its own mbuf chain *)
      newPkt := Mbuf.m_copym(curr,offset,len,Mbuf.M_WAIT);

      (* process lower layer packet *)
      CASE proto OF
      | Ip6PktFormat.IPPROTO_TCP =>
        IF FALSE THEN
          IO.Put("tcp\n");
        END;
        ip4.protocol := IpPktFormat.IPPROTO_TCP;

        (* 16_fff8 = 13 bit ip6_H_frag_off value. lower 3 bits are flags *)
        IF Word.And(ip6_H_frag_off,16_fff8) = 0 THEN
          WITH tcpHeaderBuf = SUBARRAY(Mbuf.Array(newPkt)^,0,tcp_hdr_len),
               tcpHeader = VIEW(tcpHeaderBuf,TcpPktFormat.NewT) 
           DO
            tcpHeader.check := Dht.IP6toIP4Checksum(ip6,ip4,tcpHeader.check);
          END;
        END;

      | Ip6PktFormat.IPPROTO_UDP =>
        IF FALSE THEN
          IO.Put("udp\n");
        END;
        ip4.protocol := IpPktFormat.IPPROTO_UDP;

        (* 16_fff8 = 13 bit ip6_H_frag_off value. lower 3 bits are flags *)
        IF Word.And(ip6_H_frag_off,16_fff8) = 0 THEN
          WITH udpHeaderBuf = SUBARRAY(Mbuf.Array(newPkt)^,0,udp_hdr_len),
               udpHeader = VIEW(udpHeaderBuf,UdpPktFormat.NewT) 
           DO
            udpHeader.check := Dht.IP6toIP4Checksum(ip6,ip4,udpHeader.check);
          END;
        END;

      | Ip6PktFormat.IPPROTO_ICMP6 =>
        IF FALSE THEN
          IO.Put("icmp6\n");
        END;
        ip4.protocol := IpPktFormat.IPPROTO_ICMP;

        (* we're done if this is a non-head fragment *)
        (* 16_fff8 = 13 bit ip6_H_frag_off value. lower 3 bits are flags *)
        IF Word.And(ip6_H_frag_off,16_fff8) = 0 THEN
          newPkt := DhtIcmp.IP6toIP4(newPkt,ip6,ip4);
        END;
      ELSE
        (* XXX need to handle this! *)
        IO.Put("unexpected protocol: ");
        IO.PutInt(proto);
        IO.Put("\n");
        (* clean up *)
        Mbuf.m_freem(newPkt);
        RETURN NIL;
      END;
    END;
    RETURN newPkt; 
  END IP6toIP4; 


PROCEDURE IP4toIP6(
    VAR curr           : Mbuf.T; 
    VAR offset         : CARDINAL;
    VAR ip6            : Ip6PktFormat.T;
    reverse_lookup     : BOOLEAN) : Mbuf.T =
  VAR 
    newPkt         : Mbuf.T := NIL;
    proto          : Ctypes.unsigned_char;
    ip4_H_frag_off : Ctypes.unsigned_short;
    frag_id        : Ctypes.unsigned_short;
    len            : CARDINAL;
  BEGIN
      
    IF FALSE THEN
      IO.Put("got to dht ip 4 handler\n");
    END;

    (* NOTE that for the WITH statement below 
       - ip4 16-bit and 32-bit fields will be in network order.
       - ip6 16-bit and 32-bit fields will be in host order.
    *)

    (* set ip info *)
    WITH currBuf = Mbuf.Array(curr)^,
         ip4Data = SUBARRAY(currBuf,offset,ip_hdr_len),
         ip4 = VIEW(ip4Data,IpPktFormat.T)
     DO
      (* save protocol for later *)
      proto := ip4.protocol;
      (* note if this is a frag packet *)
      ip4_H_frag_off := Net.nstoh(ip4.frag_off);
      (* 16_3fff = Word.Or(IP_MF,IP_OFFSET); *)
      IF Word.And(ip4_H_frag_off,16_3fff) # 0 THEN
        frag_id := Net.nstoh(ip4.id);
      END;

      
      (* XXX: what should we set prio to?  
         i don't think ipv4 and ipv6 prio's 
         are semantically equivalent (vkl) *)
      ip6.prio      := 16_0;      (* no standard use for prio, yet *)
      ip6.flow      := 16_000000; (* no standard use for flow label, yet *)
      ip6.hop_limit := ip4.ttl;   (* set v6 hop_limit to v4 ttl *)
      ip6.payload   := Net.nstoh(ip4.tot_len) - ip_hdr_len;

      (* XXX stateless translation *)
      IF FALSE THEN
        ip6.saddr := Ip6PktFormat.Address{0,0,Net.htonl(16_0000ffff),ip4.saddr}; (* mapped *)
        ip6.daddr := Ip6PktFormat.Address{0,0,0,ip4.daddr}; (* compatible *)
      END;

      (* icmp error packet translation needs to reverse lookups *)
      IF reverse_lookup # TRUE THEN
        IF DhtMapping.ip4node4TOip6node4.get(ip4.saddr,ip6.saddr) = FALSE THEN
          Dht.CreateIP6NODE4addr(ip4.saddr,ip6.saddr);
          IF FALSE THEN
            IO.Put("DhtIp.IP4PA can't map src adr ");
            IO.Put(NetText.FmtIp(ip4.saddr));
            IO.Put("\n");
            RETURN NIL;
          END;
        END;
        IF DhtMapping.ip4node6TOip6node6.get(ip4.daddr,ip6.daddr) = FALSE THEN
          IO.Put("DhtIp.IP4PA can't map dst adr ");
          IO.Put(NetText.FmtIp(ip4.daddr));
          IO.Put("\n");
          RETURN NIL;
        END;

      ELSE
        IF DhtMapping.ip4node6TOip6node6.get(ip4.saddr,ip6.saddr) = FALSE THEN
          Dht.CreateIP6NODE4addr(ip4.saddr,ip6.saddr);
          IF FALSE THEN
            IO.Put("DhtIp.IP4PA can't map src adr ");
            IO.Put(NetText.FmtIp(ip4.saddr));
            IO.Put("\n");
            RETURN NIL;
          END;
        END;
        IF DhtMapping.ip4node4TOip6node4.get(ip4.daddr,ip6.daddr) = FALSE THEN
          IO.Put("DhtIp.IP4PA can't map dst adr ");
          IO.Put(NetText.FmtIp(ip4.daddr));
          IO.Put("\n");
          RETURN NIL;
        END;
      END;

      (* increment current offset into mbuf by ipv4 header length *)
      INC(offset,ip4.hlen*4);
      (* XXX need better way to find actual mbuf  *)
      IF offset >= BYTESIZE(currBuf) THEN
        curr := curr.mh_hdr.mh_next;
        <* ASSERT(curr # NIL) *>
        offset := 0;
      END;

      (* use the length from the packet *)
      len := ip6.payload;
      len := MIN(len,Mbuf.m_length(curr)-offset);

      (* copy payload *)
      newPkt := Mbuf.m_copym(curr,offset,len,Mbuf.M_WAIT); 

      (* process higher layer packet *)
      CASE proto OF
      | IpPktFormat.IPPROTO_ICMP =>
        
        ip6.next_head := Ip6PktFormat.IPPROTO_ICMP6;

        (* we're done if this is a non-head fragment *)
        (* 16_1fff = IP_OFFSET; *)
        IF Word.And(ip4_H_frag_off,16_1fff) = 0 THEN
          newPkt := DhtIcmp.IP4toIP6(newPkt,ip4,ip6);
        END;

        (* add v6 fragment extension if this packet is a fragment *)
        IF Word.And(ip4_H_frag_off,16_3fff) # 0 THEN
          AddFragExtension(frag_id,ip4_H_frag_off,ip6,newPkt);
        END;

      | IpPktFormat.IPPROTO_TCP =>
        IF FALSE THEN
          IO.Put("tcp)\n");
        END;

        ip6.next_head := Ip6PktFormat.IPPROTO_TCP;

        (* adjust Udp checksum for non-fragment/head-fragment pkts *)
        IF Word.And(ip4_H_frag_off,16_1fff) = 0 THEN
          WITH tcpHeaderBuf = SUBARRAY(Mbuf.Array(newPkt)^,0,tcp_hdr_len),
               tcpHeader = VIEW(tcpHeaderBuf,TcpPktFormat.NewT) 
           DO
            tcpHeader.check := Dht.IP4toIP6Checksum(ip4,ip6,tcpHeader.check);
          END;
        END;
        (* add v6 fragment extension if this packet is a fragment *)
        IF Word.And(ip4_H_frag_off,16_3fff) # 0 THEN
          AddFragExtension(frag_id,ip4_H_frag_off,ip6,newPkt);
        END;

      | IpPktFormat.IPPROTO_UDP =>
        IF FALSE THEN
          IO.Put("udp)\n");
        END;
        
        ip6.next_head := Ip6PktFormat.IPPROTO_UDP;

        (* adjust Udp checksum for non-fragment/head-fragment pkts *)
        IF Word.And(ip4_H_frag_off,16_1fff) = 0 THEN
          WITH udpHeaderBuf = SUBARRAY(Mbuf.Array(newPkt)^,0,udp_hdr_len),
               udpHeader = VIEW(udpHeaderBuf,UdpPktFormat.NewT) 
           DO
            udpHeader.check := Dht.IP4toIP6Checksum(ip4,ip6,udpHeader.check);
          END;
        END;
        (* add v6 fragment extension if this packet is a fragment *)
        IF Word.And(ip4_H_frag_off,16_3fff) # 0 THEN
          AddFragExtension(frag_id,ip4_H_frag_off,ip6,newPkt);
        END;

      ELSE
        (* XXX need to handle this! *)
        IO.Put("unexpected protocol: " & Fmt.Unsigned(proto) & "\n");
        (* clean up *)
        Mbuf.m_freem(newPkt);
        RETURN NIL;
      END;
    END;
    RETURN newPkt; 
  END IP4toIP6;

PROCEDURE AddFragExtension(
    ip4_frag_id  : Ctypes.unsigned_short;
    ip4_frag_off : Ctypes.unsigned_short;
    VAR ip6      : Ip6PktFormat.T;
    VAR newPkt   : Mbuf.T) =
  VAR
    frag_flags : Word.T;
  BEGIN
    newPkt := Mbuf.M_PREPEND(newPkt,frag_hdr_len,Mbuf.M_WAIT);
    WITH frag = SUBARRAY(Mbuf.Array(newPkt)^,0,frag_hdr_len),
         frag_hdr = VIEW(frag,Ip6PktFormat.FragmentHeader)
     DO
      frag_hdr.next_head := ip6.next_head;
      frag_hdr.reserved := 0;
      (* note that v6 id is 32 bits, v4 id is 16 bits *)
      frag_hdr.id := Net.htonl(ip4_frag_id);

      (* flags = the first 3 bits of 16 bit offset *)
      frag_flags := Word.And(Word.RightShift(ip4_frag_off,13),16_0007);
      frag_hdr.offset := Net.htons(Word.Or(Word.LeftShift(Word.And(ip4_frag_off,16_1fff),3),frag_flags));
    END;
    ip6.next_head := Ip6PktFormat.IPPROTO_EXT_FRAG;
    ip6.payload   := ip6.payload + frag_hdr_len;
  END AddFragExtension;

BEGIN
END DhtIp.

(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new spin shell style.
 *
 *)

MODULE IcmpClassification;

IMPORT IO, Mbuf, MbufPublic, Net;
IMPORT IcmpGen, IcmpPktFormat, IpPktFormat, Icmp;

VAR echo: REFANY;

CONST
  ip_hdr_len = BYTESIZE(IpPktFormat.T);
  icmp_hdr_len = BYTESIZE(IcmpPktFormat.T);
  ip_icmp_hdr_len = ip_hdr_len + icmp_hdr_len;
CONST
  debug = FALSE;

FUNCTIONAL
PROCEDURE Guard_ECHO(
    <* UNUSED *> packet : Mbuf.T;
    curr                : Mbuf.T; 
    offset              : CARDINAL): BOOLEAN =
  BEGIN
    WITH icmpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,icmp_hdr_len),
         icmpHeader = VIEW(icmpHeaderBuf,IcmpPktFormat.T) 
     DO
      RETURN icmpHeader.type = IcmpPktFormat.ECHO;
    END;
  END Guard_ECHO; 

PROCEDURE PacketArrived_ECHO(
    packet: Mbuf.T;
    curr: Mbuf.T; 
    offset: CARDINAL):
  BOOLEAN =
  VAR
    ip		: IpPktFormat.T;
    icmp	: IcmpPktFormat.T;
    data, m	: Mbuf.T;
    len         : CARDINAL;
    dstPos,srcPos   : CARDINAL;
  BEGIN
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"IcmpEchoRequest ");
    END;

    (* get the ip address to send the packet back to *)
    WITH origpkt = Mbuf.Array(packet) ,
         ip_header = VIEW(origpkt^,IpPktFormat.T) DO

      (* flip ip address *)
      ip          := ip_header; (* XXX bad! *)
      ip.saddr    := ip_header.daddr;
      ip.daddr    := ip_header.saddr;
      ip.protocol := IpPktFormat.IPPROTO_ICMP;
      ip.tot_len  := 0; (* Net.nstoh(ip_header.tot_len); *)  (* send back same ip id field *)
      
      (* 
         Do not set remaining ip fields. They are either ignored or
         reset by the ip layer.
      *)
    END;

    WITH icmpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,icmp_hdr_len),
         icmpHeader = VIEW(icmpHeaderBuf,IcmpPktFormat.T) 
     DO
      (* copy code field from icmp header *)
      icmp.code := icmpHeader.code;
      (* send back as echo reply icmp packet *)
      icmp.type  := IcmpPktFormat.ECHOREPLY;
    END;

    (* compute the length of the icmp packet data *)
    len := Mbuf.m_length(packet);
    DEC(len,ip_icmp_hdr_len);

    (* copy data into its own mbuf chain *)
    m := Mbuf.m_gethdr(Mbuf.M_DONTWAIT, Mbuf.MT_DATA);
    IF len < Mbuf.MHLEN THEN
      m.mh_hdr.mh_len := len;
      (* Slow copy from mbuf chain to contiguous buffer -  ugh *)
      srcPos := ip_icmp_hdr_len; (* start copying after the headers *)
      dstPos := 0;
      WHILE packet # NIL DO
        WITH srcBuf = Mbuf.Array(packet)^,
             srcSize = BYTESIZE(srcBuf)-srcPos,
             dstBuf = Mbuf.Array(m)^
         DO
          IF srcSize >= 0 THEN
            SUBARRAY(dstBuf,dstPos,srcSize) := SUBARRAY(srcBuf,srcPos,srcSize);
            srcPos := 0;
          ELSE
            DEC(srcPos,BYTESIZE(srcBuf));
          END;
          INC(dstPos,srcSize);
        END;
        packet := packet.mh_hdr.mh_next;
      END;
    ELSE
      m.mh_hdr.mh_len := 0;
      Mbuf.MH_ALIGN(m,Mbuf.MHLEN);
      data := Mbuf.m_copym(packet,ip_icmp_hdr_len,len,Mbuf.M_WAIT);
      m.mh_hdr.mh_next := data;
    END;
    MbufPublic.SetPktHdrLen(m,len);
    
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"IcmpEchoRequest sending icmp packet\n");
    END;

    (* send it over IP -- needs to become a event raise rather than a proc call *)
    IcmpGen.PacketSend(ip,icmp,m);
    RETURN FALSE; 
  END PacketArrived_ECHO;

PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
    (* install echo handlers *)
    echo := Icmp.Install(Icmp.PacketArrived,
                         Guard_ECHO,
                         PacketArrived_ECHO);
    IF verbose THEN IO.Put("IcmpClassification module initialized.\n"); END;
  END Init;

BEGIN
END IcmpClassification.  

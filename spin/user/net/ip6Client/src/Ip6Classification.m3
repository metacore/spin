(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

MODULE Ip6Classification;
IMPORT IO, Icmp6, Ip6, Ip6PktFormat, Mbuf, Net;

VAR 
  icmp: REFANY;
CONST
  ip_hdr_len = BYTESIZE(Ip6PktFormat.T);

FUNCTIONAL
PROCEDURE Guard_ICMP6(
    <* UNUSED *> packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN =
  BEGIN
    WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,ip_hdr_len),
         ipHeader = VIEW(ipHeaderBuf,T)
     DO
      RETURN ipHeader.next_head = Ip6PktFormat.IPPROTO_ICMP6 AND (* ICMPv6 packet ? *)
      ipHeader.vers = 6 AND (* IP version 6  ? *)
      (* check for fragmentation? *)
      TRUE;
    END;
  END Guard_ICMP6;

PROCEDURE PacketArrived_ICMP6(
    packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN =
  BEGIN
    WITH currBuf = Mbuf.Array(curr)^ DO
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level,Net.oLevel.INFO,"IpCl.ICMP.PA ");
      END;
      (* IPv6 headers are fixed length *)
      INC(offset,ip_hdr_len);
      (* XXX need better way to find actual mbuf  *)
      IF offset >= BYTESIZE(currBuf) THEN
        curr := curr.mh_hdr.mh_next;
        <* ASSERT(curr # NIL) *>
        offset := 0;
      END;
      EVAL Icmp6.PacketArrived(packet,curr,offset);
    END;
    RETURN FALSE;
  END PacketArrived_ICMP6;

PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
    icmp := Ip6.Install(Ip6.PacketArrived,
                        Guard_ICMP6,
                        PacketArrived_ICMP6);
    IF verbose THEN IO.Put("Ip6Classification initialized.\n"); END;
  END Init;

PROCEDURE Uninit(verbose:BOOLEAN) = 
  BEGIN
    IF icmp # NIL THEN Ip6.Uninstall(icmp); icmp := NIL; END;
    IF verbose THEN IO.Put("Ip6Classification unloaded.\n"); END;
  END Uninit;

BEGIN
END Ip6Classification. 

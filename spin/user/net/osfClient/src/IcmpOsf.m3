(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 09-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	created.
 *
 *)

UNSAFE
MODULE IcmpOsf;
IMPORT IO, Mbuf, IcmpPktFormat, UdpPktFormat, IpPktFormat, OsfNet,
       Icmp, SocketAddrIn, Ctypes, Net;

CONST icmp_hdr_len = BYTESIZE(IcmpPktFormat.T);

VAR unreachable : REFANY;

FUNCTIONAL
PROCEDURE Guard_DEST_UNREACH(
    <* UNUSED *> packet : Mbuf.T;
    curr                : Mbuf.T; 
    offset              : CARDINAL): BOOLEAN =
  BEGIN
    WITH icmpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,icmp_hdr_len),
         icmpHeader = VIEW(icmpHeaderBuf,IcmpPktFormat.T) 
     DO
      RETURN icmpHeader.type = IcmpPktFormat.DEST_UNREACH;
    END;
  END Guard_DEST_UNREACH;

TYPE IcmpPortUnreachT = RECORD
  header : IcmpPktFormat.T;
  icmp_hun: ARRAY [1..BYTESIZE(Ctypes.unsigned_int)] OF Net.BYTE;
  icmp_ip: IpPktFormat.T;
  icmp_udp: UdpPktFormat.NewT;
END;

PROCEDURE PacketArrived_DEST_UNREACH(
    <* UNUSED *> packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):
  BOOLEAN =
  CONST
    len = BYTESIZE(IcmpPortUnreachT);
  VAR
    icmpsrc: SocketAddrIn.T;
  BEGIN
    WITH icmpPortUnreachBuf = SUBARRAY(Mbuf.Array(curr)^,offset,len),
         icmpPortUnreach = VIEW(icmpPortUnreachBuf,IcmpPortUnreachT)
     DO
      IF icmpPortUnreach.icmp_ip.protocol = IpPktFormat.IPPROTO_UDP THEN
        icmpsrc.sin_addr := VIEW(icmpPortUnreach.icmp_ip.daddr, Ctypes.unsigned_int);
        OsfNet.UdpCtlInput(icmpPortUnreach.header.code, icmpsrc, ADR(icmpPortUnreach.icmp_ip));
      ELSE
        IO.Put("Icmp Dest Unreach not for UDP.\n");
      END;
    END;
    RETURN TRUE;
  END PacketArrived_DEST_UNREACH;

PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
    (* install echo handlers *)
    unreachable := Icmp.Install(Icmp.PacketArrived,
                                Guard_DEST_UNREACH,
                                PacketArrived_DEST_UNREACH);
    IF verbose THEN 
      IO.Put("IcmpOsf module initialized.\n");
    END;
  END Init;

BEGIN
END IcmpOsf.

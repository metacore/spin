(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Don't free mbuf here
 *
 * 25-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Removed "ping" interface.
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created for simple tcp.
 *)

MODULE StcpIpPacket;

IMPORT StcpIpPktFormat;
IMPORT StcpMbuf, StcpNet;
IMPORT StcpEtherPacket;
IMPORT StcpEtherDev;
IMPORT StcpTcpPacket;
IMPORT StcpUtils;
IMPORT IO;
IMPORT StcpSocketAddr;
IMPORT DefaultAddr;
IMPORT Ctypes ;
IMPORT IcmpPing ;<*NOWARN*>
IMPORT Fmt;

(* ---------------------------------------------------------------- *)
CONST
  ip_hdr_len = BYTESIZE(StcpIpPktFormat.Header);

(* ---------------------------------------------------------------- *)
PROCEDURE PrintIpHeader(ipHeader : StcpIpPktFormat.T) =
  PROCEDURE GetProtocol(protocol : Ctypes.unsigned_char) : TEXT =
    BEGIN
      CASE protocol OF
        | StcpIpPktFormat.IPPROTO_IP => RETURN "IPPROTO_IP";
        | StcpIpPktFormat.IPPROTO_ICMP => RETURN "IPPROTO_ICMP";
        | StcpIpPktFormat.IPPROTO_TCP => RETURN "IPPROTO_TCP";
        | StcpIpPktFormat.IPPROTO_EGP => RETURN "IPPROTO_EGP";
        | StcpIpPktFormat.IPPROTO_PUP => RETURN "IPPROTO_PUP";
        | StcpIpPktFormat.IPPROTO_UDP => RETURN "IPPROTO_UDP";
        | StcpIpPktFormat.IPPROTO_IDP => RETURN "IPPROTO_IDP";
        | StcpIpPktFormat.IPPROTO_RAW => RETURN "IPPROTO_RAW";
      ELSE
	RETURN "Unknwon IP protocol";
      END;
    END GetProtocol;

  BEGIN
    IO.Put("IP header : ");
    IO.Put("  vers " & Fmt.Int(ipHeader.vers));
    IO.Put("  hlen " & Fmt.Int(ipHeader.hlen));
    IO.Put("  tos: " & Fmt.Int(ipHeader.tos));
    IO.Put("  length: " & Fmt.Int(StcpNet.nstoh(ipHeader.tot_len)));
    IO.Put("  id: " & Fmt.Int(StcpNet.nstoh(ipHeader.id)) & "\n");
    IO.Put("  ttl: " & Fmt.Int(ipHeader.ttl));
    IO.Put("  check: " & Fmt.Int(StcpNet.nstoh(ipHeader.check), 16));
    StcpUtils.PrintArrayOfChar(VIEW(ipHeader.saddr, ARRAY [1..4] OF CHAR),
			4, 10, FALSE);
    IO.Put(" --> ");
    StcpUtils.PrintArrayOfChar(VIEW(ipHeader.daddr, ARRAY [1..4] OF CHAR),
			4, 10, FALSE);
    IO.Put("  " & GetProtocol(ipHeader.protocol) & "\n");
  END PrintIpHeader;

PROCEDURE PacketGuard(m, cur : StcpMbuf.T ; offset : CARDINAL) : BOOLEAN =
  BEGIN

    WHILE (cur.mh_hdr.mh_len < offset+ip_hdr_len) AND
	  (cur.mh_hdr.mh_next # NIL) DO
      cur := cur.mh_hdr.mh_next;
      offset := 0;
    END;
    IF cur.mh_hdr.mh_len < offset+ip_hdr_len THEN
      IO.Put("StcpIpPacket: too short (" & Fmt.Int(cur.mh_hdr.mh_len) & ", "
				     & Fmt.Int(offset) & ")\n");
      RETURN FALSE
    END;

    WITH
      buf = StcpMbuf.Array(cur)^,
      ipHeadBuf = SUBARRAY(buf, offset, ip_hdr_len),
      ipHeader = VIEW(ipHeadBuf, StcpIpPktFormat.T),
      ipHdrLen = ipHeader.hlen * 4,
      ipHdr = SUBARRAY(buf, offset, ipHdrLen)
     DO
      IF debug THEN PrintIpHeader(ipHeader); END;

      IF StcpNet.checksum(ipHdr, ipHdrLen) # 0 THEN
	IO.Put("!!! Ip checksum is not 0\n");
      END;

      INC(offset, ipHdrLen);

      IF ipHeader.protocol = StcpIpPktFormat.IPPROTO_TCP THEN
	RETURN StcpTcpPacket.PacketGuard(m, cur, offset, ipHeader);
      END;
    END;
    RETURN FALSE
  END PacketGuard;

(* ---------------------------------------------------------------- *)
(* now offset points to the end of EtherPktFormat.T *)
PROCEDURE PacketArrived(m, cur : StcpMbuf.T ; offset : CARDINAL) : StcpMbuf.T =
  BEGIN

    WHILE (cur.mh_hdr.mh_len < offset+ip_hdr_len) AND
	  (cur.mh_hdr.mh_next # NIL) DO
     (*
      IO.Put("StcpIpPacket: Advance mbuf\n");
      *)
      cur := cur.mh_hdr.mh_next;
      offset := 0;
    END;
    IF cur.mh_hdr.mh_len < offset+ip_hdr_len THEN
      IO.Put("StcpIpPacket: too short (" & Fmt.Int(cur.mh_hdr.mh_len) & ", "
				     & Fmt.Int(offset) & ")\n");
      RETURN m;
    END;

    WITH
      buf = StcpMbuf.Array(cur)^,
      ipHeadBuf = SUBARRAY(buf, offset, ip_hdr_len),
      ipHeader = VIEW(ipHeadBuf, StcpIpPktFormat.T),
      ipHdrLen = ipHeader.hlen * 4,
      ipHdr = SUBARRAY(buf, offset, ipHdrLen)
     DO

      (* sanity check *)
      IF ipHdrLen = 0 OR NUMBER(ipHdr) = 0 THEN
	IO.Put("NUMBER(ipHdr) is 0\n");
        PrintIpHeader(ipHeader);
	(* StcpMbuf.m_freem(m); *)
	RETURN NIL;
      END;
	
      IF StcpNet.checksum(ipHdr, ipHdrLen) # 0 THEN
	IO.Put("!!! Ip checksum is not 0\n");
        PrintIpHeader(ipHeader);
	(* StcpMbuf.m_freem(m); *)
	RETURN NIL;
      END;

      IF debug THEN PrintIpHeader(ipHeader); END;

      INC(offset, ipHdrLen);

      CASE ipHeader.protocol OF
	| StcpIpPktFormat.IPPROTO_TCP =>
	  RETURN StcpTcpPacket.PacketArrived(m, cur, offset, ipHeader);
	(* Should not intercept ping packets.  owa.
	| StcpIpPktFormat.IPPROTO_ICMP =>
	  RETURN IcmpPing.PacketArrived(m, cur, offset, ipHeader);
	*)
      ELSE
	RETURN m;
      END;
    END;
  END PacketArrived;

(* ---------------------------------------------------------------- *)
(* You need to fill protocol, tot_len, in passing ip                *)
VAR
  ipId : Ctypes.unsigned_short;

PROCEDURE NextIp():Ctypes.unsigned_short =
  BEGIN
    IF ipId + 1 # LAST(Ctypes.unsigned_short) THEN
      ipId := ipId + 1;
    ELSE
      ipId := 0;
    END;
    RETURN ipId;
  END NextIp;

PROCEDURE PacketSend(m : StcpMbuf.T ; ip : StcpIpPktFormat.T ; so : StcpSocketAddr.T) =
  VAR
    ret : Ctypes.int;
  BEGIN
    (* get a space to place ip header, fill it, then send it *)
    m := StcpMbuf.M_PREPEND(m, ip_hdr_len, StcpMbuf.M_WAIT);

    WITH
      headerBuf = StcpMbuf.Array(m),
      ipHeader = VIEW(headerBuf^, StcpIpPktFormat.T)
     DO
      (* set the ip header length to the header size / 4*)
      ipHeader.hlen := ip_hdr_len DIV 4; (* we are not setting ip options *)

      (* set the version number *)
      ipHeader.vers := 4; (* version compatibility *)
      (* zero out type of service field *)
      ipHeader.tos := ip.tos;
      (* set the id field -- XXX hmm... fix this *)
      ipHeader.id := StcpNet.htons(NextIp());
      (* initialize fragmention information *)
      ipHeader.frag_off := 0; (* not fragmented by default. *)
      (* set time to live *)
      ipHeader.ttl := 32;

      ipHeader.protocol := ip.protocol;

      (* do routing table look up based on destination address *)
      (* XXX: write some writing code. *)

      (* pick device based on routing information *)
      (* XXX: write some device information code. *)

      (* set the src/dst fields for the ip packet *)
      WITH
	s = VIEW(ipHeader.saddr, ARRAY [1..4] OF CHAR),
	d = VIEW(ipHeader.daddr, ARRAY [1..4] OF CHAR)
       DO
	s := DefaultAddr.myIpAddress;
	d := DefaultAddr.targetIpAddress;
      END;
      (* fragmentation not required. *)
      (* set total packet length and checksum *)
      ipHeader.tot_len := StcpNet.htons(ip.tot_len + ip_hdr_len);

      (* compute checksum for the ip header packet *)
      ipHeader.check := 0;
      ipHeader.check := StcpNet.checksum(headerBuf^,ip_hdr_len);
    END;
    ret := StcpEtherPacket.PacketSend(StcpEtherDev.stcpDev, m, so, NIL);
  END PacketSend;

BEGIN
END StcpIpPacket.

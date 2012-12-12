(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created to test EtherPacket.m3.  Test only.
 *)

MODULE IcmpPing;

(*
IMPORT StcpEtherPktFormat, 
 *)
IMPORT StcpIpPktFormat;
IMPORT StcpIpPacket, StcpMbuf, StcpMbufPublic, StcpNet, StcpSocketAddr;
IMPORT IO, Fmt;
IMPORT IcmpPing;

CONST
  echoRequest = 8;
  pingReply = 0;
  echoRequestCode = 0;
  ping_hdr_len = BYTESIZE(IcmpPing.Header);

(* ---------------------------------------------------------------- *)
(* ought to handle query from others... *)
PROCEDURE PacketArrived(m, cur : StcpMbuf.T ; offset : CARDINAL ; ipHeader : StcpIpPktFormat.T) : StcpMbuf.T =
  BEGIN
    WITH
      buf = StcpMbuf.Array(cur)^,
      pingBuf = VIEW(SUBARRAY(buf, offset, ping_hdr_len), IcmpPing.Header)
     DO
      IF debug THEN 
        IO.Put("ICMP: --- ping request/reply\n");
        IO.Put("type " & Fmt.Int(pingBuf.type) & " ");
        IO.Put("code " & Fmt.Int(pingBuf.code) & " ");
        IO.Put("csum " & Fmt.Int(pingBuf.csum, 16) & " ");
        IO.Put("id " & Fmt.Int(pingBuf.id) & " ");
        IO.Put("seq " & Fmt.Int(pingBuf.seq)  & "\n");
      END;

      IF (pingBuf.type = echoRequest) AND (pingBuf.code = echoRequestCode) THEN
	(* yes, this is ping request. let's reply *)
	IF debug THEN IO.Put("Replying to a ping request\n"); END;
        Reply(m, cur, offset, ipHeader);
        RETURN NIL;
      END;
    END;
    RETURN m;
  END PacketArrived;

PROCEDURE Reply(m, cur : StcpMbuf.T ; offset : CARDINAL ; ipHeader : StcpIpPktFormat.T) =
  VAR
    replyPacket : StcpMbuf.T;
    tmpIp : StcpIpPktFormat.T;
    so : StcpSocketAddr.T;
  BEGIN
    WITH
      len = StcpNet.nstoh(ipHeader.tot_len) - ipHeader.hlen * 4,
      buf = StcpMbuf.Array(cur)^
     DO
      replyPacket := StcpMbuf.m_get(StcpMbuf.M_WAIT, StcpMbuf.MT_DATA);

      IF replyPacket = NIL THEN
	(* XXX what should I do? *)
	IO.Put("can't allocate mbuf\n");
	StcpMbuf.m_freem(m);
	RETURN;
      END;

      replyPacket.mh_hdr.mh_len := len;
      StcpMbuf.M_ALIGN(replyPacket, replyPacket.mh_hdr.mh_len);

      WITH
	pingOutBuf = StcpMbuf.Array(replyPacket)^,
	pingOutHdr = VIEW(SUBARRAY(pingOutBuf, 0, ping_hdr_len), IcmpPing.Header)
       DO
	FOR i:=0 TO len-1 DO
	  pingOutBuf[i] := buf[offset+i];
	END;
	pingOutHdr.type := pingReply;
	pingOutHdr.csum := 0;
	pingOutHdr.csum := StcpNet.checksum(pingOutBuf, 0);
      END;

      tmpIp.tot_len := len;
      tmpIp.protocol := StcpIpPktFormat.IPPROTO_ICMP;
      tmpIp.tos := 0;
      so.sa_family := 2;
      StcpIpPacket.PacketSend(replyPacket, tmpIp, so);

    END;
    StcpMbuf.m_freem(m);
  END Reply;

(* we may assume m is the first mbuf and contains all info *)
(* since Ping's reply request is small enough to be in a mbuf *)
(* OK, this works fine. let's try an another version ... *)
(*
PROCEDURE Reply(m : StcpMbuf.T) =
  VAR
    tmpEth : StcpEtherPktFormat.T;
    tmpIp : StcpIpPktFormat.T;
    so : StcpSocketAddr.T;
    ret : Ctypes.int;
  BEGIN
    WITH
      wholeBuf = StcpMbuf.Array(m)^,
      ethBuf = SUBARRAY(wholeBuf, 0, eth_hdr_len),
      eth_out = VIEW(ethBuf, StcpEtherPktFormat.T),
      ipBuf = SUBARRAY(wholeBuf, eth_hdr_len, ip_hdr_len),
      ip_out = VIEW(ipBuf, StcpIpPktFormat.T),
      pingBuf = SUBARRAY(wholeBuf, eth_hdr_len+ip_hdr_len,
	NUMBER(wholeBuf) - (eth_hdr_len+ip_hdr_len)),
      ping_out = VIEW(SUBARRAY(pingBuf, 0, ping_hdr_len), IcmpPing.T)
     DO
      ping_out.type := pingReply;
      ping_out.csum := 0;
      ping_out.csum := StcpNet.checksum(pingBuf, 0);

      (* swap dest/src Ip address *)
      tmpIp.daddr := ip_out.daddr;
      ip_out.daddr := ip_out.saddr;
      ip_out.saddr := tmpIp.daddr;

      (* do we have to recalculate ip check sum? *)
	(* don't think so... *)

      (* swap dest/src ether address *)
      tmpEth.dhost := eth_out.dhost;
      eth_out.dhost := eth_out.shost;
      eth_out.shost := tmpEth.dhost;
      (* somehow, this is necessary *)
      eth_out.type := StcpNet.htons(StcpEtherPktFormat.ETHERTYPE_IP);
    END;

    so.sa_family := 0;			(* send it by Raw() *)
    ret := EtherPacket.PacketSend(EtherPacket.myIfnet^, m, so, NIL);
  END Reply;

*)

PROCEDURE SendRequest() = 
  VAR
    ip : StcpIpPktFormat.T;
    data  : StcpMbuf.T;
    so : StcpSocketAddr.T;
  BEGIN

    (* create a new mbuf to send the arp packet in *)
    data := StcpMbuf.m_gethdr(StcpMbuf.M_WAIT,StcpMbuf.MT_DATA);
    WITH mh_len = data.mh_hdr.mh_len DO
      mh_len := ping_hdr_len;
      StcpMbuf.M_ALIGN(data,mh_len);
    END;
    StcpMbufPublic.SetPktHdrLen(data, data.mh_hdr.mh_len);

    WITH data_out = StcpMbuf.Array(data)^,
	 pingBuf = SUBARRAY(data_out, 0, ping_hdr_len),
         ping_out = VIEW(pingBuf, IcmpPing.Header)
     DO
      (* setup the ping header *)
      ping_out.type := echoRequest;
      ping_out.code := echoRequestCode;
      ping_out.id := 212;		(* usually pid *)
      ping_out.seq := 0;		(* starts at 0 *)
      ping_out.csum := 0;
      ping_out.csum := StcpNet.checksum(pingBuf, ping_hdr_len);
    END;

    (* setup some ip header *)
    ip.tot_len := ping_hdr_len;		(* exclude ip header len *)
    ip.protocol := StcpIpPktFormat.IPPROTO_ICMP;

    so.sa_family := 2;
    StcpIpPacket.PacketSend(data, ip, so);

  END SendRequest;

BEGIN
END IcmpPing.

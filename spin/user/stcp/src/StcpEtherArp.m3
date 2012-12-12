(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 24-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	use m_gethdr rather than m_get to set M_PKTHDR for loom boxes.
 *
 * 21-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *      fixed a bug with the vx (3c905) driver for loom boxes.
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/net/etherClient/src and simplified.
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to work with FreeBSD SAL.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 01-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed the Arp*Guard procedures to not get a range fault when the
 *	packets are short.
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up GetAddr and GetEtherAddr.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 12-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support for respond to Arp requests.
 *
 * 06-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Switched to new spin shell commands.
 *
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Turned on timeouts on arp entries.
 *
 * 12-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Replaced Clib with IO for output.
 *	Cleaned up SendRequest() and ArpReplyHandler().
 *	Fixed alignment problem and eliminated need for bcopy().
 *	Fixed ArpIp record to use StcpIpPktFormat.Address.
 *
 * 10-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Replaced LOOPHOLEs with VIEW.
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
 *
 * 05-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	Updating to arp implementation with cache, timeouts, etc.
 *
 * 05-Mar-95  Marc Fiuczynski (mef) at the University of Washington
 *	Makeshift ARP table implementation.
 *)

MODULE StcpEtherArp;

IMPORT StcpEtherPktFormat, StcpArpPktFormat, StcpIpPktFormat;
IMPORT StcpEtherPacket, StcpMbuf, StcpNet, StcpSocketAddr;
IMPORT Ctypes, IO, Fmt, NetText; 
IMPORT DefaultAddr; 
IMPORT Clock, Mutex, Thread; <* NOWARN *>
IMPORT Sema;
IMPORT StcpMbufPublic;
IMPORT StcpEtherDev;

TYPE ArpIp = RECORD (* must be packed *)
  header              : StcpArpPktFormat.Header;
  srcHardwareAddress  : StcpEtherPktFormat.Address;
  srcIpAddress        : StcpIpPktFormat.AddressArray;
  dstHardwareAddress  : StcpEtherPktFormat.Address;
  dstIpAddress        : StcpIpPktFormat.AddressArray;
END;

VAR
  arprequesttype, arpreplytype : CARDINAL;
  sendAgain : Sema.T;
  gotReply : BOOLEAN;
  isAsking : BOOLEAN;

CONST
  arp_len = BYTESIZE(ArpIp);
  eth_hdr_len = BYTESIZE(StcpEtherPktFormat.Header);

(* ---------------------------------------------------------------- *)
(* XXX ought to handle query from others... *)
PROCEDURE PacketArrived(m, cur : StcpMbuf.T ; offset : CARDINAL) : StcpMbuf.T =
  VAR
    ip: StcpIpPktFormat.Address;
  BEGIN
    (* sanity check *)
    WHILE (cur.mh_hdr.mh_len < offset+arp_len) AND
          (cur.mh_hdr.mh_next # NIL) DO
      cur := cur.mh_hdr.mh_next;
      offset := 0;
    END;
    IF cur.mh_hdr.mh_len < offset+arp_len THEN
      IO.Put("IpPacket: too short (" & Fmt.Int(cur.mh_hdr.mh_len) & ", "
                                     & Fmt.Int(offset) & ")\n");
      RETURN m;
    END;

    WITH
      buf = StcpMbuf.Array(cur)^,
      arpBuf = VIEW(SUBARRAY(buf, offset, arp_len), ArpIp)
     DO
      IF debug THEN
	IO.Put("Arp : " &NetText.FmtEther(arpBuf.srcHardwareAddress));
	VIEW(ip, StcpIpPktFormat.AddressArray) := arpBuf.srcIpAddress;
        IO.Put("  " &NetText.FmtIp(ip));
        IO.Put(" --> " &NetText.FmtEther(arpBuf.dstHardwareAddress));
	VIEW(ip, StcpIpPktFormat.AddressArray) := arpBuf.dstIpAddress;
        IO.Put("  " &NetText.FmtIp(ip));
        IO.Put("  hdr " & Fmt.Int(arpBuf.header.hrd) & " ");
        IO.Put("pro " & Fmt.Int(arpBuf.header.pro) & " ");
        IO.Put("hln " & Fmt.Int(arpBuf.header.hln) & " ");
        IO.Put("pln " & Fmt.Int(arpBuf.header.pln) & " ");
        IO.Put("op " & Fmt.Int(arpBuf.header.op )  & "\n");
      END;
      IF isAsking AND arpBuf.header.op = arpreplytype THEN
	IF DefaultAddr.targetIpAddress = arpBuf.srcIpAddress THEN
	  DefaultAddr.targetHardwareAddress := arpBuf.srcHardwareAddress;
	  VIEW(ip, StcpIpPktFormat.AddressArray) := arpBuf.srcIpAddress;
	  IO.Put("Arp: " &NetText.FmtIp(ip));
	  IO.Put(" says his ether address is " &NetText.FmtEther(arpBuf.srcHardwareAddress) &"\n");
	  gotReply := TRUE;
	END;
      END;
    END;
    IF isAsking THEN Sema.V(sendAgain); END;
    RETURN m;
  END PacketArrived;

(* ---------------------------------------------------------------- *)
PROCEDURE SendRequest() = 
  BEGIN

    isAsking := TRUE;
    gotReply := FALSE;

    (* XXX nasty... send actual request and wait for a while *)
    FOR i:= 0 TO 5 DO		(* 5 is enough ? *)
      IF debug THEN
        IO.Put("sending " & Fmt.Int(i) & "th request\n");
      END;
      IF NOT gotReply THEN SendRequestLow(); END;
      Sema.P(sendAgain);
      IF gotReply THEN
	isAsking := FALSE;
	RETURN;
      END;
    END;

    (* should not reach here *)
    IO.Put("Arp: could not get an reply...\n");
    IO.Put("     using default address\n");
      
  END SendRequest;

(* ---------------------------------------------------------------- *)
PROCEDURE SendRequestLow() = 
  VAR
    data  : StcpMbuf.T;
    so : StcpSocketAddr.T;
    ret : Ctypes.int;
  BEGIN

    (* create a new mbuf to send the arp packet in *)
    data := StcpMbuf.m_gethdr(StcpMbuf.M_WAIT,StcpMbuf.MT_DATA);
    WITH mh_len = data.mh_hdr.mh_len DO
      mh_len := arp_len + eth_hdr_len;
      StcpMbuf.MH_ALIGN(data,mh_len);
    END;
    StcpMbufPublic.SetPktHdrLen(data, data.mh_hdr.mh_len);

    WITH data_out = StcpMbuf.Array(data)^,
	 eth_out = VIEW(SUBARRAY(data_out, 0, eth_hdr_len), StcpEtherPktFormat.T),
         arp_out = VIEW(SUBARRAY(data_out,eth_hdr_len, arp_len), ArpIp)
      DO
       (* setup the ether header *)
       eth_out.dhost := StcpEtherPktFormat.broadcast;
       eth_out.shost := DefaultAddr.myHardwareAddress;
       eth_out.type := StcpNet.htons(StcpEtherPktFormat.ETHERTYPE_ARP);
         
       (* setup the arp header *)
       arp_out.header.hrd := StcpNet.htons(1); (* XXX ether hardware type *)
       arp_out.header.pro := StcpNet.htons(StcpEtherPktFormat.ETHERTYPE_IP);
       arp_out.header.hln := BYTESIZE(StcpEtherPktFormat.Address);
       arp_out.header.pln := BYTESIZE(StcpIpPktFormat.Address);
       arp_out.header.op  := arprequesttype;

       (* setup the arp packet *)
       arp_out.dstHardwareAddress := StcpEtherPktFormat.zero;
       arp_out.srcHardwareAddress := DefaultAddr.myHardwareAddress;

       (* assumed to be in network byte order *)
       arp_out.dstIpAddress := DefaultAddr.targetIpAddress;
       arp_out.srcIpAddress := DefaultAddr.myIpAddress;
      END;
    so.sa_family := 0;		(* SocketRep.AF_INET, send it by Raw() *)

    ret := StcpEtherPacket.PacketSend(StcpEtherDev.stcpDev, data, so, NIL);

  END SendRequestLow;

(* ---------------------------------------------------------------- *)
PROCEDURE Init() =
  BEGIN
    arprequesttype := StcpNet.htons(StcpArpPktFormat.REQUEST);
    arpreplytype := StcpNet.htons(StcpArpPktFormat.REPLY);
    sendAgain := Sema.Alloc(0);    
    isAsking := FALSE;
    gotReply := FALSE;
  END Init;

BEGIN
END StcpEtherArp.

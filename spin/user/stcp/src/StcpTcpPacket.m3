(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 20-Jun-97  Tsutomu Owa (owa) at the University of Washington
 *	Removed StcpMbuf.m_freem() as packets are all freed
 *	at StcpEtherPacket.AsyncDeliver() now.
 *
 * 02-Jun-97  Tsutomu Owa (owa) at the University of Washington
 *	It checks tcp check sum.
 *	Use Clock.ClockInterruptRate() to get number of ticks per sec.
 *	Fixed a bug in Dispatch where it should check seq # of incoming packet
 *	equal to the one already gotten.  Don't know why this has not been
 *	discovered for a long time.
 *
 * 24-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	use m_gethdr rather than m_get to set M_PKTHDR for loom boxes.
 *
 * 21-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	fixed a bug with the vx (3c905) driver for loom boxes.
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.  Fixed a bug to set dst port.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Cleanup to make compiler quiet. 
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

MODULE StcpTcpPacket;

IMPORT StcpTcpPktFormat;
IMPORT StcpMbufPublic, StcpMbuf;
IMPORT StcpNet;
IMPORT StcpIpPacket;
IMPORT StcpIpPktFormat;
IMPORT StcpUtils;
IMPORT Fmt, IO, Text;
IMPORT Sema, Clock;
IMPORT StcpSocketAddr;
IMPORT DefaultAddr, Ctypes;

(* ------------------------------------------------------- *)
(* those are temporary...  It's quite different from the socket interface *)
CONST
  tcp_hdr_len = BYTESIZE(StcpTcpPktFormat.Header);
  BufSize = 64 * 1024;	(* 65636 bytes *)

VAR
  isOpenWaiting : BOOLEAN := FALSE;
  isErrorHappen : BOOLEAN;
  myTcpStatus : tcpStatus;
  oldMyTcpStatus : tcpStatus;
  basePort : CARDINAL := 31000;
  numOfPort : CARDINAL := 500;
  contents : REF ARRAY OF CHAR;
  contentsSize : CARDINAL;
  debug := FALSE;
  debugDispatch := FALSE;


(* ------------------------------------------------------- *)
PROCEDURE GetTcpHdrLen (tcpHeader : StcpTcpPktFormat.T) : CARDINAL =
  BEGIN
    (* sanity check *)
    IF tcpHeader.xoff < 5 THEN
      IO.Put("  Tcp Hdr len (" & Fmt.Int(tcpHeader.xoff) & ") < 20H!\n");
      RETURN tcp_hdr_len;
    END;
    RETURN tcpHeader.xoff * 4 ;
  END GetTcpHdrLen;

(* ------------------------------------------------------- *)
PROCEDURE PrintTcpHeader(tcpHeader : StcpTcpPktFormat.T;
			 VAR buf : ARRAY OF CHAR) =
  PROCEDURE GetTcpFlags(flags : StcpTcpPktFormat.Flags) : TEXT =
    VAR
      val : TEXT := " ";
    BEGIN
      IF StcpTcpPktFormat.Flag.fin IN flags THEN val := val & "FIN|"; END;
      IF StcpTcpPktFormat.Flag.syn IN flags THEN val := val & "SYN|"; END;
      IF StcpTcpPktFormat.Flag.rst IN flags THEN val := val & "RST|"; END;
      IF StcpTcpPktFormat.Flag.push IN flags THEN val := val & "PUSH|"; END;
      IF StcpTcpPktFormat.Flag.ack IN flags THEN val := val & "ACK|"; END;
      IF StcpTcpPktFormat.Flag.urg IN flags THEN val := val & "URG|"; END;
      RETURN val;
    END GetTcpFlags;

  VAR
    offset : CARDINAL;
  BEGIN
  IO.Put("Tcp header :  ");
  IO.Put("port " & Fmt.Int(StcpNet.nstoh(tcpHeader.sport)) & " --> " & 
		   Fmt.Int(StcpNet.nstoh(tcpHeader.dport)) & "\n");
  IO.Put("  seq (" & Fmt.Int(StcpNet.nltoh(tcpHeader.seq), 16) & ") ack seq (" &
		     Fmt.Int(StcpNet.nltoh(tcpHeader.ack_seq), 16) & ")");
  IO.Put("  flags: " & GetTcpFlags(tcpHeader.flags)); 
  IO.Put("  hlen: " & Fmt.Int(tcpHeader.xoff)); 
  IO.Put("  check " & Fmt.Int(StcpNet.nstoh(tcpHeader.check), 16));
  IO.Put("  window (" & Fmt.Int(StcpNet.nstoh(tcpHeader.window)) & ")\n");
  (* size of option header = (tcpHeader.xoff - 5) * 4 *)
  IO.Put("  extra options <");
  FOR i:= 0 TO NUMBER(buf)-1 DO
      offset := ORD(buf[i]);
      IO.Put(Fmt.Int(offset, 16) & " ");
  END;
  IO.Put(">\n");

  END PrintTcpHeader;

PROCEDURE PacketGuard(<* UNUSED *> m : StcpMbuf.T;
		      cur : StcpMbuf.T ;
		      offset : CARDINAL ;
		      ipHeader : StcpIpPktFormat.T) : BOOLEAN =
  VAR
    len : CARDINAL;
    (* XXX. need to change 128 w/ CONSTANT or whatever.. *)
    (*      assume size of tcp option is less than 128. *)
    tcpExtraBuf: ARRAY [0..128] OF CHAR;
  BEGIN
    len := StcpNet.nstoh(ipHeader.tot_len) - ipHeader.hlen * 4;

    WHILE (cur.mh_hdr.mh_len < tcp_hdr_len + offset) AND
	  (cur.mh_hdr.mh_next # NIL) DO
     (*
      IO.Put("StcpTcpPacket: Advance mbuf\n");
      *)
      cur := cur.mh_hdr.mh_next;
      offset := 0;
    END;
    IF cur.mh_hdr.mh_len < offset + tcp_hdr_len THEN
      IO.Put("StcpTcpPacket: too short (" & Fmt.Int(cur.mh_hdr.mh_len) & ", "
				      & Fmt.Int(offset) & ")\n");
      RETURN FALSE
    END;

    WITH
      buf = StcpMbuf.Array(cur)^,
      tcpHeadBuf = SUBARRAY(buf, offset, tcp_hdr_len),
      tcpHeader = VIEW(tcpHeadBuf, StcpTcpPktFormat.T),
      hdrLen = GetTcpHdrLen(tcpHeader),
      rest = cur.mh_hdr.mh_len-offset-tcp_hdr_len
     DO
      (* XXX. the server send a packet which consist of two mbuf.
	      and the tcp option exists both mbufs!   *)
      IF offset+hdrLen > cur.mh_hdr.mh_len THEN
	(* why does this happen... *)
	SUBARRAY(tcpExtraBuf, 0, rest)
		:= SUBARRAY(buf, offset+tcp_hdr_len, rest);
	IF (cur.mh_hdr.mh_next # NIL) THEN
	  cur := cur.mh_hdr.mh_next;
	  offset := 0;
	  IF cur.mh_hdr.mh_len < hdrLen-tcp_hdr_len-rest THEN
	    (* XXX. ought to handle more cleaner. *)
	    IO.Put("StcpTcpPacket: too short for tcp option\n");
	    RETURN FALSE
	  END;
	  WITH buf2 = StcpMbuf.Array(cur)^ DO
	    SUBARRAY(tcpExtraBuf, rest, hdrLen-rest-tcp_hdr_len)
		:= SUBARRAY(buf2, 0, hdrLen-rest-tcp_hdr_len);
	    INC(offset, hdrLen-rest);
	  END;
	ELSE
	  IO.Put("StcpTcpPacket: too short for tcp option\n");
	  RETURN FALSE
	END;
      ELSE
	SUBARRAY(tcpExtraBuf, 0, hdrLen-tcp_hdr_len)
		:= SUBARRAY(buf, offset+tcp_hdr_len, hdrLen-tcp_hdr_len);
	INC(offset, hdrLen);
      END;

      IF debug THEN
        PrintTcpHeader(tcpHeader, SUBARRAY(tcpExtraBuf, 0, hdrLen-tcp_hdr_len));
        StcpUtils.PrintStcpMbuf(cur);
	IO.Put("expected dport " & Fmt.Int(myTcpStatus.sport) &
		", got " & Fmt.Int(StcpNet.nstoh(tcpHeader.dport)) & "\n");
	IO.Put("expected sport " & Fmt.Int(myTcpStatus.dport) &
		", got " & Fmt.Int(StcpNet.nstoh(tcpHeader.sport)) & "\n");
      END;
      
      (* compute lenght again here *)
      len := len - hdrLen;

      IF (StcpNet.nstoh(tcpHeader.dport) = myTcpStatus.sport) AND
	 (StcpNet.nstoh(tcpHeader.sport) = myTcpStatus.dport) THEN
	RETURN TRUE
      END;
    END;

    RETURN FALSE
  END PacketGuard;

(* ------------------------------------------------------- *)
(* assume offset points to the begining of StcpTcpPktFormat *)
PROCEDURE PacketArrived(m, cur : StcpMbuf.T ; offset : CARDINAL ; ipHeader : StcpIpPktFormat.T) : StcpMbuf.T =
  VAR
    len : CARDINAL;
    (* XXX. need to change 128 w/ CONSTANT or whatever.. *)
    (*      assume size of tcp option is less than 128. *)
    tcpExtraBuf: ARRAY [0..128] OF CHAR;
  BEGIN
    len := StcpNet.nstoh(ipHeader.tot_len) - ipHeader.hlen * 4;

    WHILE (cur.mh_hdr.mh_len < tcp_hdr_len + offset) AND
	  (cur.mh_hdr.mh_next # NIL) DO
     (*
      IO.Put("StcpTcpPacket: Advance mbuf\n");
      *)
      cur := cur.mh_hdr.mh_next;
      offset := 0;
    END;
    IF cur.mh_hdr.mh_len < offset + tcp_hdr_len THEN
      IO.Put("StcpTcpPacket: too short (" & Fmt.Int(cur.mh_hdr.mh_len) & ", "
				      & Fmt.Int(offset) & ")\n");
      RETURN m;
    END;

    WITH
      buf = StcpMbuf.Array(cur)^,
      tcpHeadBuf = SUBARRAY(buf, offset, tcp_hdr_len),
      tcpHeader = VIEW(tcpHeadBuf, StcpTcpPktFormat.T),
      hdrLen = GetTcpHdrLen(tcpHeader),
      rest = cur.mh_hdr.mh_len-offset-tcp_hdr_len
     DO

      (* see if this packet is mine.  if not, return it *)
      IF (StcpNet.nstoh(tcpHeader.dport) # myTcpStatus.sport) OR
	 (StcpNet.nstoh(tcpHeader.sport) # myTcpStatus.dport) THEN
	RETURN m;
      END;

      IF CheckTcpChecksum(cur, offset, ipHeader, len) THEN
	(* IO.Put("Got wrong checksum. discarding it\n"); *)
	(* StcpMbuf.m_freem(m); *)
	RETURN NIL;
      END;

      (* XXX. the server send a packet which consist of two mbuf.
	      and the tcp option exists both mbufs!   *)
      IF offset+hdrLen > cur.mh_hdr.mh_len THEN
	(* why does this happen... *)
	SUBARRAY(tcpExtraBuf, 0, rest)
		:= SUBARRAY(buf, offset+tcp_hdr_len, rest);

	IF (cur.mh_hdr.mh_next # NIL) THEN
	  cur := cur.mh_hdr.mh_next;
	  offset := 0;
	  IF cur.mh_hdr.mh_len < hdrLen-tcp_hdr_len-rest THEN
	    (* XXX. ought to handle more cleaner. *)
	    IO.Put("StcpTcpPacket: too short for tcp option\n");
	    RETURN m;
	  END;
	  WITH buf2 = StcpMbuf.Array(cur)^ DO
	    SUBARRAY(tcpExtraBuf, rest, hdrLen-rest-tcp_hdr_len)
		:= SUBARRAY(buf2, 0, hdrLen-rest-tcp_hdr_len);
	    INC(offset, hdrLen-rest);
	  END;
	ELSE
	  IO.Put("StcpTcpPacket: too short for tcp option\n");
	  RETURN m;
	END;
      ELSE
	SUBARRAY(tcpExtraBuf, 0, hdrLen-tcp_hdr_len)
		:= SUBARRAY(buf, offset+tcp_hdr_len, hdrLen-tcp_hdr_len);
	INC(offset, hdrLen);
      END;

      IF debug THEN
        PrintTcpHeader(tcpHeader, SUBARRAY(tcpExtraBuf, 0, hdrLen-tcp_hdr_len));
        StcpUtils.PrintStcpMbuf(cur);
	IO.Put("expected dport " & Fmt.Int(myTcpStatus.sport) &
		", got " & Fmt.Int(StcpNet.nstoh(tcpHeader.dport)) & "\n");
	IO.Put("expected sport " & Fmt.Int(myTcpStatus.dport) &
		", got " & Fmt.Int(StcpNet.nstoh(tcpHeader.sport)) & "\n");
      END;
      
      (* compute lenght again here *)
      len := len - hdrLen;

      Sema.P(myTcpStatus.sema);
      Dispatch(m, cur, offset, tcpHeader, len);
      Sema.V(myTcpStatus.sema);
      (* StcpMbuf.m_freem(m); *)
      RETURN NIL
    END;
  END PacketArrived; 

(* ------------------------------------------------------- *)
PROCEDURE CheckTcpChecksum(cur: StcpMbuf.T;
	offset: CARDINAL;
	ipHeader: StcpIpPktFormat.T;
	len: CARDINAL) : BOOLEAN = 
  VAR
    ipovl_buf : ARRAY [1 .. BYTESIZE(StcpIpPktFormat.OverlayHeader)] OF CHAR;
    ipovl_csum: Ctypes.unsigned_short;
    tcp_csum: Ctypes.unsigned_short;
  BEGIN
    WITH ipovl = VIEW(ipovl_buf,StcpIpPktFormat.OverlayHeader) DO
      ipovl.fill[0]:= 0;
      ipovl.fill[1]:= 0;
      ipovl.ih_x1  := 0;
      ipovl.ih_pr  := StcpIpPktFormat.IPPROTO_TCP;
      ipovl.ih_len := StcpNet.htons(len);
      ipovl.ih_src := ipHeader.saddr; 
      ipovl.ih_dst := ipHeader.daddr; 
    END;
    ipovl_csum := StcpNet.checksum(ipovl_buf);
    (* hack!  Should not change mbuf... *)
    INC(cur.mh_hdr.mh_data, offset);
    DEC(cur.mh_hdr.mh_len, offset); 
    tcp_csum := StcpMbuf.Checksum(cur,ipovl_csum, len);
    DEC(cur.mh_hdr.mh_data, offset);
    INC(cur.mh_hdr.mh_len, offset); 
    IF tcp_csum # 0 THEN
      IO.Put("tcpchecksum got " & Fmt.Int(tcp_csum) & ". discarding it\n");
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;

  END CheckTcpChecksum;

(* ------------------------------------------------------- *)
PROCEDURE UpdateSeq(cur : StcpTcpPktFormat.Seq; len : CARDINAL) : StcpTcpPktFormat.Seq =
  BEGIN
    IF cur + len <= LAST(StcpTcpPktFormat.Seq) THEN
      RETURN(cur + len);
    ELSE
      RETURN(cur+len-LAST(StcpTcpPktFormat.Seq)-1);
    END;
  END UpdateSeq;

(* ------------------------------------------------------- *)
PROCEDURE PrintMyStatus() =
  BEGIN
    PrintDispatchMessage("Dispatch entered -- ");
    PrintDispatchMessage("State ("& Fmt.Int(myTcpStatus.status) & ") ");
    PrintDispatchMessage("Seq ("& Fmt.Int(myTcpStatus.seq,16) & ") ");
    PrintDispatchMessage("AckSeq ("& Fmt.Int(myTcpStatus.ack_seq,16) & ")\n");
  END PrintMyStatus;

PROCEDURE PrintDispatchMessage(msg : TEXT) =
  BEGIN
    IF debugDispatch THEN IO.Put(msg); END;
  END PrintDispatchMessage;

PROCEDURE CheckSeqNumber(flags : StcpTcpPktFormat.Flags;
				 recved, sent : StcpTcpPktFormat.Seq) : BOOLEAN =
  VAR
    tcpHeader : StcpTcpPktFormat.T; 
  BEGIN
    (* XXX need to check rounding... *)
    IF (recved = sent) THEN
      RETURN FALSE;
    ELSE
      IF StcpTcpPktFormat.Flag.rst IN flags THEN
       (*
	IO.Put("RST: port (" &Fmt.Int(StcpNet.nstoh(tcpHeader.dport))& ")\n");
	IO.Put("R");
	*)
      END;
      (* XXX need to handle re-transmited packets.  Do we need ack? *)

      (* XXX ugly... *)
      IF isOpenWaiting THEN
        tcpHeader.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.rst}; 
        PacketSend(tcpHeader, 0, NIL);
        isOpenWaiting := FALSE;
        Sema.Broadcast(myTcpStatus.semaEstablished);
      END;
      RETURN TRUE;
    END;
  END CheckSeqNumber;

(* ------------------------------------------------------- *)
PROCEDURE Dispatch(
	<* UNUSED *> m : StcpMbuf.T;
	cur : StcpMbuf.T;
	offset : CARDINAL;
	tcpHeader : StcpTcpPktFormat.T;
	len : CARDINAL) =
  VAR
    flags : StcpTcpPktFormat.Flags;
    testFlags : StcpTcpPktFormat.Flags;
    tcp : StcpTcpPktFormat.T;
    recvedSeq : StcpTcpPktFormat.Seq;
    recvedAckSeq : StcpTcpPktFormat.Seq;
  BEGIN

    recvedSeq := StcpNet.nltoh(tcpHeader.seq);
    recvedAckSeq := StcpNet.nltoh(tcpHeader.ack_seq);
    flags := tcpHeader.flags;
    IF debugDispatch THEN
      PrintMyStatus();
      IO.Put("tcp.seq is "); IO.Putx(recvedSeq);
      IO.Put(" tcp.ack_seq is "); IO.Putx(recvedAckSeq); IO.Put("\n");
    END;

    IF CheckSeqNumber(flags, recvedAckSeq, myTcpStatus.seq) THEN
      RETURN;
    END;

    CASE myTcpStatus.status OF
      (* ------------------------------------------------- *)
      | CLOSED =>
	PrintDispatchMessage("--- Enter CLOSED\n");
	IF StcpTcpPktFormat.Flag.syn IN flags THEN
          (* XXX: you should take care of Passive Open & Reset?  *)
	  (* passive open *)
	END;
      (* ------------------------------------------------- *)
      | SYN_SENT => 
	PrintDispatchMessage("--- Enter SYN_SENT\n");
	testFlags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.syn,
					StcpTcpPktFormat.Flag.ack};
	IF testFlags = flags THEN
	  (* send stage of active open *)
	  IF recvedAckSeq = myTcpStatus.seq THEN
	    (* this is the first time to set ack_seq.  just do it *)
	    myTcpStatus.ack_seq := UpdateSeq(recvedSeq, 1);
	    tcp.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack};
	    PacketSend(tcp, 0, NIL);
	    ChangeState(ESTABLISHED);
	  END;
	  (* XXXX fine? *)
          IF debug THEN IO.Put("Unblocking ConnectLow\n"); END;
	  Sema.V(myTcpStatus.semaEstablished);
	ELSIF StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.fin} = flags THEN
	  IF recvedAckSeq = myTcpStatus.seq THEN
	    myTcpStatus.ack_seq := UpdateSeq(recvedSeq, 1);
	    tcp.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.fin};
	    ChangeState(CLOSE_WAIT);
	    PacketSend(tcp, 0, NIL);
	  END;
	  Sema.V(myTcpStatus.semaEstablished);
	ELSE 
	  (* do I ought to send RST in this case? *)
	  (* kick sender anyway.  somehow server sends only ack sometimes *)
	  Sema.V(myTcpStatus.semaEstablished);
	END;
      (* ------------------------------------------------- *)
      | ESTABLISHED =>
	PrintDispatchMessage("--- Enter ESTABLISHED\n");
	testFlags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack};

        (* If incomming data > recved seq number. then discard it *)
	(* XXX need to check rounding... *)
	IF recvedSeq > myTcpStatus.ack_seq THEN
	  PrintDispatchMessage("-- not ready to receive it\n");
	  PrintDispatchMessage(" (" & Fmt.Int(recvedSeq, 16) & "), (" &
			Fmt.Int(myTcpStatus.ack_seq, 16) & ")\n");
	ELSIF StcpTcpPktFormat.Flag.syn IN flags THEN
	  PrintDispatchMessage("-- already established. ignore syn\n");
	ELSIF recvedSeq < myTcpStatus.ack_seq THEN
	  PrintDispatchMessage("-- already got the packet. discarding it\n");
	  PrintDispatchMessage("recvedSeq (" & Fmt.Int(recvedSeq) &
		 ") my ack_seq (" & Fmt.Int(myTcpStatus.ack_seq) & ")\n");
	ELSE
	  (* should be recvedSeq = myTcpStatus *)
	  IF (len = 0) AND (testFlags = flags) THEN
	    PrintDispatchMessage(" - just a ACK. ignore\n");
	  ELSE
	    tcp.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack};
	    myTcpStatus.ack_seq := UpdateSeq(myTcpStatus.ack_seq, len);
	    ProcessData(cur, offset, len);
	    IF StcpTcpPktFormat.Flag.fin IN flags THEN
	      myTcpStatus.ack_seq := UpdateSeq(myTcpStatus.ack_seq, 1);
	    END;
	    PacketSend(tcp, 0, NIL);
	    IF StcpTcpPktFormat.Flag.fin IN flags THEN
	      ChangeState(CLOSE_WAIT);
	      (* XXX fix this! *)
	      Sema.V(myTcpStatus.semaCloseWait);
	    END;
	  END;
	END;
      (* ------------------------------------------------- *)
      | FIN_WAIT1 =>
	PrintDispatchMessage("--- Enter FIN_WAIT1\n");
	testFlags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack,
					StcpTcpPktFormat.Flag.fin};
	(* waiting ACK from server *)
	IF testFlags = flags THEN
	  (* ok, send ACK and goto TIME_WAIT *)
	  tcp.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack};
	  myTcpStatus.ack_seq := UpdateSeq(myTcpStatus.ack_seq, 1);
	  ChangeState(TIME_WAIT);
	  PacketSend(tcp, 0, NIL);
	ELSIF StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack} = flags THEN
	  (* wait until we get FIN.  goto FIN_WAIT2 *)
	  ChangeState(FIN_WAIT2);
	ELSIF StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.fin} = flags THEN
	  (* simultaneous close. wait until we get ACK. goto CLOSING *)
	  myTcpStatus.ack_seq := UpdateSeq(myTcpStatus.ack_seq, 1);
	  ChangeState(CLOSING);
	END;

      (* ------------------------------------------------- *)
      | FIN_WAIT2 =>
	PrintDispatchMessage("--- Enter FIN_WAIT2\n");
	IF StcpTcpPktFormat.Flag.fin IN flags THEN
	  (* well, I should go TIME_WAIT and then CLOSE. but... *)
	    myTcpStatus.ack_seq := UpdateSeq(myTcpStatus.ack_seq, 1);
	    tcp.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack};
	    PacketSend(tcp, 0, NIL);
	  ChangeState(CLOSED);
	END;
      (* ------------------------------------------------- *)
      | CLOSE_WAIT =>
        PrintDispatchMessage("--- Enter CLOSE_WAIT.  Waiting res from appl.\n");
      (* ------------------------------------------------- *)
      | LAST_ACK =>
        PrintDispatchMessage("--- Enter LAST_ACK.\n");
	IF StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack} = flags THEN
	  myTcpStatus.ack_seq := UpdateSeq(myTcpStatus.ack_seq, 1);
	  ChangeState(CLOSED);
	  (* XXX Fix me *)
	 (*
          Sema.V(myTcpStatus.semaLastAck);
	  *)
	END;
      (* ------------------------------------------------- *)
      | CLOSING =>
        PrintDispatchMessage("--- Enter CLOSING.\n");
	IF StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack} = flags THEN
	  myTcpStatus.ack_seq := UpdateSeq(myTcpStatus.ack_seq, 1);
	  ChangeState(TIME_WAIT);
	END;
      (* ------------------------------------------------- *)
      | TIME_WAIT =>
	(* hmm... how should I check 2MSL timeout? *)
	ChangeState(CLOSED);
      (* ------------------------------------------------- *)
      ELSE
	PrintDispatchMessage("--- Enter ELSE...\n");
    END;

  END Dispatch;

PROCEDURE ProcessData(cur : StcpMbuf.T ; offset, len : CARDINAL) =
  VAR
    newBuf : REF ARRAY OF CHAR;
    thisbufsize: CARDINAL;
  BEGIN
    IF debugDispatch THEN
      IO.Put("mh_len " & Fmt.Int(cur.mh_hdr.mh_len) &
	   " offset " & Fmt.Int(offset) & " len " & Fmt.Int(len) & "\n");
    END;

    IF len > 0 THEN
      IF offset <= cur.mh_hdr.mh_len THEN
	(* ok. data is in this StcpMbuf *)
	WITH
	  buf = StcpMbuf.Array(cur)^
         DO
	  thisbufsize := cur.mh_hdr.mh_len - offset;
	  (* thisbufsize is based on etherpacket size with min 60 bytes.
	     The TCP packet could be shorter, based on the ip length (len).
	   *)
	  IF thisbufsize > len THEN
	    thisbufsize := len; 
	  END;
	  (* XXX need to optimize *)
	  IF contents = NIL THEN
	    contents := NEW(REF ARRAY OF CHAR, BufSize);
	  END;
	  IF NUMBER(contents^) < contentsSize+thisbufsize THEN
	    WITH newSize = BufSize * ((contentsSize+thisbufsize) DIV BufSize + 1) DO
	         newBuf := NEW(REF ARRAY OF CHAR, newSize);
	         SUBARRAY(newBuf^, 0, contentsSize) :=
		   SUBARRAY(contents^, 0, contentsSize);
	         contents := newBuf;
	    END;
	  END;

	  (* copy date into pre-allocated buf. *)
	  SUBARRAY(contents^, contentsSize, thisbufsize)
		:= SUBARRAY(buf, offset, thisbufsize);
	  INC(contentsSize, thisbufsize);
        END;
	len := len - thisbufsize;
      ELSE
	IO.Put("!!! offset > mh_len.  something is wrong!!! \n");
      END;

      (* data should be in the next Mub. *)
      IF (cur.mh_hdr.mh_next # NIL) THEN
	cur := cur.mh_hdr.mh_next;
	offset := 0;
	ProcessData(cur, offset, len);
      ELSIF len > 0 THEN
	IO.Put("!!! data is too short.  expecting more...\n");
      END;
    END;
  END ProcessData;

(* ------------------------------------------------------- *)
(* those are temporary...  It's quite different from the socket interface *)
PROCEDURE TextToRefArrayOfBytes(READONLY t: TEXT): REF ARRAY OF StcpNet.BYTE =
  VAR len: CARDINAL;
      data: REF ARRAY OF StcpNet.BYTE;
  BEGIN
    IF t = NIL THEN RETURN NIL; END;
    len := Text.Length(t);
    IF len = 0 THEN RETURN NIL; END;
    data := NEW(REF ARRAY OF StcpNet.BYTE, len);
    Text.SetChars(data^,t);
    RETURN data;
  END TextToRefArrayOfBytes;

VAR numOfGet : CARDINAL := 0;
PROCEDURE Send(msg : TEXT) =
  VAR
    tcpHeader : StcpTcpPktFormat.T;
    len : CARDINAL;
    data : REF ARRAY OF CHAR;
    m : StcpMbuf.T;
  BEGIN
    len := Text.Length(msg);

    IF len <= StcpMbuf.MHLEN THEN
      (* use just one mbuf *)
      m := StcpMbuf.m_gethdr(StcpMbuf.M_WAIT, StcpMbuf.MT_DATA);
      m.mh_hdr.mh_len := len;
      StcpMbuf.MH_ALIGN(m, m.mh_hdr.mh_len);
      StcpMbufPublic.SetPktHdrLen(m, m.mh_hdr.mh_len);

      WITH buf = StcpMbuf.Array(m)^ DO
	Text.SetChars(buf, msg);
      END;
    ELSE
      (* use a mbuf with external buf. *)
      INC(numOfGet);

      data := TextToRefArrayOfBytes(msg);
      TRY
        m := StcpMbuf.MclGetOa( data, len, NIL, msg);
      EXCEPT
      | StcpMbuf.LengthMismatch =>
        IO.Put("Internal mbuf length error\n");
      END;
    END;

    IF m = NIL THEN
      IO.Put("Can't allocate mbuf\n");
    END;

    tcpHeader.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.ack};
    PacketSend(tcpHeader, 0, m);
  END Send;

PROCEDURE PacketSend(tcp : StcpTcpPktFormat.T ;
		     size : CARDINAL ;
		     data : StcpMbuf.T) =
  VAR
    m : StcpMbuf.T;
    ip : StcpIpPktFormat.T;
    so : StcpSocketAddr.T;
    ipovl_buf : ARRAY [1 .. BYTESIZE(StcpIpPktFormat.OverlayHeader)] OF CHAR;
    ipovl_csum: Ctypes.unsigned_short;
    textLen : CARDINAL;
  BEGIN

    IF data # NIL THEN
      textLen := StcpMbuf.m_length(data);
    ELSE
      textLen := 0;
    END;

    (* allocate a buffer *)
    IF data # NIL THEN
      m := data; (* XXX don't lose the data pointer in m_prepend *)
      m := StcpMbuf.M_PREPEND(m, tcp_hdr_len+8, StcpMbuf.M_WAIT);
    ELSE
      m := StcpMbuf.m_gethdr(StcpMbuf.M_WAIT, StcpMbuf.MT_DATA);
      m.mh_hdr.mh_len := tcp_hdr_len+8;
      StcpMbuf.MH_ALIGN(m, m.mh_hdr.mh_len);
      StcpMbufPublic.SetPktHdrLen(m, m.mh_hdr.mh_len);
    END;

    WITH
      buf = StcpMbuf.Array(m)^,
      headerBuf = SUBARRAY(buf, 0, tcp_hdr_len),
      tcpHeader = VIEW(headerBuf, StcpTcpPktFormat.T),
      tcpExtraBuf = SUBARRAY(buf, tcp_hdr_len, 8)
     DO
      tcpHeader.sport := StcpNet.htons(myTcpStatus.sport);
      tcpHeader.dport := StcpNet.htons(myTcpStatus.dport);
      tcpHeader.seq := StcpNet.htonl(myTcpStatus.seq);

      (* XXX update myTcpStatus.seq *)
      IF StcpTcpPktFormat.Flag.syn IN tcp.flags THEN
	myTcpStatus.seq := UpdateSeq(myTcpStatus.seq, 1);
      END;
      IF StcpTcpPktFormat.Flag.fin IN tcp.flags THEN
	myTcpStatus.seq := UpdateSeq(myTcpStatus.seq, 1);
      END;
      myTcpStatus.seq := UpdateSeq(myTcpStatus.seq, textLen);

      tcpHeader.ack_seq := StcpNet.htonl(myTcpStatus.ack_seq);

      tcpHeader.xoff := 7;	  (* Is this header length field? *) 
      tcpHeader.flags := tcp.flags;
      tcpHeader.window := StcpNet.htons(32768);
      tcpHeader.urg_ptr := 0;
      tcpHeader.check := 0;
      tcpExtraBuf[0] := VAL(16_02,CHAR); tcpExtraBuf[1] := VAL(16_04,CHAR);
      tcpExtraBuf[2] := VAL(16_05,CHAR); tcpExtraBuf[3] := VAL(16_b4,CHAR);
      tcpExtraBuf[4] := VAL(16_01,CHAR); tcpExtraBuf[5] := VAL(16_03,CHAR);
      tcpExtraBuf[6] := VAL(16_03,CHAR); tcpExtraBuf[7] := VAL(16_00,CHAR);

      (* compute checksum for psudo header: ip overlay header *)
      WITH ipovl = VIEW(ipovl_buf,StcpIpPktFormat.OverlayHeader) DO
        ipovl.fill[0]:= 0;
        ipovl.fill[1]:= 0;
        ipovl.ih_x1  := 0;
        ipovl.ih_pr  := StcpIpPktFormat.IPPROTO_TCP;
        ipovl.ih_len := StcpNet.htons(size + tcp_hdr_len + 8 + textLen); (* ? + ip_hdr_len *)
	WITH s = VIEW(ipovl.ih_src, ARRAY [1..4] OF CHAR),
	     d = VIEW(ipovl.ih_dst, ARRAY [1..4] OF CHAR) DO
	  s := DefaultAddr.myIpAddress;
	  d := DefaultAddr.targetIpAddress;
	END;
      END;
      ipovl_csum := StcpNet.checksum(ipovl_buf);

      (* sum up checksum again w/ Tcp header & data *)
      tcpHeader.check := StcpMbuf.Checksum(m,ipovl_csum, size + tcp_hdr_len+8+textLen);
    END;

    (* assume that m is consumed by lower level *)
    ip.protocol := StcpIpPktFormat.IPPROTO_TCP;
    ip.tot_len := size + tcp_hdr_len + 8 + textLen;
    ip.tos := 16;
    so.sa_family := 2;		(* StcpSocketAddrRep.AF_INET *)
    StcpIpPacket.PacketSend(m, ip, so);
  END PacketSend;

(* ------------------------------------------------------- *)
PROCEDURE ChangeState(state : CARDINAL) =
  PROCEDURE State(state : CARDINAL) : TEXT =
    BEGIN
      CASE state OF
      | CLOSED => RETURN "CLOSED";
      | SYN_SENT => RETURN "SYN_SENT";
      | ESTABLISHED => RETURN "ESTABLISHED";
      | FIN_WAIT1 => RETURN "FIN_WAIT1";
      | FIN_WAIT2 => RETURN "FIN_WAIT2";
      | TIME_WAIT => RETURN "TIME_WAIT";
      | CLOSE_WAIT => RETURN "CLOSE_WAIT";
      | LAST_ACK => RETURN "LAST_ACK";
      | CLOSING => RETURN "CLOSING";
      ELSE
	RETURN "Unknown";
      END;
    END State;
  PROCEDURE PrintState(old, new : CARDINAL) =
    BEGIN
        IO.Put("---------- ");
        IO.Put("State changed from " & State(old) & " to " & State(new) & "\n");
    END PrintState;

  BEGIN
    IF debugDispatch THEN PrintState(myTcpStatus.status, state); END;
    myTcpStatus.status := state;
  END ChangeState;

(* ------------------------------------------------------- *)
(* XXX need to make tcp timeout-able. *)
VAR 
  DefaultTimeoutForSynAndFin := 1024 * 4;
  DefaultTimeout := 1024 * 4;
PROCEDURE SetTimeOut(limit : CARDINAL) =
  BEGIN
    oldMyTcpStatus := myTcpStatus;
    isErrorHappen := FALSE;
    Clock.SetAlarm(limit, Timeout, NIL);
  END SetTimeOut;

PROCEDURE Timeout(<* UNUSED *> arg: REFANY) = 
  VAR
    state : tcpStatus;
  BEGIN
    (* XXX Should I do P() and V() here? *)
    state := myTcpStatus;
    IF (state.status # CLOSED) AND (state = oldMyTcpStatus) THEN
      (* print state info for debugging *)
      (*
      IO.Put("State ("& Fmt.Int(myTcpStatus.status) & ") ");
      IO.Put("Seq ("& Fmt.Int(myTcpStatus.seq,16) & ") ");
      IO.Put("AckSeq ("& Fmt.Int(myTcpStatus.ack_seq,16) & ")\n");
      *)

      isErrorHappen := TRUE;
      (* Cancel all semaphore *)
      Sema.Broadcast(myTcpStatus.semaEstablished);
      Sema.Broadcast(myTcpStatus.semaCloseWait);
     (*
      Sema.Broadcast(myTcpStatus.semaLastAck);
      *)

    ELSE
      IF (state.status = ESTABLISHED) OR (state.status = SYN_SENT) THEN
	SetTimeOut(DefaultTimeout);
      END;
    END;
  END Timeout;

(* ------------------------------------------------------- *)
(* Active Open() *)
PROCEDURE Connect() RAISES {NoPortAvailable, NoResponseFromServer} =
  VAR
    prevPort : CARDINAL;
    ret : BOOLEAN;
  BEGIN
    (* retry two times... *)
    prevPort := myTcpStatus.sport-basePort;
    FOR i := 1 TO numOfPort DO
      myTcpStatus.sport := ((prevPort+i) MOD numOfPort) + basePort;
      myTcpStatus.dport := DefaultAddr.defaultHttpdPort;
      (*
      IO.Put("trying to use sport: " & Fmt.Int(myTcpStatus.sport) & "\n");
       *)
      TRY
	ret := ConnectLow();
	IF ret THEN
	  (* contents := " "; *)
	  contentsSize := 0;
	  RETURN
	END;
      EXCEPT
	| NoResponseFromServer(msg) =>
	  RAISE NoResponseFromServer(msg);
      END;
    END;

    (* XXX hopefully, not reach here... *)
    RAISE NoPortAvailable("No port available");
  END Connect;

(* XXX currently not used.
PROCEDURE CheckPortAvailable() : BOOLEAN =
  BEGIN
    IF (myTcpStatus.status # CLOSED) AND (myTcpStatus.status # SYN_SENT) THEN
      IF debug THEN
	IO.Put("Port (" & Fmt.Int(myTcpStatus.sport) &
	       ") not available.  ConnectLow returned false.\n");
      END;
      RETURN FALSE;
    END;
    RETURN TRUE;
  END CheckPortAvailable;
*)

PROCEDURE InitTcpStatus() =
  BEGIN
    isOpenWaiting := TRUE;
    myTcpStatus.status := CLOSED;
    myTcpStatus.ack_seq := 0;
    myTcpStatus.seq := UpdateSeq(myTcpStatus.seq, 100);

(*
    IF (myTcpStatus.sema = NIL) THEN 
      (* XXX How can this happen? *)
      myTcpStatus.sema := Sema.Alloc(1);
      myTcpStatus.semaEstablished := Sema.Alloc(0);
      myTcpStatus.semaCloseWait := Sema.Alloc(0);
      myTcpStatus.semaLastAck := Sema.Alloc(0);
 *)
    (* XXX Can I do this safely? *)
    Sema.Broadcast(myTcpStatus.semaEstablished);
    Sema.Broadcast(myTcpStatus.semaCloseWait);

  END InitTcpStatus;

PROCEDURE BlockUntilEstablished () : BOOLEAN =
  VAR
    tcpHeader : StcpTcpPktFormat.T;
  BEGIN
    (* XXX ugly... *)
    IF debug THEN IO.Put("Blocked at ConnectLow\n"); END;

    Sema.P(myTcpStatus.semaEstablished);

    IF debug THEN IO.Put("Unblocked at ConnectLow\n"); END;

    (* see if this V() is done by Timeout routine *)
    isOpenWaiting := FALSE;
    EVAL Clock.CancelAlarm(Timeout, NIL);
    IF isErrorHappen THEN
      IO.Put("Connect: timeout error\n");

      (* XXX better to reset the connection... *)
      tcpHeader.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.rst}; 
      PacketSend(tcpHeader, 0, NIL);
      Sema.P(myTcpStatus.sema);
      ChangeState(CLOSED);
      Sema.V(myTcpStatus.sema);
      RETURN TRUE;
    END;
    RETURN FALSE;
  END BlockUntilEstablished;

PROCEDURE ConnectLow() : BOOLEAN RAISES {NoResponseFromServer} =
  VAR
   tcpHeader : StcpTcpPktFormat.T;
  BEGIN
    Sema.P(myTcpStatus.sema);
   (*
    IF CheckPortAvailable() THEN
      Sema.V(myTcpStatus.sema);
      RETURN FALSE;
    END;
    *)
    InitTcpStatus();

    ChangeState(SYN_SENT);
    Sema.V(myTcpStatus.sema);

    SetTimeOut(DefaultTimeoutForSynAndFin);
    tcpHeader.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.syn}; 
    PacketSend(tcpHeader, 0, NIL);

    (* wait until any packet to myTcpStatus.sport comes in *)
    IF BlockUntilEstablished() THEN
      RAISE NoResponseFromServer("Get no SYN"); 
    END;

    RETURN myTcpStatus.status = ESTABLISHED;
  END ConnectLow;

(* ------------------------------------------------------- *)
PROCEDURE Recv(VAR buf : REF ARRAY OF CHAR) RAISES {NoResponseFromServer} =
  BEGIN
    SetTimeOut(DefaultTimeout);

    (* XXX ugly... wait until the server send fin *)
    IF debug THEN IO.Put("Blocked at Get\n"); END;
    Sema.P(myTcpStatus.semaCloseWait);

    EVAL Clock.CancelAlarm(Timeout, NIL);
    IF isErrorHappen THEN
      (* XXX better to reset the connection... *)
      Sema.P(myTcpStatus.sema);
      ChangeState(CLOSED);
      Sema.V(myTcpStatus.sema);
      RAISE NoResponseFromServer("Get no 1st Fin");
    END;

    buf := NEW(REF ARRAY OF CHAR, contentsSize);
    IF buf # NIL THEN
      SUBARRAY(buf^, 0, contentsSize) := SUBARRAY(contents^, 0, contentsSize);
    ELSE
      (* XXX how should we deal with this? *)
      (* Does NEW() return NIL if it fails? *)
      IO.Put("!!! Recv: Can't allocate " & Fmt.Int(contentsSize) & " bytes\n");
    END;
  END Recv;

(* ------------------------------------------------------- *)
PROCEDURE Close() =
  PROCEDURE CloseLow() =
    VAR
     tcpHeader : StcpTcpPktFormat.T;
    BEGIN
      (*  make FIN packet and send it. *)
      tcpHeader.flags := StcpTcpPktFormat.Flags{StcpTcpPktFormat.Flag.fin,
					    StcpTcpPktFormat.Flag.ack}; 
      Sema.P(myTcpStatus.sema);
      IF (myTcpStatus.status = ESTABLISHED) THEN
        (* Active Close *)
        ChangeState(FIN_WAIT1);
      ELSIF (myTcpStatus.status = CLOSE_WAIT) THEN
        (* On the way of Passive Close *)
        ChangeState(LAST_ACK);
      ELSE
        IF debug THEN IO.Put("nope.  we should n't send FIN now\n"); END;
      END;
      Sema.V(myTcpStatus.sema);

      PacketSend(tcpHeader, 0, NIL);
    END CloseLow;
  BEGIN
    CloseLow();

    (* XXX ugly... wait until the server send ack for LAST_ACK *)
   (*
    IF debug THEN IO.Put("Blocked at Get2\n"); END;
    SetTimeOut(DefaultTimeoutForSynAndFin);
    Sema.P(myTcpStatus.semaLastAck);
    EVAL Clock.CancelAlarm(Timeout, NIL);
    *)
    (* hmm... just ignore it *)
    (* IF isErrorHappen THEN END; *)

  END Close;

PROCEDURE FreeMemory() =
  BEGIN
    IF contents # NIL THEN
      (*
      IO.Put("SimpleHttp: " & Fmt.Int(NUMBER(contents^)) & " bytes freed.\n");
      *)
      contents := NIL;
    END;
  END FreeMemory;
(* ------------------------------------------------------- *)
PROCEDURE Init() =
  VAR
    clockRate : INTEGER;
  BEGIN
    (* get number of ticks/sec and set timeout *)
    clockRate := Clock.InterruptRate();
    IF clockRate = 0 THEN
      IO.Put("StcpTcpPacket: Clock.ClockInterruptRate() returns 0!\n");
      IO.Put("  looks like you are using loom boxes.  Setting it to 100\n");
      clockRate := 100;
    END;
    (* timeout after 4 sec for the time being *)
    DefaultTimeoutForSynAndFin := 4 * clockRate;
    DefaultTimeout := 4 * clockRate;
    IO.Put("  StcpTcpPacket: timeout is " & Fmt.Int(DefaultTimeout) & "ticks\n");
    myTcpStatus.status := CLOSED;
    myTcpStatus.seq := 99999;
    myTcpStatus.ack_seq := 99999;
    myTcpStatus.sport := basePort + numOfPort;
    myTcpStatus.dport := DefaultAddr.defaultHttpdPort;
    myTcpStatus.sema := Sema.Alloc(1);
    myTcpStatus.semaEstablished := Sema.Alloc(0);
    myTcpStatus.semaCloseWait := Sema.Alloc(0);
    myTcpStatus.semaLastAck := Sema.Alloc(0);

  END Init;

BEGIN
  Init();
END StcpTcpPacket.


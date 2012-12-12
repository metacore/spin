(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 20-Jun-97  Tsutomu Owa (owa) at the University of Washington
 *	All incoming packets are freed at AsyncDeliver().
 *	Don't handle arp packets. 
 *
 * 24-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Removed the quick hack for vx driver.
 *
 * 21-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	fixed a bug with the vx (3c905) driver for loom boxes.
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created for simpile tcp.
 *	Some code came from user/net/ether/src/StcpEtherPacket.i3 
 *)

UNSAFE (* to set spl *)
MODULE StcpEtherPacket;


IMPORT StcpEtherPktFormat, StcpMbuf, StcpMbufPublic, Thread,
       ThreadExtra, CPUPrivate, CPU, Sema,
       IO, StcpIf, StcpSocketAddr, Ctypes;
IMPORT Word;
IMPORT Fmt;
IMPORT StcpUtils;
IMPORT StcpIpPacket ;
(* IMPORT StcpEtherArp; *)
IMPORT StcpNet;
IMPORT DefaultAddr;
(* IMPORT Debugger; *)
IMPORT StcpEtherDev;

VAR
  debug:=FALSE;
  bcast:=TRUE;

(* ---------------------------------------------------------------- *)
(* synchronization and queue for incoming packets *)
CONST
  eth_hdr_len = BYTESIZE(StcpEtherPktFormat.T);

VAR
  process_packet   : Sema.T;
  io_thread        : Thread.T;
  ifq              : StcpIf.ifqueue;

PROCEDURE ArrivedGuard(<*UNUSED*>dev: StcpEtherDev.T; m: StcpMbuf.T): BOOLEAN =
  VAR
    cur: StcpMbuf.T;
    offset: CARDINAL;
  BEGIN
    cur := CheckStcpMbuf(m);
    IF cur = NIL THEN RETURN FALSE END;

    WITH
      buf = StcpMbuf.Array(cur)^,
      ethHeadBuf = SUBARRAY(buf, 0, eth_hdr_len),
      ethHeader = VIEW(ethHeadBuf, StcpEtherPktFormat.T)
     DO
      offset := eth_hdr_len;
      IF ethHeader.type = StcpEtherPktFormat.ETHERTYPE_IP THEN
	RETURN StcpIpPacket.PacketGuard(m, cur, offset);
      END;
    END;
    RETURN FALSE
  END ArrivedGuard;

(* ---------------------------------------------------------------- *)
(*
 * Arrived()
 * 
 * Takes an incoming packet from the ethernet and pushes it up through
 * the protocol decission tree.
 *
 * For asynchronous dispatch enqueue the packet and kick the worker
 * thread.  Otherwise, the event handlers are invoked as part of the
 * interrupt handler.  This support will be supported and checked by
 * the SPIN dispatcher.
 *
 * Executed at CPU.InterruptLevel.High.
 *)

PROCEDURE Arrived( dev: StcpEtherDev.T; m: StcpMbuf.T) =
  <*UNUSED*>
  CONST
    header_len = BYTESIZE(StcpEtherPktFormat.Header);
  BEGIN
    StcpMbufPublic.SetPktHdrRcvIf(m,dev);
    StcpIf.Enqueue(ifq,m);
    (* kick the I/O thread. *)
    Sema.V(process_packet);
  END Arrived; 


PROCEDURE AsyncDeliver(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR packet: StcpMbuf.T;
      spl: CPU.InterruptLevel;
    BEGIN
    LOOP
      Sema.P(process_packet);
      spl := CPUPrivate.SetInterruptMask(
		CPUPrivate.InterruptClass.High);
      packet := StcpIf.Dequeue(ifq);
      CPUPrivate.RestoreInterruptMask(spl);        
      EVAL PacketArrived(packet, packet, 0);
      (* all incoming packets are freed here. *)
      StcpMbuf.m_freem(packet);
    END;
  END AsyncDeliver;

(* ---------------------------------------------------------------- *)
(* see if this mbuf chain starts with packet header set...*)
PROCEDURE CheckStcpMbuf(packet : StcpMbuf.T) : StcpMbuf.T =
  VAR
    m : StcpMbuf.T;
    len1, len2 : CARDINAL;
  BEGIN
    m := packet;

    IF m = NIL THEN RETURN NIL; END;

    IF (Word.And(m.mh_hdr.mh_flags, StcpMbuf.M_PKTHDR) = 0) THEN
      IO.Put("StcpEtherPacket: No M_PKTHDR set\n");
      RETURN NIL;
    ELSE
      len1 := StcpMbufPublic.GetPktHdrLen(m);
      len2 := StcpMbuf.m_length(m);
      IF len1 # len2 THEN
	IO.Put("StcpEtherPacket: PktHdrLen(" & Fmt.Int(len1) & ") " &
		"# m_length(" & Fmt.Int(len2) & ")\n");
	RETURN NIL;
      END;
      IF m.mh_hdr.mh_len < eth_hdr_len THEN
	WHILE (m.mh_hdr.mh_len < eth_hdr_len) AND (m.mh_hdr.mh_next # NIL) DO
	 (*
	  IO.Put("StcpEtherPacket: Advance mbuf\n");
	  *)
	  m := m.mh_hdr.mh_next;
	END;
	IF m.mh_hdr.mh_len < eth_hdr_len THEN 
	  IO.Put("StcpEtherPacket: too short (" & Fmt.Int(m.mh_hdr.mh_len) & ")\n");
	  RETURN NIL;
	END;
      END;

      RETURN m;
(* hmm... should I check following data at this moment?
      WHILE (m.mh_hdr_next # NIL) DO END;
 *)
    END;
  END CheckStcpMbuf;

(* ---------------------------------------------------------------- *)
(* see if this is my packet, if not just return it *)
PROCEDURE PacketArrived(m, cur : StcpMbuf.T ; offset : CARDINAL) : StcpMbuf.T =
  BEGIN
    cur := CheckStcpMbuf(m);
    IF cur = NIL THEN RETURN m; END;

    WITH
      buf = StcpMbuf.Array(cur)^,
      ethHeadBuf = SUBARRAY(buf, 0, eth_hdr_len),
      ethHeader = VIEW(ethHeadBuf, StcpEtherPktFormat.T)
     DO
      IF debug THEN
	PrintEtherHeader(ethHeader);
	StcpUtils.PrintStcpMbuf(m);
      END;

      INC(offset, eth_hdr_len);

      CASE ethHeader.type OF
	| StcpEtherPktFormat.ETHERTYPE_IP =>
	  RETURN StcpIpPacket.PacketArrived(m, cur, offset);
      (*
	| StcpEtherPktFormat.ETHERTYPE_ARP =>
	  RETURN StcpEtherArp.PacketArrived(m, cur, offset);
       *)
      ELSE
	(* nope, this is not mime *)
	RETURN m;
      END;
    END;
  END PacketArrived;

(* ---------------------------------------------------------------- *)
    
PROCEDURE Output(dev: StcpEtherDev.T; m: StcpMbuf.T;): Ctypes.int = 
  VAR s : CPU.InterruptLevel;
  BEGIN

    (* XXX Would a LOCK do here instead of spl?  db *)
    s := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.IO);
    IF dev#NIL THEN
      IO.Put("StcpEtherPacket.Output: dev not NIL\n");
    ELSE
      StcpEtherDev.Send(dev, m);
    END;

    CPUPrivate.RestoreInterruptMask(s);

    RETURN 0;
  END Output;


(* ---------------------------------------------------------------- *)
PROCEDURE PacketSend(
  VAR dev : StcpEtherDev.T;
  mbuf    : StcpMbuf.T;
  VAR s   : StcpSocketAddr.T;
  <* UNUSED *> rte     : ADDRESS := NIL) : Ctypes.int =
  VAR error : Ctypes.int;
  PROCEDURE Ip() : Ctypes.int =
    (* In general, you should extract source&dest IP address and
     * convert it to ether address here.  But do we need it? nope!
     *)
    VAR
      header : StcpMbuf.T;
    BEGIN
      (* basically add header and send it *)
      header := StcpMbuf.M_PREPEND(mbuf,eth_hdr_len,StcpMbuf.M_WAIT);

      IF header = NIL THEN
	(* XXX What should I do?  Ask upper protocol to re-send it? *)
	IO.Put("StcpEtherPacket:Ip() can't allocate mbuf\n");
	RETURN -1;
      END;
      WITH
        ether_data = StcpMbuf.Array(header)^, (* get the ether_data area *)
        ether_hdr  = VIEW(SUBARRAY(ether_data, 0, eth_hdr_len),StcpEtherPktFormat.T)
       DO
        ether_hdr.dhost := DefaultAddr.targetHardwareAddress;
        ether_hdr.shost := DefaultAddr.myHardwareAddress;
        ether_hdr.type  := StcpNet.htons(StcpEtherPktFormat.ETHERTYPE_IP);
      END;
      RETURN Output(dev, header);

    END Ip;
  PROCEDURE Raw(): Ctypes.int =
    BEGIN
        RETURN Output(dev, mbuf);
    END Raw;

  BEGIN
    CASE s.sa_family OF
    (* IP packet assumes that mbuf starts with ip header *)
    | 2 =>		(* SocketRep.AF_INET *)
      error := Ip();
    (* assumes header starts with network layer information *)
    | 0 => 		(* SocketRep.AF_UNSPEC *)
      error :=  Raw();
    (* assumes header starts with original network layer information *)
    | 127 => 		(* XXX not defined *)
      error :=  Raw();
    ELSE
      IO.Put("Address Family not supported.\n");
      error := 0;
    END;
    RETURN error;
  END PacketSend;

(* ---------------------------------------------------------------- *)
PROCEDURE PrintEtherHeader(ethHeader : StcpEtherPktFormat.T) =
  VAR type : TEXT;
  BEGIN

    IF (NOT bcast) AND (ethHeader.dhost = StcpEtherPktFormat.broadcast) THEN
      RETURN;
    END;

    IO.Put("EtherHeader: "); 
    StcpUtils.PrintArrayOfChar(VIEW(ethHeader.shost, ARRAY [1..6] OF CHAR),
			6, 16, FALSE);
    IO.Put(" -> ");
    StcpUtils.PrintArrayOfChar(VIEW(ethHeader.dhost, ARRAY [1..6] OF CHAR),
			6, 16, FALSE);
   CASE ethHeader.type OF
      | StcpEtherPktFormat.ETHERTYPE_PUP => type := "ETHERTYPE_PUP";
      | StcpEtherPktFormat.ETHERTYPE_IP => type := "ETHERTYPE_IP";
      | StcpEtherPktFormat.ETHERTYPE_ARP => type := "ETHERTYPE_ARP";
      | StcpEtherPktFormat.ETHERTYPE_LAT => type := "ETHERTYPE_LAT";
      | StcpEtherPktFormat.ETHERTYPE_DECNET => type := "ETHERTYPE_DECNET";
      | StcpEtherPktFormat.ETHERTYPE_MOPRC => type := "ETHERTYPE_MOPRC";
      | StcpEtherPktFormat.ETHERTYPE_MOPDL => type := "ETHERTYPE_MOPDL";
      | StcpEtherPktFormat.ETHERTYPE_LBACK => type := "ETHERTYPE_LBACK";
      | StcpEtherPktFormat.ETHERTYPE_AM => type := "ETHERTYPE_AM";
    ELSE
      type := "!!! UNKNOWN " & Fmt.Int(ethHeader.type, 16);
    END;
    IO.Put("  " & type & "\n");
  END PrintEtherHeader;

(* ---------------------------------------------------------------- *)
(*
 * Init()
 * Used to initialize this module, since we cannot rely on M3
 * initialization order.
 *)
PROCEDURE Init() = 
  BEGIN

    (* create the sema used to sync with the thread *)
    process_packet := Sema.Alloc();
    (* create ether packet thread *)
    io_thread := ThreadExtra.PFork(AsyncDeliver,
			NIL(* XXX , NIL, ThreadExtra.defaultPriority+2*));

    (* ifq initialization *)
    ifq.ifq_head := NIL;
    ifq.ifq_tail := NIL;
    ifq.ifq_len := 0;
    ifq.ifq_maxlen := 512;
    ifq.ifq_drops := 0;

  END Init;

BEGIN
END StcpEtherPacket.


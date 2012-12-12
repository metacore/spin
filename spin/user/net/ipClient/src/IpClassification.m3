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
 * 27-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Eliminated use of Net.subarray and Net.subfree.  
 *	Changed to use three argument PacketArrived event.
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use new spin shell command style.
 *
 * 02-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Handles various packet formats and pushes them up to the next
 *	protocol level by raising their event handler.  Each handler
 *	should live in its own file, but for now we'll leave it the way
 *	it is.
 *
 * 27-Feb-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE IpClassification;
IMPORT IO, Icmp, Ip, IpPktFormat, Mbuf, Net, Spy, Tcp, Udp, Ctypes,
       Word, IpClient, ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ipclassifcation *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSE
        Usage();
      END;
    EXCEPT
      ParseParams.Error => Usage();
    END;
   RETURN TRUE;
 END Run;

PROCEDURE Usage() = 
  BEGIN
    IO.Put("Usage: " & CommandName & ":" & CommandHelp & "\n");
  END Usage;

VAR
  debug_level : Net.Level := Net.oLevel.NODEBUG;

VAR 
  icmp: REFANY;
  udp : REFANY;
  tcp : REFANY;

CONST 
  timing = IpClient.timing;
  debug  = FALSE;

VAR
  icmptimer    : Spy.T;
  udptimer     : Spy.T;
  tcptimer     : Spy.T;
  udpdataflow  : Spy.T;
  tcpdataflow  : Spy.T;
  icmpdataflow : Spy.T;

CONST
  ip_hdr_len = BYTESIZE(IpPktFormat.T);
CONST
  getMF = Word.Not(Word.Or(Word.Or(IpPktFormat.IP_OFFSET,
                                   IpPktFormat.IP_CE),
                           IpPktFormat.IP_DF));
                          

<*INLINE*>
FUNCTIONAL
PROCEDURE IsIpFrag(frag_off: Ctypes.unsigned_short)
  : BOOLEAN = 
  VAR
    mf: Word.T;
    fragSize : Word.T;
  BEGIN
    frag_off := Net.nstoh(frag_off); (* XXX put into host format *)
    mf := Word.And(frag_off, getMF);
    fragSize := Word.And(frag_off, IpPktFormat.IP_OFFSET);
    RETURN mf = IpPktFormat.IP_MF OR fragSize # 0;
  END IsIpFrag;

FUNCTIONAL
PROCEDURE Guard_ICMP(
    <* UNUSED *> packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN =
  VAR
  BEGIN
(*
    IF timing THEN Spy.Enter(icmptimer); END;
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"IpCl.ICMP.WC ");
    END;
*)

    WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,ip_hdr_len),
         ipHeader = VIEW(ipHeaderBuf,T)
     DO
      RETURN ipHeader.protocol = IpPktFormat.IPPROTO_ICMP AND (* ICMP packet ? *)
      ipHeader.hlen = 5 AND (* No IP options ? *)
      ipHeader.vers = 4 AND (* IP version 4  ? *)
      IsIpFrag(ipHeader.frag_off) # TRUE AND (* Fragmented ? *)
      TRUE; (* something that determines that the packet is for me *)
    END;
  END Guard_ICMP;

PROCEDURE PacketArrived_ICMP(
    packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN =
  BEGIN
    WITH currBuf = Mbuf.Array(curr)^,
      ipHeaderBuf = SUBARRAY(currBuf,offset,ip_hdr_len),
      ipHeader = VIEW(ipHeaderBuf,T)
     DO
      IF timing THEN Spy.Exit(icmptimer); END;
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level,Net.oLevel.INFO,"IpCl.ICMP.PA ");
      END;

      INC(offset,ipHeader.hlen*4);
      (* XXX need better way to find actual mbuf  *)
      IF offset >= BYTESIZE(currBuf) THEN
        curr := curr.mh_hdr.mh_next;
        <* ASSERT(curr # NIL) *>
        offset := 0;
      END;

      EVAL Icmp.PacketArrived(packet,curr,offset);
    END;
    RETURN FALSE;
  END PacketArrived_ICMP;

FUNCTIONAL
PROCEDURE Guard_UDP(
    <*UNUSED*> packet: Mbuf.T;
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN =
  BEGIN
(*
    IF timing THEN Spy.Enter(udptimer); END;
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"IpCl.UDP.WC ");
    END;
*)

    WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,ip_hdr_len),
         ipHeader = VIEW(ipHeaderBuf,T)
     DO
      RETURN ipHeader.protocol = IpPktFormat.IPPROTO_UDP AND (* UDP packet ? *)
      ipHeader.hlen = 5 AND (* No IP options ? *)
      ipHeader.vers = 4 AND (* IP version 4  ? *)
      IsIpFrag(ipHeader.frag_off) # TRUE AND (* Fragmented ? *)
      TRUE; (* something that determines that the packet is for me *)
    END;
  END Guard_UDP;

PROCEDURE PacketArrived_UDP(packet: Mbuf.T; curr: Mbuf.T; offset: CARDINAL):BOOLEAN =
  BEGIN
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"IpCl.UDP.PA ");
    END;

    WITH currBuf = Mbuf.Array(curr)^,
      ipHeaderBuf = SUBARRAY(currBuf,offset,ip_hdr_len),
      ipHeader = VIEW(ipHeaderBuf,T)
     DO
      INC(offset,ipHeader.hlen*4);
      (* XXX need better way to find actual mbuf  *)
      IF offset >= BYTESIZE(currBuf) THEN
        curr := curr.mh_hdr.mh_next;
        <* ASSERT(curr # NIL) *>
        offset := 0;
      END;


      IF timing THEN
        Spy.Exit(udptimer);
        (* NOT ON IX86_SPIN
        WITH timer = VIEW(packet.mh_hdr.mh_union[0], INTEGER),
             stop = SAL.Timestamp() 
         DO
          Spy.Hit(udpdataflow,timer,stop);
          timer := stop;
        END;
        *)
      END;

      EVAL Udp.PacketArrived(packet,curr,offset);
    END;
    RETURN FALSE; (* return true if consuming packet *)
  END PacketArrived_UDP;

FUNCTIONAL
PROCEDURE Guard_TCP(
    <*UNUSED*> packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN =
  BEGIN
(*
    IF timing THEN Spy.Enter(tcptimer); END;
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"IpCl.TCP.WC ");
    END;
*)

    WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,ip_hdr_len),
         ipHeader = VIEW(ipHeaderBuf,T)
     DO
      RETURN ipHeader.protocol = IpPktFormat.IPPROTO_TCP AND (* TCP packet ? *)
      ipHeader.hlen = 5 AND (* No IP options ? *)
      ipHeader.vers = 4 AND (* IP version 4  ? *)
      IsIpFrag(ipHeader.frag_off) # TRUE AND (* Fragmented ? *)
      TRUE; (* something that determines that the packet is for me *)
    END;
  END Guard_TCP;

PROCEDURE PacketArrived_TCP(packet: Mbuf.T; curr: Mbuf.T; offset: CARDINAL):BOOLEAN =
  BEGIN
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"IpCl.TCP.PA ");
    END;

    WITH currBuf = Mbuf.Array(curr)^,
      ipHeaderBuf = SUBARRAY(currBuf,offset,ip_hdr_len),
      ipHeader = VIEW(ipHeaderBuf,T)
     DO
      
      INC(offset,ipHeader.hlen*4);
      (* XXX need better way to find actual mbuf  *)
      IF offset >= BYTESIZE(currBuf) THEN
        curr := curr.mh_hdr.mh_next;
        <* ASSERT(curr # NIL) *>
        offset := 0;
      END;

      IF timing THEN 
        Spy.Exit(tcptimer);
        (*
        WITH timer = VIEW(packet.mh_hdr.mh_union[0],INTEGER),
             stop = SAL.Timestamp()
         DO
          Spy.Hit(tcpdataflow,timer,stop);
          timer := stop;
        END;
        *)
      END;
      EVAL Tcp.PacketArrived(packet,curr,offset);

    END;
    RETURN FALSE; (* return true if consuming packet *)
  END PacketArrived_TCP;


PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
       udp := Ip.Install(Ip.PacketArrived,
                         Guard_UDP,
                         PacketArrived_UDP);

       icmp := Ip.Install(Ip.PacketArrived,
                          Guard_ICMP,
                          PacketArrived_ICMP);

       tcp := Ip.Install(Ip.PacketArrived,
                         Guard_TCP,
                         PacketArrived_TCP);

       IF timing THEN
         icmptimer    := Spy.Create("ip_input icmp");
         udptimer     := Spy.Create("ip_input udp");
         tcptimer     := Spy.Create("ip_input tcp");
         udpdataflow  := Spy.Create("device.PA -> IpUdp.PA");
         tcpdataflow  := Spy.Create("device.PA -> IpTcp.PA");
         icmpdataflow := Spy.Create("device.PA -> IpIcmp.PA");
       END;
       IF verbose THEN IO.Put("IpClassification module initialized.\n"); END;
  END Init;

BEGIN
END IpClassification. 

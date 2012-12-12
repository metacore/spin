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
 * 24-Apr-96  Trent Piepho (tap) at the University of Washington
 *      Rewrote IP frag reassembly.
 *
 * 28-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *  Eliminated use of Net.subarray() and Net.subfree.
 *  Now using new PacketArrived event type.
 *  Added TCP frag reassembler handler.
 *
 * 12-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *  Replaced LOOPHOLE with VIEW.  Changed Clib.Print to IO.Put.
 *  Using language based array copies, rather than bcopy explicitly.
 *  Changed packet buffer from CHAR types to Net.BYTE.
 *
 * 12-Mar-95  Marc Fiuczynski (mef) at the University of Washington
 *  Whisted.
 *
 *)

MODULE IpFrag;
IMPORT Mbuf, MbufPublic, Ctypes;
IMPORT IO; <* NOWARN *>
IMPORT Net;
IMPORT IpPktFormat;
IMPORT Ip; (* event we are handling *)
IMPORT Udp; (* event we are raising *)
IMPORT Tcp; (* event we are raising *)
IMPORT Word;
IMPORT StrongRef; <* NOWARN *>
IMPORT Clock;
IMPORT IpFragTbl, IpFragKey;

IMPORT Spy;
(* IMPORT SAL; *)

IMPORT ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ipfrag *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-walk") THEN
        Walk();
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

CONST
  ip_hdr_len = BYTESIZE(IpPktFormat.Header);

CONST
  DefaultTimeout = 1024 * 30; (* 30 seconds *) 

VAR
  udp : REFANY;
  tcp : REFANY;

VAR
  fragTable: IpFragTbl.Default;
  timeoutPending: BOOLEAN;
  mutex: MUTEX;
  debug_level : Net.Level := Net.oLevel.NODEBUG;

VAR 
  udpreassemblytimer: Spy.T;
  tcpreassemblytimer: Spy.T;

CONST 
  timing = FALSE;
  debug  = FALSE;

PROCEDURE Walk() = 
  VAR
    key       : IpFragKey.T;
    entry     : T;
    size : CARDINAL;
  BEGIN
    LOCK mutex DO
      size := fragTable.size();
      IO.Put("Frag tabel has "); IO.PutInt(size); IO.Put(" entries\n");
      WITH iterate = fragTable.iterate() DO
        WHILE iterate.next(key,entry) = TRUE DO
          IO.Put("frag curr timestamp ");
          IO.PutInt(Clock.ReadTicks());
          IO.Put(" entry timestamp ");
          IO.PutInt(entry.timestamp);
          IO.Put(" size ");          
          IO.PutInt(entry.tot_len);
          IO.Put("\n");          
        END;
      END;
    END;
  END Walk;

PROCEDURE Timeout(<* UNUSED *> arg: REFANY) =
  VAR
    curpacket : REF IpFragPacket;
    key       : IpFragKey.T;
    entry     : T;
    oldtime   : INTEGER;
  BEGIN
    LOCK mutex DO
      oldtime := Clock.ReadTicks() - DefaultTimeout;
      WITH iterate = fragTable.iterate()
       DO
        WHILE iterate.next(key,entry) = TRUE DO
          (* delete old fragments *)
          IF entry.timestamp < oldtime THEN
            curpacket := entry.packets;
            WHILE (curpacket # NIL) DO
              Mbuf.m_freem(curpacket.data);
              curpacket := curpacket.next;
            END;
            EVAL fragTable.delete(key,entry);
          END;
        END;
      END;
      IF fragTable.size() = 0 THEN
        timeoutPending := FALSE;
      ELSE
        (* If there are still more fragments in the table, schedule another check *)
        timeoutPending := TRUE;
        Clock.SetAlarm(DefaultTimeout,Timeout,NIL);
      END;
    END;
  END Timeout;

PROCEDURE Reassemble(<* UNUSED *>packet: Mbuf.T; curr: Mbuf.T; ipOffset: CARDINAL; VAR newpacket: Mbuf.T) =
  VAR 
    key         : IpFragKey.T;
    entry       : T;
    inFragTable : BOOLEAN;
    curpacket   : REF IpFragPacket;
    c,p         : REF IpFragPacket; 
    m           : Mbuf.T;
  BEGIN
    WITH payload   = Mbuf.Array(curr),
         headerBuf = SUBARRAY(payload^, ipOffset,ip_hdr_len),
         (* IP header *)
         header    = VIEW(headerBuf,NewT),
         (* packet ID *)
         hid       = Net.nstoh(header.id),
         (* fragment offset *)
         hfo       = Net.nstoh(header.frag_off),
         (* source address *)
         hsa       = Net.nltoh(VIEW(header.saddr,BITS 32 FOR Ctypes.unsigned_int)),
         (* dest address *)
         hda       = Net.nltoh(VIEW(header.daddr,BITS 32 FOR Ctypes.unsigned_int)),
         (* length of packet *)
         hln       = Net.nstoh(header.tot_len),
         (* protocol *)
         hpr       = header.protocol,
         (* length of ip header *)
         iphdrlen  = header.hlen*4,
         (* length of data *)
         datalen  = hln-iphdrlen,
         (* fragment offset, again *)
         offset   = Word.LeftShift(Word.And(hfo,IpPktFormat.IP_OFFSET),3),
         (* more fragments flag *)
         ipmf     = Word.And(Word.And(hfo,Word.Not(IpPktFormat.IP_OFFSET)),IpPktFormat.IP_MF)
     DO
      (* construct the new IpFragPacket to stick in the table *)
      curpacket := NEW(REF IpFragPacket);
      (* save the header of the first packet, drop the header of subsequent packets *)
      IF (offset = 0)  THEN
        curpacket.data := Mbuf.m_copym(curr,0,hln,Mbuf.M_WAIT);
        curpacket.length := hln;
        curpacket.offset := offset;
      ELSE 
        curpacket.data := Mbuf.m_copym(curr,iphdrlen,datalen,Mbuf.M_WAIT);
        curpacket.length := datalen;
        curpacket.offset := offset+iphdrlen;
      END;

      (* Stick the fragment in the table, either by chaining it on or making a
         new entry *)
      key.id := hid;
      key.saddr := hsa;
      key.daddr := hda;
      key.protocol := hpr;
      LOCK mutex DO
        inFragTable := fragTable.get(key,entry);
        IF NOT inFragTable THEN
          (* If there in no timeout pending, start a 30 second timer to expire
             old fragments *)
          IF NOT timeoutPending THEN
            timeoutPending := TRUE;
            Clock.SetAlarm(DefaultTimeout,Timeout,NIL);
          END;
          curpacket.next := NIL;
          entry := NEW(REF IpFrag);
          entry.packets := curpacket;
          entry.tot_len := curpacket.length;
          entry.exp_len := 0;
          entry.timestamp := Clock.ReadTicks();
          EVAL fragTable.put(key,entry);
        ELSE
          (* do an insertion sort *)
          (* packet goes at head of list *)
          IF entry.packets.offset > curpacket.offset THEN
            curpacket.next := entry.packets;
            entry.packets := curpacket;
          ELSE   (* packet isn't at head *)
            p := entry.packets;
            c := p.next;
            WHILE (c # NIL) AND (curpacket.offset > c.offset) DO
              p := c;
              c := c.next;
            END;
            p.next := curpacket;
            curpacket.next := c;
          END;
          entry.tot_len := entry.tot_len + curpacket.length;
          entry.timestamp := Clock.ReadTicks();
        END;
        (* if the ip more fragment bit is not set, calculate the expected
           length of this packet, so we will know when all the fragments have
           been received. *)
        IF (ipmf # IpPktFormat.IP_MF) THEN
          entry.exp_len := hln+offset;
        END;
        (* If we have received enough data, reassemble the packet *)
        IF entry.exp_len = entry.tot_len THEN
          (* fix up the header *)
          WITH packet      = Mbuf.Array(entry.packets.data),
               ipHeaderBuf = SUBARRAY(packet^,0,ip_hdr_len),
               ipHeader    = VIEW(ipHeaderBuf,NewT) 
           DO
            ipHeader.frag_off  := 0;
            (* the total length is all of the ip data + ip header length *)
            ipHeader.tot_len := Net.htons(entry.tot_len);
            (* recompute the checksum *)
            ipHeader.check := 0;
            ipHeader.check := Net.checksum(ipHeaderBuf,iphdrlen);
          END;
          (* now chain the mbufs *)
          curpacket := entry.packets;
          WHILE (curpacket.next # NIL) DO
            m := curpacket.data;
            WHILE m.mh_hdr.mh_next # NIL DO
              m := m.mh_hdr.mh_next;
            END;
            m.mh_hdr.mh_next := curpacket.next.data;
            curpacket := curpacket.next;
          END;
          (* return the head of the mbuf chain *)
          IF Word.And(entry.packets.data.mh_hdr.mh_flags,Mbuf.M_PKTHDR) = 0 THEN
            IO.Put("Not a packet header mbuf\n");
            newpacket := Mbuf.m_gethdr(Mbuf.M_DONTWAIT,Mbuf.MT_HEADER);
            newpacket.mh_hdr.mh_next := entry.packets.data;
          ELSE
            newpacket := entry.packets.data;
          END;
          MbufPublic.SetPktHdrLen(newpacket,entry.tot_len);
	    
          (* delete this packet from the reassembly table *)
          EVAL fragTable.delete(key,entry);
        ELSE
          (* Don't have all the fragments yet, so just return *)
          newpacket := NIL;
        END;
      END; (* mutex *)
    END;
  END Reassemble;

FUNCTIONAL
PROCEDURE IsIpFrag(frag_off: Ctypes.unsigned_short):BOOLEAN = 
  VAR
    result, t, flags: Ctypes.unsigned_short;
  BEGIN

    (* paranoid: make sure only 16 bit value *)
    (* get the frag_off field into host format *)
    t := Word.And(Net.nstoh(frag_off),LAST(Ctypes.unsigned_short));

    (* paranoid: make sure only 16 bit value *)
    flags := Word.And(Word.And(t, Word.Not(IpPktFormat.IP_OFFSET)),LAST(Ctypes.unsigned_short));

    t := Word.And(t, IpPktFormat.IP_OFFSET);
    result := Word.And(flags,IpPktFormat.IP_MF);

    IF result # IpPktFormat.IP_MF AND t = 0 THEN
      RETURN FALSE;      (* not fragmented *)
    ELSE
      RETURN TRUE;       (* it is fragmented *)
    END;
  END IsIpFrag;


FUNCTIONAL
PROCEDURE Guard_UDP(
    <* UNUSED *> packet: Mbuf.T;
    curr: Mbuf.T;
    offset: CARDINAL):BOOLEAN =
  BEGIN
    WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,ip_hdr_len),
         ipHeader = VIEW(ipHeaderBuf,NewT)
     DO
      RETURN ipHeader.protocol = IpPktFormat.IPPROTO_UDP AND (* UDP packet ? *)
      ipHeader.hlen = 5 AND (* No IP options ? *)
      ipHeader.vers = 4 AND (* IP version 4  ? *)
      IsIpFrag(ipHeader.frag_off) = TRUE AND (* Fragmented ? *)
      TRUE; (* something that determines that the packet is for me *)
    END;
  END Guard_UDP;

PROCEDURE PacketArrived_UDP(packet: Mbuf.T; curr: Mbuf.T; offset: CARDINAL):BOOLEAN =
  VAR
    newpacket: Mbuf.T;
  BEGIN
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN 
      Net.Debug(debug_level, Net.oLevel.INFO,"IpFrag.Udp.PA ");
    END;

    IF timing THEN Spy.Enter(udpreassemblytimer); END;
    Reassemble(packet,curr,offset,newpacket);
    IF timing THEN Spy.Exit(udpreassemblytimer); END;

    IF newpacket # NIL THEN
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN 
        Net.Debug(debug_level, Net.oLevel.DEBUG,"IpFrag.Udp.PA got newpacket. ");
      END;

      WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(newpacket)^,0,ip_hdr_len),
           ipHeader = VIEW(ipHeaderBuf,NewT),
           udpOffset = ipHeader.hlen*4
       DO
        EVAL Udp.PacketArrived(newpacket,newpacket,udpOffset);
        Mbuf.m_freem(newpacket);
      END;

    END;
    RETURN FALSE; (* return true if consuming packet *)
  END PacketArrived_UDP;

FUNCTIONAL
PROCEDURE Guard_TCP(
    <* UNUSED *> packet: Mbuf.T;
    curr: Mbuf.T;
    offset: CARDINAL):BOOLEAN =
  BEGIN
    WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,ip_hdr_len),
         ipHeader = VIEW(ipHeaderBuf,NewT)
     DO
      RETURN ipHeader.protocol = IpPktFormat.IPPROTO_TCP AND (* TCP packet ? *)
      ipHeader.hlen = 5 AND (* No IP options ? *)
      ipHeader.vers = 4 AND (* IP version 4  ? *)
      IsIpFrag(ipHeader.frag_off) = TRUE AND (* Fragmented ? *)
      TRUE; (* something that determines that the packet is for me *)
    END;
  END Guard_TCP; 

PROCEDURE PacketArrived_TCP(packet: Mbuf.T; curr: Mbuf.T; offset: CARDINAL):BOOLEAN =
  VAR
    newpacket: Mbuf.T;
  BEGIN
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN 
      Net.Debug(debug_level, Net.oLevel.INFO,"IpFrag.Tcp.PA ");
    END;

    IF timing THEN Spy.Enter(tcpreassemblytimer); END;
    Reassemble(packet,curr,offset,newpacket);
    IF timing THEN Spy.Exit(tcpreassemblytimer); END;

    IF newpacket # NIL THEN
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN 
        Net.Debug(debug_level, Net.oLevel.DEBUG,"IpFrag.Tcp.PA got newpacket. ");
      END;

      WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(newpacket)^,0,ip_hdr_len),
           ipHeader = VIEW(ipHeaderBuf,NewT),
           udpOffset = ipHeader.hlen*4
       DO
        EVAL Tcp.PacketArrived(newpacket,newpacket,udpOffset);
        Mbuf.m_freem(newpacket);
      END;

    END;
    RETURN FALSE; (* return true if consuming packet *)
  END PacketArrived_TCP; 


PROCEDURE Init (verbose: BOOLEAN) = 
  BEGIN
    udp := Ip.Install(Ip.PacketArrived,
                      Guard_UDP,
                      PacketArrived_UDP);
    tcp := Ip.Install(Ip.PacketArrived,
                      Guard_TCP,
                      PacketArrived_TCP);

    IF timing THEN
      udpreassemblytimer := Spy.Create("ip_reas_udp");
      tcpreassemblytimer := Spy.Create("ip_reas_tcp");
    END;

    fragTable         := NEW(IpFragTbl.Default).init();
    mutex             := NEW(MUTEX);
    timeoutPending    := FALSE;

    IF verbose THEN
      IO.Put("IpFrag module initialized.\n");
    END;
  END Init;

BEGIN
END IpFrag. 

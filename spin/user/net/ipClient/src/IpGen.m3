(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Dec-97  David Becker at the University of Washington
 *	Make PacketSend lookup the route instead of caller.
 *
 * 19-Jun-97  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed the Fragment() procedure to free the mbuf that was just
 *	fragmented.  This bug created a serious mbuf leak.
 *
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Set the ipid correctly when fragmenting packets.
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Conditionally use spy timers.
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new spin shell style.
 *
 * 07-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Rewrote IP fragmentation code.
 *
 * 01-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *      After checksum, no need to do htons order conversion. Already in
 *      the right order.
 *      
 *
 * 21-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *      Rewrote send routine to take UNIX mbufs.  NOTE: The NextIp()
 *      values are a shared resource and should not be allocated from
 *      within this "untrusted" module.
 *      
 *
 *)

  (* THIS MODULE IMPLEMTENT IP FRAGMENTATION BASED ON THIS RFC
     excerpt from RFC 791 explaining how to do IP fragmentation.

     Fragmentation

     Fragmentation of an internet datagram is necessary when it
     originates in a local net that allows a large packet size and must
     traverse a local net that limits packets to a smaller size to reach
     its destination.

     An internet datagram can be marked "don't fragment."  Any internet
     datagram so marked is not to be internet fragmented under any
     circumstances.  If internet datagram marked don't fragment cannot be
     delivered to its destination without fragmenting it, it is to be
     discarded instead.

     Fragmentation, transmission and reassembly across a local network
     which is invisible to the internet protocol module is called
     intranet fragmentation and may be used [6].

     The internet fragmentation and reassembly procedure needs to be able
     to break a datagram into an almost arbitrary number of pieces that
     can be later reassembled.  The receiver of the fragments uses the
     identification field to ensure that fragments of different datagrams
     are not mixed.  The fragment offset field tells the receiver the
     position of a fragment in the original datagram.  The fragment
     offset and length determine the portion of the original datagram
     covered by this fragment.  The more-fragments flag indicates (by
     being reset) the last fragment.  These fields provide sufficient
     information to reassemble datagrams.

     The identification field is used to distinguish the fragments of one
     datagram from those of another.  The originating protocol module of
     an internet datagram sets the identification field to a value that
     must be unique for that source-destination pair and protocol for the
     time the datagram will be active in the internet system.  The
     originating protocol module of a complete datagram sets the
     more-fragments flag to zero and the fragment offset to zero.

     To fragment a long internet datagram, an internet protocol module
     (for example, in a gateway), creates two new internet datagrams and
     copies the contents of the internet header fields from the long
     datagram into both new internet headers.  The data of the long
     datagram is divided into two portions on a 8 octet (64 bit) boundary
     (the second portion might not be an integral multiple of 8 octets,
     but the first must be).  Call the number of 8 octet blocks in the
     first portion NFB (for Number of Fragment Blocks).  The first
     portion of the data is placed in the first new internet datagram,
     and the total length field is set to the length of the first
     datagram.  The more-fragments flag is set to one.  The second
     portion of the data is placed in the second new internet datagram,
     and the total length field is set to the length of the second
     datagram.  The more-fragments flag carries the same value as the
     long datagram.  The fragment offset field of the second new internet
     datagram is set to the value of that field in the long datagram plus
     NFB.

     This procedure can be generalized for an n-way split, rather than
     the two-way split described.

     To assemble the fragments of an internet datagram, an internet
     protocol module (for example at a destination host) combines
     internet datagrams that all have the same value for the four fields:
     identification, source, destination, and protocol.  The combination
     is done by placing the data portion of each fragment in the relative
     position indicated by the fragment offset in that fragment's
     internet header.  The first fragment will have the fragment offset
     zero, and the last fragment will have the more-fragments flag reset
     to zero.

  *)

MODULE IpGen;

IMPORT Net, Word, IO, Fmt, Spy, IpPktFormat, IpRoute, Mbuf, Ctypes,
       SocketRep, SocketAddr, ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ipgen *)
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
  ipid: Ctypes.unsigned_short := 0;
  debug_level : Net.Level := Net.oLevel.NODEBUG;

CONST
  debug = FALSE;
  timing = FALSE;


PROCEDURE NextIp():Ctypes.unsigned_short = 
  BEGIN
    IF ipid + 1 # LAST(Ctypes.unsigned_short) THEN
      ipid := ipid + 1;
    ELSE
      ipid := 0;
    END;
    RETURN ipid;
  END NextIp;


PROCEDURE PacketSend(
  READONLY ip         : IpPktFormat.Header; 
  READONLY data       : Mbuf.T;
           ro         : IpRoute.T) = 
  VAR 
    header_len : Ctypes.unsigned_short := BYTESIZE(IpPktFormat.Header);
    packet : Mbuf.T;
    size : CARDINAL;
    s: SocketAddr.T;

  PROCEDURE NoFragment() = 
    BEGIN
      (* grab an mbuf to stick the IP header in.  We need a new mbuf
         prepend function that checks if there is space in the data mbuf
         that passed down by the client. The main problem is that the
         mbuf data pointer will be changed, allowng clients to
         manipulate the IP address, thus potentially being able to
         violate system integrity.  
      *)

      (* m_prepend sets the length field in the mbuf header.  Total length 
         needs to be computed by the device layer, so that the device driver
         only has to lok in the top level mbuf header to know how much data
         there is in the entire chain.  
      *)

      IF timing THEN Spy.Enter(ipsendtimer); END;

      (* probably want to use no blocking mbuf header allocation function. *)
      packet := data;
      packet := Mbuf.M_PREPEND(packet,header_len,Mbuf.M_WAIT);

      (* set ip address stuff *)

      (* use function to get the open array header out of the header.  I
         am cheating with mbufs by using he m.mh_hdr start & len fields
         as M3 open_elt fields.  This is to avoid allocation just so M3
         can look at stuff.  Mbufs clearly will need to become an
         integral part of our networking code.  I.e., dynlinked with all
         networking code and device drivers.
      *)
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO, "Sending ip packet. ");
      END;

      WITH header_buf = Mbuf.Array(packet),
           header = VIEW(header_buf^, IpPktFormat.T) 
       DO
        (* set the ip header length to the header size / 4*)
        header.hlen := header_len DIV 4; (* we are not setting ip options *)
        (* set the version number *)
        header.vers := 4; (* version compatibility *)
        (* zero out type of service field *)
        header.tos := ip.tos;
        (* set the id field -- this is hard *)
        header.id := Net.htons(NextIp()); (* nextip must come from a trusted server. *)
        (* initialize fragmention information *)
        header.frag_off := ip.frag_off; (* not fragmented by default. *)
        (* set time to live *)
        header.ttl := ip.ttl; (* set max time to life. *)

        header.protocol := ip.protocol;

        (* do routing table look up based on destination address *)
        (* XXX: write some writing code. *)

        (* pick device based on routing information *)
        (* XXX: write some device information code. *)

        (* set the src/dst fields for the ip packet *)
        header.daddr := ip.daddr;
        header.saddr := ro.dst;
        (* fragmentation not required. *)

        (* set total packet length and checksum *)
        header.tot_len := Net.htons(size);

        (* compute checksum for the ip header packet *)
        header.check := 0;
        header.check := Net.checksum(header_buf^,header_len,0);
        (* header.check := Net.htons(header.check); *)
      END;
      IF timing THEN Spy.Exit(ipsendtimer); END;
      EVAL ro.PacketSend(ro.dev, packet, s, NIL);
    END NoFragment;

  PROCEDURE Fragment () = 
    CONST MinFragSize = 8; (* bytes *)
    VAR 
      packet    : Mbuf.T;
      frag      : Mbuf.T;

      mtu       : Word.T := ro.dev.mtu();

      done      : BOOLEAN;

      header_len: Ctypes.unsigned_short;
      flags     : Ctypes.unsigned_short;
      fragoffset: Ctypes.unsigned_short;
      len       : Ctypes.unsigned_short;
      ipid      := Net.htons(NextIp());

    BEGIN
      header_len := BYTESIZE(IpPktFormat.Header);
      fragoffset := 0; (* starting offset into IP packet data*)
      mtu := ro.dev.mtu() - header_len; (* ip data mtu *)
      (* max size of each ip packet *)
      len := Word.And(mtu, Word.Not(MinFragSize-1));
      done := FALSE;

      (* remove the size of the ip header length from the data size *)
      DEC(size,header_len);

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO, "IpGenFragment size = " & Fmt.Int(size) & " ");
      END;

    (* now take an IP packet and chop it into mtu sized pieces *)
    REPEAT 
      IF timing THEN Spy.Enter(ipfragsendtimer); END;

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.DEBUG,"len = " & 
          Fmt.Int(len) & " fragoff = " & Fmt.Int(fragoffset) & "\n");
      END;

      IF fragoffset + len >= size THEN
        len := size - fragoffset;

        flags := Word.And(Word.RightShift(fragoffset,3),IpPktFormat.IP_OFFSET);

        (* create an mbuf for the fragment.  *) 
        frag := Mbuf.m_copym(data,fragoffset,len,Mbuf.M_WAIT);

        (* last time through the loop *)
        done := TRUE;

      ELSE
        (* set more fragment bit *)
        (* frag_off = 3bit flags + 13bit frag offset *)
        flags := Word.And(Word.RightShift(fragoffset,3),IpPktFormat.IP_OFFSET);
        flags := Word.Or(flags,IpPktFormat.IP_MF);

        (* create an mbuf for the fragment  *) 
        frag := Mbuf.m_copym(data,fragoffset,len,Mbuf.M_WAIT);

      END;

      IF frag = NIL THEN IO.Put("IpGenFrag PANIC out of mbufs for fragments.\n") END;
      (* prepend the IP header and increment the *)
      packet := Mbuf.m_prepend(frag,header_len,Mbuf.M_WAIT);
      IF packet = NIL THEN IO.Put("IpGenFrag PANIC out of mbufs for header.\n") END;
      (* copy ip options if any into header *)


      (* set ip address information *)
      WITH header_buf = Mbuf.Array(packet),
           header = VIEW(header_buf^, IpPktFormat.T)
       DO

        (* set the ip header length to the header size / 4*)
        header.hlen := header_len DIV 4; 
        (* set the version number *)
        header.vers := 4; (* version compatibility *)
        (* zero out type of service field *)
        header.tos := ip.tos;
        (* set the id field -- this is hard *)
        header.id := ipid;
        
        (* header.id := Net.htons(NextIp()); *) (* nextip must come from a trusted server. *)
        (* initialize fragmention information *)
        header.frag_off := Net.htons(fragoffset); 
        (* set time to live *)
        header.ttl := ip.ttl; (* set max time to life. *)

        header.protocol := ip.protocol;

        (* do routing table look up based on destination address *)
        (* XXX: write some writing code. *)

        (* pick device based on routing information *)
        (* XXX: write some device information code. *)

        (* set the src/dst fields for the ip packet *)
        header.daddr := ip.daddr;
        header.saddr := ro.dst;
        (* fragmentation not required. *)

        header.frag_off := Net.htons(flags);
        IF debug AND debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.INFO,"fragsize = " & Fmt.Int(header_len + len) & "\n");
        END;

        (* set packet length for fragment and checksum the packet *)
        header.tot_len := Net.htons(header_len + len);

        (* compute checksum for the ip header packet *)
        header.check := 0;
        header.check := Net.checksum(header_buf^,header_len);

        IF timing THEN Spy.Exit(ipfragsendtimer); END;
        EVAL ro.PacketSend(ro.dev, packet, s, NIL);
      END;
      INC(fragoffset,len);
    UNTIL done; (* done fragmenting packet *)

    (* Finished fragmenting and sending out data packet.
       Can now safely free the data mbuf 
     *)
    Mbuf.m_freem(data);
  END Fragment;

  BEGIN
    (* check if fragmentation is required *)
    (* XXX: device dependent MTU.  which device we send over depends 
       on which route we are taking.
    *)
    
    IF ip.tot_len # 0 THEN 
      size := ip.tot_len; (* must be in host order *)
    ELSE
      size := Mbuf.m_length(data) + header_len;
    END;

    IF timing THEN Spy.Enter(routeLookup); END;

    (* go grab a route for the ip address *)
    IF ro = NIL THEN ro := IpRoute.Lookup(ip.daddr); END;
    s.sa_family := SocketRep.AF_INET;
    s.sa_len    := BYTESIZE(SocketAddr.T); (* required by the networking code to be set *)
    WITH ipdaddr = VIEW(s.sa_data,IpPktFormat.AddressArray) DO
      IF IpRoute.Status.Gateway IN ro.status THEN 
        ipdaddr := VIEW(ro.dst,IpPktFormat.AddressArray);
      ELSE
        ipdaddr := VIEW(ip.daddr,IpPktFormat.AddressArray);
      END;
    END;

    IF timing THEN Spy.Exit(routeLookup);  END;

    IF size <= ro.dev.mtu() THEN
      NoFragment();
    ELSE
      (* need to fragment IP packet *)
      Fragment();
    END;
  END PacketSend;

VAR
  routeLookup     : Spy.T;
  ipsendtimer     : Spy.T;
  ipfragsendtimer : Spy.T;

PROCEDURE Init(<* UNUSED *> verbose:BOOLEAN) =
  BEGIN
    IF timing THEN
      routeLookup     := Spy.Create("IpGen.RouteLookup");
      ipsendtimer     := Spy.Create("IpGen.PacketSend");
      ipfragsendtimer := Spy.Create("IpGen.PacketSendFrag");
    END
  END Init;

BEGIN
END IpGen.

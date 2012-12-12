(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

MODULE Ip6Gen;

IMPORT Net, Word, IO, Ip6PktFormat, Mbuf, Ctypes, SocketRep,
       SocketAddr, Ip6Route, Fmt;

IMPORT Glob, Shell, Device, NetDev, NameServer, Ether6Gen;

VAR 
  ipid: Ctypes.unsigned_int := 0;

<* INLINE *>
PROCEDURE OverlayChecksum(
    READONLY ip : Ip6PktFormat.Header;
    paylen      : Ctypes.unsigned_int;
    next_head   : Ctypes.unsigned_char) : Ctypes.unsigned_short =
  
  VAR
    overlay_header : ARRAY [1..40] OF CHAR;

  BEGIN
      
    (* compute overlay header checksum *)
    (* RFC 1883 section 8.1 *)
    
    WITH header = VIEW(overlay_header, Ip6PktFormat.OverlayHeader) DO
      
      header.saddr := ip.saddr;
      header.daddr := ip.daddr;
      header.paylen := Net.htonl(paylen);
      header.zero_next_head := Net.htonl(Word.And(16_000000ff, next_head));
      
    END;

    RETURN Net.checksum(overlay_header);

  END OverlayChecksum;

PROCEDURE NextIp():Ctypes.unsigned_int = 
  BEGIN
    IF ipid + 1 # LAST(Ctypes.unsigned_int) THEN
      ipid := ipid + 1;
    ELSE
      ipid := 0;
    END;
    RETURN ipid;
  END NextIp;


PROCEDURE PacketSend(
  READONLY ip   : Ip6PktFormat.Header; 
  READONLY data : Mbuf.T;
  ro            : Ip6Route.T) = 

  CONST
    header_len : Ctypes.unsigned_short = BYTESIZE(Ip6PktFormat.Header);
  VAR 
    packet : Mbuf.T;
    size   : CARDINAL;
    s      : SocketAddr.T;
    mtu    : INTEGER;

  PROCEDURE NoFragment() = 
    BEGIN
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
           header = VIEW(header_buf^, Ip6PktFormat.T) 
       DO
        (* set the version number *)
        header.vers := 16_6;
        (* set priority *)
        header.prio := ip.prio;
        (* set flow *)
        header.flow := ip.flow;        
        (* set payload length *)
        header.payload := Net.htons(size);
        (* set next header type *)
        header.next_head := ip.next_head;
        (* set hop limit *)
        header.hop_limit := ip.hop_limit;

        (* do routing table look up based on destination address *)
        (* XXX: write some writing code. *)

        (* pick device based on routing information *)
        (* XXX: write some device information code. *)

        (* XXX: this won't work if header.daddr is a multicast address *)
        (* set the src/dst fields for the ip packet *)
        header.daddr := ip.daddr;
        header.saddr := ip.saddr;

        (* fragmentation not required. *)

      END;
      EVAL ro.PacketSend(ro.dev, packet, s, NIL);
    END NoFragment;

  PROCEDURE Fragment () = 
    CONST 
      MinFragSize = 8; (* bytes *)
      fragheader_len = BYTESIZE(Ip6PktFormat.FragmentHeader);


    VAR
      fragHdr     : CARDINAL;
      packet      : Mbuf.T;
      frag        : Mbuf.T;
      done        : BOOLEAN;
      inherit_flg : Word.T;
      inherit_off : Word.T;
      thisipid    : Ctypes.unsigned_int;
      offset      : Ctypes.unsigned_short;
      fragoffset  : Ctypes.unsigned_short;
      len         : Ctypes.unsigned_short;
      next_head   : Ctypes.unsigned_char;
    BEGIN

      DEC(mtu,fragheader_len); (* subtract fragment header*)
      (* max size of each ip packet *)
      len := Word.And(mtu, Word.Not(MinFragSize-1));
      done := FALSE;


      (* check if the application layer already added a fragement header *)
      IF ip.next_head # Ip6PktFormat.IPPROTO_EXT_FRAG THEN
        (* its not a fragment header, so use the next_head from ip header
           and compute some ip id. *)
        next_head   := ip.next_head;
        thisipid    := Net.htonl(NextIp()); (* generate an ip id *)
        (* starting offset into IP packet data*)
        fragoffset  := 0; 
        fragHdr     := 0;
        inherit_off := 0;
        inherit_flg := 16_0;
      ELSE
        DEC(len,fragheader_len);
        WITH header_buf = Mbuf.Array(data),
             header = VIEW(header_buf^,Ip6PktFormat.FragmentHeader)
         DO
          thisipid    := header.id;
          next_head   := header.next_head;
          offset      := Net.nstoh(header.offset);
          inherit_off := Word.And(offset,16_fff8);
          inherit_flg := Word.And(offset,16_0007); (* inherit flags *)
        END;
        (* use id and next_head from fragment header *)
        fragoffset := fragheader_len; (* starting offset into IP packet data*)
        fragHdr    := fragheader_len;
      END;

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO, "IpGenFragment size = " & Fmt.Int(size) & " ");
      END;

      (* now take an IP packet and chop it into mtu sized pieces *)
      REPEAT 

        IF debug AND debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.DEBUG,"len = " & 
            Fmt.Int(len) & " fragoff = " & Fmt.Int(fragoffset) & "\n");
        END;

        offset := Word.Or(inherit_off+fragoffset-fragHdr,inherit_flg);
        IF fragoffset + len >= size THEN
          (* last time through the loop *)
          len := size - fragoffset;
          done := TRUE;
        ELSE
          (* set more fragment bit *)
          offset := Word.Or(offset,16_1);
        END;

        (* create an mbuf for the fragment  *) 
        frag := Mbuf.m_copym(data,fragoffset,len,Mbuf.M_WAIT);

        IF frag = NIL THEN IO.Put("IpGenFrag PANIC out of mbufs for fragments.\n") END;
        (* prepend the IP header & fragment header and increment *)
        packet := Mbuf.m_prepend(frag,header_len + fragheader_len,Mbuf.M_WAIT);
        IF packet = NIL THEN IO.Put("IpGenFrag PANIC out of mbufs for header.\n") END;
        (* copy ip options if any into header *)

        (* set ip address information *)
        WITH header_buf = Mbuf.Array(packet),
           header = VIEW(header_buf^, Ip6PktFormat.T),
           fragheader = VIEW(SUBARRAY(header_buf^,header_len,fragheader_len),Ip6PktFormat.FragmentHeader)
         DO
          (* big copy --- hopefully compiler uses bcopy (mef) *)
          header := ip;
          (* set the version number *)
          header.vers := 16_6;
          (* set payload length *)
          header.payload := Net.htons(len+fragheader_len);
          (* set next header type *)
          header.next_head := Ip6PktFormat.IPPROTO_EXT_FRAG;

          (* fragment header stuff *)


          (* set next header type *)
          fragheader.next_head := next_head;
          (* unused *)
          fragheader.reserved := 0;
          (* 13 bit offset, 2 bit reserved, 1 bit more frag field *)
          fragheader.offset    := Net.htons(offset);
          (* the id of this fragment *)
          fragheader.id        := thisipid;
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

    IF ip.payload # 0 THEN 
      size := ip.payload; (* must be in host order *)
    ELSE
      size := Mbuf.m_length(data);
    END;

    s.sa_family := SocketRep.AF_INET6;
    s.sa_len    := BYTESIZE(SocketAddr.T); (* required by the networking code to be set *)

    (* XXX insert route lookup here and determine whether we need to
       fragment packet *)
    IF ro = NIL THEN 
      IO.Put("Ip6Gen route = NIL; using default route.\n");
      ro := defaultRoute;
    END;

    mtu := ro.dev.mtu() - header_len;

    IF size <= mtu THEN
      NoFragment();
    ELSE
      Fragment();
    END;
  END PacketSend;


VAR 
  dev          : NetDev.T;
  defaultRoute : Ip6Route.T;

PROCEDURE Uninit(verbose:BOOLEAN) = 
  BEGIN
    defaultRoute := NIL;
    dev := NIL;
    IF verbose THEN IO.Put("Ip6Gen unloaded.\n"); END;
  END Uninit;

PROCEDURE Init(verbose:BOOLEAN) =
  VAR
    devname : TEXT;
  BEGIN
    devname := Glob.GetVariable(Shell.Vars(), "ETHERDEV");
    (*ifp := IfUtil.GetIf(devname, 0);*)
    TRY
      (* Lookup() marked OBSOLETE, but IpRoute.m3 uses it as well *)
      dev := Device.Lookup(devname);
    EXCEPT
    | NameServer.Error =>
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO, "Ip6Gen: Device.Lookup() failed");
      END;      
      RETURN;
    END;
    defaultRoute := NEW(Ip6Route.T);
    defaultRoute.dev := dev;    
    defaultRoute.PacketSend := Ether6Gen.PacketSend;
    IF verbose THEN IO.Put("Ip6Gen initialized.\n"); END;
  END Init;

BEGIN
END Ip6Gen.

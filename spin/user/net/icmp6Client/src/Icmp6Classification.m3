(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

MODULE Icmp6Classification;

IMPORT IO, Fmt;
IMPORT Mbuf;
IMPORT Ip6Gen;
IMPORT Icmp6Gen;
IMPORT Icmp6PktFormat;
IMPORT UdpPktFormat; <* NOWARN *>
IMPORT Ip6PktFormat;
IMPORT Icmp6;
IMPORT EtherPktFormat;
IMPORT SocketRep; <* NOWARN *>
IMPORT Ctypes; <* NOWARN *>
IMPORT Net;
IMPORT Glob, Shell;
IMPORT Device, EtherDev, NetDev, NameServer;

VAR etherdev : EtherDev.T;

VAR
  echo: REFANY;
  neighbor_solict: REFANY;

CONST
  ip_hdr_len = BYTESIZE(Ip6PktFormat.Header);
  icmp_hdr_len = BYTESIZE(Icmp6PktFormat.Header);
  ip_icmp_hdr_len = ip_hdr_len + icmp_hdr_len;

CONST
  debug = TRUE;


FUNCTIONAL
PROCEDURE Guard_ECHO(
    <* UNUSED *> packet: Mbuf.T;
    curr: Mbuf.T; 
    offset: CARDINAL):
  BOOLEAN =
  BEGIN
    WITH icmpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,icmp_hdr_len),
         icmpHeader = VIEW(icmpHeaderBuf,T) 
     DO
      RETURN icmpHeader.type = Icmp6PktFormat.ECHO;
    END;
  END Guard_ECHO; 

PROCEDURE PacketArrived_ECHO(
    packet: Mbuf.T;
    curr: Mbuf.T; 
    offset: CARDINAL):
  BOOLEAN =
  VAR
    ip		 : Ip6PktFormat.Header;
    icmp	 : Icmp6PktFormat.Header;
    data	 : Mbuf.T;
    len          : CARDINAL;
    
    paylen       : Ctypes.unsigned_int;
    overlay_csum : Ctypes.unsigned_short;
    foo : Ctypes.unsigned_short;

  BEGIN

    (* #ifdef debug_level != NODEBUG *)
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"Icmp6NeighborSolictRequest ");
    END;

    WITH origpkt = Mbuf.Array(packet) ,
         ip_header = VIEW(origpkt^,Ip6PktFormat.T) DO

      (* verify checksum *)

      ip.saddr := ip_header.saddr;
      ip.daddr := ip_header.daddr;
      
      (* this doesn't account for extension hdrs *)

      (* OverlayChecksum() converts it right back to net order.  but
         we have to do this since it seems like htonl() doesn't nop if
         paylen is already in net order *)
      paylen   := Net.nltoh(ip_header.payload);   
      
      overlay_csum := Ip6Gen.OverlayChecksum(ip, paylen, Ip6PktFormat.IPPROTO_ICMP6);
      
      (* compute the length of the icmp packet *)
      len := Mbuf.m_length(packet);
      DEC(len,ip_hdr_len);
      
IF FALSE THEN
      IO.Put("paylen: " & Fmt.Unsigned(paylen) & " len: " & Fmt.Unsigned(len) & " overlay_csum: " & Fmt.Unsigned(overlay_csum) & "\n");
END;

      WITH icmpBuf = SUBARRAY(Mbuf.Array(curr)^,offset,len) DO
        
        foo := Net.checksum(icmpBuf, len, overlay_csum);

        IF foo # 0 THEN
          IO.Put("echo cksum failed: " & Fmt.Unsigned(foo) & "\n");
          RETURN FALSE;    (* checksum failed *)
        END;

      END;

      (* set ip info *)
      
      (* icmp packets get high priority *)
      ip.prio      := 16_f;
      (* no standard use for flow label yet *)
      ip.flow      := 16_000000;
      (* ip layer sets payload len *)
      (* set Next Header to ICMP type *)
      ip.next_head := Ip6PktFormat.IPPROTO_ICMP6;
      (* set hop limit to max life *)
      ip.hop_limit := 16_ff;
      
      (* swap addresses *)
      ip.saddr     := ip_header.daddr;
      ip.daddr     := ip_header.saddr;
      
    END;

    (* set icmp info *)

    (* zero icmp code *)
    icmp.code := 16_0;
    (* send back as echo reply icmp packet *)
    icmp.type  := Icmp6PktFormat.ECHOREPLY;

    (* checksum gets computed during PacketSend() *)

    (* compute the length of the icmp packet data *)
    len := Mbuf.m_length(packet);
    DEC(len,ip_icmp_hdr_len);

    (* copy data into its own mbuf chain *)
    data := Mbuf.m_copym(packet,ip_icmp_hdr_len,len,Mbuf.M_WAIT);

    (* no need to change icmp data, we just reply with the data received *)

    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"Icmp6EchoRequest sending icmp packet\n");
    END;

    (* send it over IP -- needs to become a event raise rather than a proc call *)
    Icmp6Gen.PacketSend(ip,icmp,data);

    RETURN FALSE; 
  END PacketArrived_ECHO;

FUNCTIONAL
PROCEDURE Guard_NEIGHBOR_SOLICT(
    <* UNUSED *> packet: Mbuf.T;
    curr: Mbuf.T; 
    offset: CARDINAL):
  BOOLEAN =
  BEGIN
    WITH icmpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,icmp_hdr_len),
         icmpHeader = VIEW(icmpHeaderBuf,T) 
     DO

      RETURN icmpHeader.type = Icmp6PktFormat.NEIGHBOR_SOLICT;

    END;
  END Guard_NEIGHBOR_SOLICT; 

PROCEDURE PacketArrived_NEIGHBOR_SOLICT(
    packet: Mbuf.T;
    <*UNUSED*> curr: Mbuf.T; 
    <*UNUSED*> offset: CARDINAL):
  BOOLEAN =
  VAR
    ip           : Ip6PktFormat.T;
    icmp         : Icmp6PktFormat.T;
    data         : Mbuf.T;
    len          : CARDINAL;
    paylen       : Ctypes.unsigned_int;
    overlay_csum : Ctypes.unsigned_short;
    foo          : Ctypes.unsigned_short;
    targ_addr    : EtherPktFormat.Address;

  BEGIN

    (* #ifdef debug_level != NODEBUG *)
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"Icmp6NeighborSolictRequest ");
    END;

    WITH origpkt = Mbuf.Array(packet) ,
         ip_header = VIEW(origpkt^,Ip6PktFormat.T) DO

      (* verify checksum *)

      ip.saddr := ip_header.saddr;
      ip.daddr := ip_header.daddr;

      (* this doesn't account for extension hdrs *)
      (* OverlayChecksum() converts it right back to net order.  but
         we have to do this since it seems like htonl() doesn't nop if
         paylen is already in net order *)

      paylen   := Net.nltoh(ip_header.payload);   

      overlay_csum := Ip6Gen.OverlayChecksum(ip, paylen, Ip6PktFormat.IPPROTO_ICMP6);

      (* compute the length of the icmp packet *)
      len := Mbuf.m_length(packet);
      DEC(len,ip_hdr_len);

IF FALSE THEN
      IO.Put("paylen: " & Fmt.Unsigned(paylen) & " len: " & Fmt.Unsigned(len) & " overlay_csum: " & Fmt.Unsigned(overlay_csum) & "\n");
END;

      WITH icmpBuf = SUBARRAY(Mbuf.Array(packet)^,ip_hdr_len,len) DO

        foo := Net.checksum(icmpBuf, len, overlay_csum);

IF FALSE THEN
        IO.Put("nb cksum: " & Fmt.Unsigned(foo) & "\n");
END;

        IF foo # 0 THEN
          IO.Put("nb cksum failed: " & Fmt.Unsigned(foo) & "\n");
          RETURN FALSE;    (* checksum failed *)
        END;

      END;

      (* set ip info *)

      (* icmp packets get high priority *)
      ip.prio      := 16_f;
      (* no standard use for flow label yet *)
      ip.flow      := 16_000000;
      (* ip layer sets payload len *)
      (* set Next Header to ICMP type *)
      ip.next_head := Ip6PktFormat.IPPROTO_ICMP6;
      (* set hop limit to max life *)
      ip.hop_limit := 16_ff;
      
      (* compute the length of the icmp packet data *)
      len := Mbuf.m_length(packet);
      DEC(len,ip_icmp_hdr_len);
      
      (* set ip addresses *)
      WITH ndBuf = SUBARRAY(Mbuf.Array(packet)^,ip_icmp_hdr_len,len),
           nsolict = VIEW(ndBuf,Icmp6PktFormat.NeighborSolicitation) DO
      
        (* set ip src addr to neighbor solict target addr *)
        ip.saddr := nsolict.addr;
        ip.daddr := ip_header.saddr;

      END;

    END;

    (* set icmp info *)

    (* zero icmp code *)
    icmp.code := 16_0;
    (* send back as neighbor advertisement icmp packet *)
    icmp.type  := Icmp6PktFormat.NEIGHBOR_ADV;

    (* checksum gets computed during PacketSend() *)

    (* copy data into its own mbuf chain *)
    data := Mbuf.m_copym(packet,ip_icmp_hdr_len,len,Mbuf.M_WAIT);

    (* grab local phys addr for later *)
    etherdev.etherAddr(targ_addr);

    WITH newdat = Mbuf.Array(data) ,
         nadv_data = VIEW(newdat^,Icmp6PktFormat.NeighborAdvertisement) DO
      
      (* set solicited flag.  rest of field zeroed out *)
      nadv_data.reserved := Net.htonl(16_40000000);

      (* no need to update target addr.  just use the solicitation target addr. *)

      (* fill out target link layer addr option *)

      nadv_data.options.type := 16_02;
      nadv_data.options.len  := 16_01;  (* XXX: bad!  ethernet specific *)
      nadv_data.options.addr := targ_addr;

    END;
   
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"Icmp6EchoRequest sending icmp packet\n");
    END;

    (* send it over IP -- needs to become a event raise rather than a proc call *)
    Icmp6Gen.PacketSend(ip,icmp,data);

    RETURN FALSE; 
  END PacketArrived_NEIGHBOR_SOLICT;

PROCEDURE Init(verbose:BOOLEAN) =
  VAR
    devname : TEXT; 
    dev : NetDev.T;
  BEGIN
    (* install echo handlers *)
    echo := Icmp6.Install(Icmp6.PacketArrived,
                         Guard_ECHO,
                         PacketArrived_ECHO);
    neighbor_solict := Icmp6.Install(Icmp6.PacketArrived,
                         Guard_NEIGHBOR_SOLICT,
                         PacketArrived_NEIGHBOR_SOLICT);
    
    devname := Glob.GetVariable(Shell.Vars(), "ETHERDEV");

    TRY
      (* Lookup() marked OBSOLETE, but IpRoute.m3 uses it as well *)
      dev := Device.Lookup(devname);
    EXCEPT
    | NameServer.Error =>
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO, "Device.Lookup() failed");
      END;      
      RETURN;
    END;
    
    TYPECASE dev OF
    | NULL =>
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO, "invalid dev!");
      END;      
      RETURN;
    | EtherDev.T =>
      etherdev := dev;
    | NetDev.T =>
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO, "dev isn't an etherdev!");
      END;      
      RETURN;
    END;

    IF verbose THEN IO.Put("Icmp6Classification module initialized.\n"); END;
  END Init;

PROCEDURE Uninit(verbose:BOOLEAN) = 
  BEGIN
    etherdev := NIL;
    Icmp6.Uninstall(echo); 
    echo := NIL;
    Icmp6.Uninstall(neighbor_solict);
    neighbor_solict:= NIL;
    IF verbose THEN IO.Put("Icmp6Classification unloaded.\n"); END;
  END Uninit;

BEGIN
END Icmp6Classification.

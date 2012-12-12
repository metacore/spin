(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 17-Nov-97  Marc Fiuczynski (mef) at the University of Washington
 *	Based on Vinh Lam's DHT sources from Spring'97.
 *)

MODULE Dht EXPORTS Dht, DhtMapping;

IMPORT Ip, Ip6, Icmp6PktFormat, IcmpPktFormat, Ip6Gen, IpGen,
       Ip6PktFormat, IpPktFormat, Ip6Route;
       (*plexus*)
IMPORT Mbuf, Net; (*urt*)
IMPORT Word, Ctypes; (*m3core*)
IMPORT ParseParams, IO, Fmt; (*libm3*)
IMPORT Dispatcher, NetText; (*SPIN*)
IMPORT Commands; (*shell*)

IMPORT Device, NetDev, Glob, Shell, NameServer, Ether6Gen;


IMPORT DhtIp;
IMPORT DhtIP4Tbl, DhtIP6Tbl;

TYPE proto = {ip, ip6};
VAR
  shell       : REFANY;
  bindings    : ARRAY [FIRST(proto) .. LAST(proto)] OF REFANY;
  dhtAddr     : Ip6PktFormat.Address;

VAR
  debug_level : Net.Level := Net.oLevel.NODEBUG;

CONST
  ip_hdr_len      = BYTESIZE(IpPktFormat.Header);
  ip6_hdr_len     = BYTESIZE(Ip6PktFormat.T);
  icmp_hdr_len    = BYTESIZE(IcmpPktFormat.T);
  icmp6_hdr_len   = BYTESIZE(Icmp6PktFormat.T);
  frag_hdr_len    = BYTESIZE(Ip6PktFormat.FragmentHeader);

PROCEDURE FmtIp(ip:Ip6PktFormat.Address): TEXT =
  BEGIN
    WITH ipChunks = VIEW(ip, ARRAY[0..7] OF Ctypes.unsigned_short) DO 
      RETURN  Fmt.Unsigned(Net.htons(ipChunks[0]))& ":"&
	      Fmt.Unsigned(Net.htons(ipChunks[1]))& ":"&
	      Fmt.Unsigned(Net.htons(ipChunks[2]))& ":"&
	      Fmt.Unsigned(Net.htons(ipChunks[3]))& ":"&
	      Fmt.Unsigned(Net.htons(ipChunks[4]))& ":"&
	      Fmt.Unsigned(Net.htons(ipChunks[5]))& ":"&
	      Fmt.Unsigned(Net.htons(ipChunks[6]))& ":"&
	      Fmt.Unsigned(Net.htons(ipChunks[7]));
    END;
  END FmtIp;


(****************************************************************)

CONST CommandName = "dht";
      CommandHelp = "-- debug # | uninstall [-v]";
PROCEDURE Run(
    closure: REFANY;
    pp: ParseParams.T): BOOLEAN =
  VAR
    verbose : BOOLEAN;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* etherdefault *)
      IF pp.testNext("debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("uninstall") THEN
        verbose := pp.testNext("-v");
        Uninit(verbose);
      ELSIF pp.testNext("prefix") THEN
        VAR
          ip4 : IpPktFormat.Address;
        BEGIN
          ip4 := NetText.TextToIp(pp.getNext());
          dhtAddr := Ip6PktFormat.Address{Net.htonl(16_deadbeef),
                                          ip4,0,0};
        END;
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
      ParseParams.Error => 
      Commands.ParseError(closure);
    END;
   RETURN TRUE;
 END Run;

(****************************************************************)
(* FUNCTIONAL *)
PROCEDURE Guard_IP6(
    <*UNUSED*>closure : REFANY;
    <*UNUSED*>packet  : Mbuf.T;
    curr              : Mbuf.T; 
    offset            : CARDINAL):BOOLEAN = 
  BEGIN
    WITH ipData = SUBARRAY(Mbuf.Array(curr)^,offset,ip6_hdr_len),
         ip = VIEW(ipData,Ip6PktFormat.T)
     DO
      WITH forward = Word.And(ip.daddr[FIRST(ip.daddr)], 16_FFFF) DO
        (* XXX need to take all but ff02::1: and own fe80 addresses *)
IF FALSE THEN
IO.Put("Guard_IP6: ");
IO.PutInt(forward);
IO.Put("\n");
END;
        IF forward = 16_02FF OR forward = 16_80FE THEN
IF FALSE THEN
IO.Put("DHT IP6 ignoring RA and self packet.\n");
END;
          RETURN FALSE;
        END;
      END;
      RETURN TRUE;
    END;
  END Guard_IP6;

PROCEDURE PacketArrived_IP6(
    <*UNUSED*>
    closure : REFANY;
    packet  : Mbuf.T;
    curr    : Mbuf.T; 
    offset  : CARDINAL):BOOLEAN = 

  VAR
    new_iph4 : IpPktFormat.T;
    new_pkt4 : Mbuf.T;

  BEGIN
    WITH ipData = SUBARRAY(Mbuf.Array(curr)^,offset,ip6_hdr_len),
         ip = VIEW(ipData,Ip6PktFormat.T)
     DO

      IF FALSE THEN
        IO.Put("DHT IP6 packet (src:");
        IO.Put(FmtIp(ip.saddr));
        IO.Put(" dst:");
        IO.Put(FmtIp(ip.daddr));
        IO.Put(" proto:");
      END;

      new_pkt4 := DhtIp.IP6toIP4(curr,offset,new_iph4,FALSE);
      IF new_pkt4 # NIL THEN
        (* XXX cannot use the 1500 constant, use link/path mtu *)
        IF Mbuf.m_length(new_pkt4)+ip_hdr_len > 1500 THEN
          (* XXX should never get here on the 6to4 conversion (mef) *)
          Dht4PacketTooBig(ip, packet);
        ELSE
          IpGen.PacketSend(new_iph4, new_pkt4);
        END;
      ELSE
        IO.Put("Dht.PacketArrived_IP6 packet not translated.\n");
      END;
    END;
    RETURN FALSE;
  END PacketArrived_IP6;

FUNCTIONAL
PROCEDURE Guard_IP4(
    closure          : REFANY;
    <*UNUSED*>packet : Mbuf.T;
    curr             : Mbuf.T; 
    offset           : CARDINAL):BOOLEAN = 
  BEGIN
    WITH ipData = SUBARRAY(Mbuf.Array(curr)^,offset,ip_hdr_len),
         ip = VIEW(ipData,IpPktFormat.T),
         four2six = NARROW(closure, T4)
     DO
      RETURN Word.And(ip.daddr, four2six.mask) = four2six.addr;
    END;
  END Guard_IP4;

PROCEDURE PacketArrived_IP4(
    <*UNUSED*>
    closure : REFANY;
    <*UNUSED*>
    packet  : Mbuf.T;
    curr    : Mbuf.T; 
    offset  : CARDINAL):BOOLEAN =
  VAR
    new_iph6 : Ip6PktFormat.T;
    new_pkt6 : Mbuf.T;
  BEGIN
    WITH ipData = SUBARRAY(Mbuf.Array(curr)^,offset,ip_hdr_len),
         ip = VIEW(ipData,IpPktFormat.T)
     DO
      IF Word.And(Net.htonl(ip.daddr),16_ff) = 16_ff THEN
        IF FALSE THEN
          IO.Put("DHT IP4 ignoring broadcast packet.\n");
        END;
        RETURN FALSE;
      END;

      IF FALSE THEN
        IO.Put("DHT IP4 packet (src:");
        IO.Put(NetText.FmtIp(ip.saddr));
        IO.Put(" dst:");
        IO.Put(NetText.FmtIp(ip.daddr));
        IO.Put(" proto:");
      END;

      new_pkt6 := DhtIp.IP4toIP6(curr,offset,new_iph6,FALSE);
      IF new_pkt6 # NIL THEN
        (* XXX cannot use the 1500 constant, use link/path mtu *)
        Ip6Gen.PacketSend(new_iph6, new_pkt6, route);
      END;
    END;
    RETURN FALSE;
  END PacketArrived_IP4;


<*UNUSED*>
PROCEDURE Dht6PacketTooBig(
        ip_hdr : IpPktFormat.T; 
        packet : Mbuf.T) =

  VAR
    len : CARDINAL;
    err_ip_hdr : IpPktFormat.T;
    icmp_packet : Mbuf.T;
    
  BEGIN

    (* only include ip hdr + 64 bits of error invoking packet *)
    len := Mbuf.m_length(packet);      
    IF len > ip_hdr_len+8 THEN
      len := ip_hdr_len+8
    END;
      
    icmp_packet := Mbuf.m_copym(packet,0,len,Mbuf.M_WAIT);
    icmp_packet := Mbuf.M_PREPEND(icmp_packet,icmp_hdr_len+4,Mbuf.M_WAIT);
    WITH icmpData = SUBARRAY(Mbuf.Array(icmp_packet)^,0,icmp_hdr_len),
         icmpHdr = VIEW(icmpData,IcmpPktFormat.T)
      DO
       icmpHdr.type := IcmpPktFormat.DEST_UNREACH;
       icmpHdr.code := IcmpPktFormat.FRAG_NEEDED;
       
       WITH errData = SUBARRAY(Mbuf.Array(icmp_packet)^,icmp_hdr_len,4),
         errHdr = VIEW(errData,IcmpPktFormat.DestinationUnreachable)
        DO
         (* XXX how do we find out MTU? *)
         errHdr.unused := Word.And(Net.htonl(1500-(ip6_hdr_len-ip_hdr_len)-frag_hdr_len),16_0000ffff);
       END;

       icmpHdr.csum := 0;
       icmpHdr.csum := Mbuf.Checksum(icmp_packet, 0);
       
     END;
    
    err_ip_hdr.tos := 0;
    err_ip_hdr.id := 0;
    err_ip_hdr.frag_off := 0;
    err_ip_hdr.ttl := 16_ff;
    err_ip_hdr.protocol := IpPktFormat.IPPROTO_ICMP;
    err_ip_hdr.saddr := ip_hdr.daddr;
    err_ip_hdr.daddr := ip_hdr.saddr;

    IpGen.PacketSend(err_ip_hdr, icmp_packet);
    
  END Dht6PacketTooBig;


PROCEDURE Dht4PacketTooBig(
  READONLY
  ip6_hdr : Ip6PktFormat.T; 
  packet  : Mbuf.T) =

  VAR
    len : CARDINAL;
    err_ip6_hdr : Ip6PktFormat.T;
    icmp_packet : Mbuf.T;
    
  BEGIN
    
    (* only include as much as can fit in 576 bytes *)
    (* XXX this doesn't account for extension headers! *)
    len := Mbuf.m_length(packet);            
    IF (len+ip6_hdr_len+icmp6_hdr_len+4) > 576 THEN
      len := 576-ip6_hdr_len-icmp6_hdr_len-4;
    END;

    icmp_packet := Mbuf.m_copym(packet,0,len,Mbuf.M_WAIT);
    icmp_packet := Mbuf.M_PREPEND(icmp_packet,icmp6_hdr_len+4,Mbuf.M_WAIT);
    WITH icmpData = SUBARRAY(Mbuf.Array(icmp_packet)^,0,icmp6_hdr_len),
         icmpHdr = VIEW(icmpData,Icmp6PktFormat.T)
      DO
       icmpHdr.type := Icmp6PktFormat.PACKET_TOO_BIG;
       icmpHdr.code := 0;
       
       WITH errData = SUBARRAY(Mbuf.Array(icmp_packet)^,icmp6_hdr_len,4),
         errHdr = VIEW(errData,Icmp6PktFormat.PacketTooBig)
        DO
         (* XXX how do we find out MTU? *)
         errHdr.MTU := Net.htonl(1500);
       END;

       icmpHdr.csum := 0;
       icmpHdr.csum := Mbuf.Checksum(icmp_packet, Ip6Gen.OverlayChecksum(ip6_hdr, Mbuf.m_length(icmp_packet), Ip6PktFormat.IPPROTO_ICMP6));
       
     END;
    
    err_ip6_hdr.flow := 0;
    err_ip6_hdr.next_head := Ip6PktFormat.IPPROTO_ICMP6;
    err_ip6_hdr.hop_limit := 16_ff;
    err_ip6_hdr.saddr := ip6_hdr.daddr;
    err_ip6_hdr.daddr := ip6_hdr.saddr;

    Ip6Gen.PacketSend(err_ip6_hdr, icmp_packet, route);
    
  END Dht4PacketTooBig;

PROCEDURE IP4toIP6Checksum(
  READONLY ip4 : IpPktFormat.T; (* fields in network order *)
  READONLY ip6 : Ip6PktFormat.T;  (* fields in host order *)
  headcsum     : Ctypes.unsigned_short) : Ctypes.unsigned_short = 
  VAR
    csum  : Word.T;
    csum4 : Word.T;
  BEGIN
    csum := headcsum;
    (* csum := csum + checksum(ip6.{saddr,daddr}) *)
    csum := Net.checksum(VIEW(ip6.saddr,Ip6PktFormat.AddressArray),0,
                         Net.checksum(VIEW(ip6.daddr,Ip6PktFormat.AddressArray),0,csum));

    (* csum4 := checksum(ip4.{saddr,daddr}) *)
    csum4 := 
        Net.checksum(VIEW(ip4.saddr,IpPktFormat.AddressArray),0,
                     Net.checksum(VIEW(ip4.daddr,IpPktFormat.AddressArray),0));

    (* csum := csum + (-csum4) *)
    csum := Word.Plus(csum,Word.And(Word.Not(csum4),16_FFFF));

    (* fold 32 bits -> 16 bit *)
    csum := Word.Plus(Word.And(Word.RightShift(csum,16),16_FFFF),Word.And(csum,16_FFFF));
    (* do it again in case of overflow *)
    csum := Word.Plus(Word.And(Word.RightShift(csum,16),16_FFFF),Word.And(csum,16_FFFF));
    (* do it again in case of overflow *)
    csum := Word.Plus(Word.And(Word.RightShift(csum,16),16_FFFF),Word.And(csum,16_FFFF));
    RETURN csum;
  END IP4toIP6Checksum;

PROCEDURE IP6toIP4Checksum(
  READONLY ip6 : Ip6PktFormat.T;  (* fields in host order *)
  READONLY ip4 : IpPktFormat.T; (* fields in network order *)
  headcsum     : Ctypes.unsigned_short) : Ctypes.unsigned_short = 
  VAR
    csum  : Word.T;
    csum6 : Word.T;
  BEGIN
    csum := headcsum;
    (* csum := csum + checksum(ip4.{saddr,daddr}) *)
    csum := Net.checksum(VIEW(ip4.saddr,IpPktFormat.AddressArray),0,
                         Net.checksum(VIEW(ip4.daddr,IpPktFormat.AddressArray),0,csum));

    (* csum6 := checksum(ip6.{saddr,daddr}) *)
    csum6 := 
        Net.checksum(VIEW(ip6.saddr,Ip6PktFormat.AddressArray),0,
                     Net.checksum(VIEW(ip6.daddr,Ip6PktFormat.AddressArray),0));

    (* csum := csum + (-csum6) *)
    csum := Word.Plus(csum,Word.And(Word.Not(csum6),16_FFFF));

    (* fold 32 bits -> 16 bit *)
    csum := Word.Plus(Word.And(Word.RightShift(csum,16),16_FFFF),Word.And(csum,16_FFFF));
    (* do it again in case of overflow *)
    csum := Word.Plus(Word.And(Word.RightShift(csum,16),16_FFFF),Word.And(csum,16_FFFF));
    (* do it again in case of overflow *)
    csum := Word.Plus(Word.And(Word.RightShift(csum,16),16_FFFF),Word.And(csum,16_FFFF));
    RETURN csum;
  END IP6toIP4Checksum;

(****************************************************************)

TYPE PacketArrivedEvent = 
      PROCEDURE (
          packet, curr: Mbuf.T; 
          offset: CARDINAL):BOOLEAN;
TYPE PacketArrivedEventWithClosure = 
      PROCEDURE (
          closure: REFANY;
          packet, curr: Mbuf.T; 
          offset: CARDINAL):BOOLEAN;

PROCEDURE ExceptionPrint(ec:Dispatcher.ErrorCode) = 
  BEGIN
    CASE ec OF
    | Dispatcher.ErrorCode.InvalidProcedure =>
      IO.Put("Trusted invalid procedure installed.\n");
    ELSE
      IO.Put("Trusted dispatcher install error.\n");
    END;
  END ExceptionPrint;

PROCEDURE InstallWithClosure (
    event          : PacketArrivedEvent; 
    whenClause     : PacketArrivedEventWithClosure; 
    handler        : PacketArrivedEventWithClosure;
    guardClosure   : REFANY := NIL; 
    handlerClosure : REFANY := NIL): Dispatcher.Binding =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    TRY
      TRY
        binding := Dispatcher.InstallHandler(event,
                                             whenClause, handler,
                                             guardClosure,handlerClosure,
                                             Dispatcher.Options{Dispatcher.Opt.First,Dispatcher.Opt.Cancel});
      EXCEPT
      | Dispatcher.Error(ec) => ExceptionPrint(ec);
      END;
    FINALLY
      RETURN binding;
    END;
  END InstallWithClosure; 

PROCEDURE CreateIP6NODE4addr(
    ip4node4: IpPktFormat.Address;
    VAR ip6node4 : Ip6PktFormat.Address) =
  BEGIN
      ip6node4 := dhtAddr;
      ip6node4[LAST(ip6node4)] := ip4node4;
      EVAL ip4node4TOip6node4.put(ip4node4,ip6node4);
      EVAL ip6node4TOip4node4.put(ip6node4,ip4node4);
  END CreateIP6NODE4addr; 

TYPE machine = {loom09, loom10, loom11, weft};
TYPE Map = RECORD
  ip6: Ip6PktFormat.Address;
  ip4: IpPktFormat.Address;
END;

VAR address : ARRAY [FIRST(machine) .. LAST(machine)] OF Map;

(* XXX {devname,dev} hack to setup the ethernet device.  Need to do
   some sort of route lookup based on v6 packets to determine the
   device.  This will come later. (mef) *)

VAR dev   : NetDev.T;
    route : Ip6Route.T;

PROCEDURE Init(verbose:BOOLEAN) = 
  VAR 
    IP4toIP6 : T4;
    ip4node6 : IpPktFormat.Address := 16_0a000001;
    devname : TEXT;
  BEGIN
    devname := Glob.GetVariable(Shell.Vars(), "ETHERDEV");
    (*ifp := IfUtil.GetIf(devname, 0);*)
    TRY
      (* Lookup() marked OBSOLETE, but IpRoute.m3 uses it as well *)
      dev := Device.Lookup(devname); <*NOWARN*>
    EXCEPT
    | NameServer.Error =>
      RETURN;
    END;
    route := NEW(Ip6Route.T);
    route.dev := dev;
    route.PacketSend := Ether6Gen.PacketSend;

    ip4node4TOip6node4 := NEW(DhtIP4Tbl.Default).init();
    ip4node6TOip6node6 := NEW(DhtIP4Tbl.Default).init();
    ip6node4TOip4node4 := NEW(DhtIP6Tbl.Default).init();
    ip6node6TOip4node6 := NEW(DhtIP6Tbl.Default).init();

    address[machine.loom09] := Map
    {Ip6PktFormat.Address{Net.htonl(16_5f02ad00),
                          Net.htonl(16_805f0200),
                          Net.htonl(16_00000060),
                          Net.htonl(16_971c0bfd)},
     Net.htonl(16_805f029d)};

    address[machine.loom10] := Map
    {Ip6PktFormat.Address{Net.htonl(16_5f02ad00),
                          Net.htonl(16_805f0200),
                          Net.htonl(16_00000060),
                          Net.htonl(16_971bfea2)},
     Net.htonl(16_805f029e)};

    address[machine.loom11] := Map
    {Ip6PktFormat.Address{Net.htonl(16_5f02ad00),
                          Net.htonl(16_805f0200),
                          Net.htonl(16_00000060),
                          Net.htonl(16_97148c7f)},
     Net.htonl(16_805f029f)};

    address[machine.weft] := Map
    {Ip6PktFormat.Address{Net.htonl(16_5f02ad00),
                          Net.htonl(16_805f0200),
                          Net.htonl(16_000000a0),
                          Net.htonl(16_c9069700)},
     Net.htonl(16_805f02db)};

    FOR i := FIRST(machine) TO LAST(machine) DO
      WITH v4address = Net.htonl(ip4node6) DO
        (* mapping from ip4node4->ip6node4
        EVAL ip4node4TOip6node4.put(address[i].ip4, address[i].ip6);
        *)

        (* mapping from ip4node6->ip6node6 *)
        EVAL ip4node6TOip6node6.put(v4address, address[i].ip6);
        (* mapping from ip6node6->ip4node6 *)
        EVAL ip6node6TOip4node6.put(address[i].ip6, v4address);

        (* mapping from ip6node4->ip4node4 *)
        EVAL ip6node4TOip4node4.put(address[i].ip6, address[i].ip4);
      END;
      INC(ip4node6);
    END;    

    IP4toIP6 := NEW(T4);
    IP4toIP6.mask := Net.htonl(16_ffff0000);
    IP4toIP6.addr := Net.htonl(Word.And(16_ffff0000,ip4node6));

    bindings[proto.ip] := InstallWithClosure(
                              Ip.PacketArrived,
                              Guard_IP4,
                              PacketArrived_IP4,
                              IP4toIP6);

    bindings[proto.ip6] := InstallWithClosure(
                               Ip6.PacketArrived,
                               Guard_IP6,
                               PacketArrived_IP6);

    shell := Commands.Install(Run, CommandName, CommandHelp);

    IF verbose THEN IO.Put("Dht module initialized.\n"); END;
  END Init;

PROCEDURE Uninstall(binding: REFANY) =
  BEGIN
    WITH s = NARROW(binding,Dispatcher.Binding) DO
      TRY
        Dispatcher.Uninstall(s);
      EXCEPT
      | Dispatcher.Error(ec) => ExceptionPrint(ec);
      END;
    END;
  END Uninstall;

PROCEDURE Uninit(<*UNUSED*>verbose:BOOLEAN) = 
  BEGIN
    FOR i := FIRST(bindings) TO LAST(bindings) DO
      IF bindings[i] # NIL THEN
        Uninstall(bindings[i]); 
        bindings[i] := NIL;
      END;
    END;
    Commands.Uninstall(shell);
    ip4node4TOip6node4 := NIL;
    ip4node6TOip6node6 := NIL;
    ip6node4TOip4node4 := NIL;
    ip6node6TOip4node6 := NIL;
    shell := NIL;
    dev   := NIL;
    route := NIL;
  END Uninit; 

BEGIN
  Init(FALSE);
END Dht. 

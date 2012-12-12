(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Dec-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added support for unloading.
 *
 * 09-Dec-97  David Becker at the University of Washington
 *	Removed myAddr so hosts can have multiple addrs.
 *	Fixed the RequestHandler to function.
 *
 * 28-Sep-97  Yasushi Saito (yasushi) at the University of Washington
 *	Merged reply/request handlers to one.
 *
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
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
 *	Fixed ArpIp record to use IpPktFormat.Address.
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

MODULE EtherArp;

IMPORT ArpPktFormat, Ctypes, Ether, EtherArpKey, EtherArpTbl,
       EtherGen, EtherPktFormat, IO, If, IpPktFormat, Mbuf,
       MbufPublic, Net, SocketRep, SocketAddr;
IMPORT EtherDev;
IMPORT Clock, Mutex, Thread; <* NOWARN *>

CONST debug = TRUE;

TYPE ArpIp = RECORD (* must be packed *)
  header              : ArpPktFormat.Header;
  srcHardwareAddress  : EtherPktFormat.Address;
  srcIpAddress        : IpPktFormat.AddressArray;
  dstHardwareAddress  : EtherPktFormat.Address;
  dstIpAddress        : IpPktFormat.AddressArray;
END;

VAR ArpRequestType := Net.htons(ArpPktFormat.REQUEST);
VAR ArpReplyType := Net.htons(ArpPktFormat.REPLY);

(* arp table timeout values *)
CONST THREEMINUTES  = 1024 * 60 * 3;

CONST
  arp_len = BYTESIZE(ArpIp);
  eth_hdr_len = BYTESIZE(EtherPktFormat.Header);


TYPE Cache = RECORD
  table: EtherArpTbl.Default;
  mutex: MUTEX;
END;

VAR cache : Cache;
    key   : T;

TYPE fastcache = RECORD 
  protocolAddress : IpPktFormat.Address; 
  entry: T;
END;
VAR cacheentry : ARRAY BOOLEAN OF fastcache;
VAR cacheN : BOOLEAN := FALSE;

PROCEDURE Resolve(
    <*UNUSED*> proType : Ctypes.unsigned_short; 
    protocolAddress    : IpPktFormat.Address; 
    VAR entry          : T):ResolveResult = 
  BEGIN
    entry := NIL;

    (* look for the ether dst address *)
    LOCK cache.mutex DO
      (* look into the cache to avoid overhead of m3 table generic *)
      IF cacheentry[cacheN].protocolAddress = protocolAddress THEN
        entry := cacheentry[cacheN].entry;
        cacheN := NOT cacheN;
      ELSE
        IF cache.table.get(protocolAddress,entry) THEN
          cacheentry[cacheN].protocolAddress := protocolAddress;
          cacheentry[cacheN].entry := entry;
          cacheN := NOT cacheN;
        END;
      END;
    END;

    (* if there is a cache item then entry # NIL *)
    IF entry = NIL THEN 
      RETURN ResolveResult.NotInCache;
    ELSIF NOT Status.Resolved IN entry.status THEN 
      RETURN ResolveResult.NotResolved;
    END;
    RETURN ResolveResult.OK;
  END Resolve;

<*OBSOLETE*> 
PROCEDURE Delete(<* UNUSED *>e:REFANY) = 
  BEGIN
  END Delete;

PROCEDURE Stale(e:REFANY) = 
  VAR
    entry: T;
  BEGIN
    entry := NARROW(e,T);
    entry.status := entry.status + StatusSet{Status.Stale};
    entry.status := entry.status - StatusSet{Status.Resolved};
  END Stale;

PROCEDURE Add(VAR entry:T) = 
  VAR eTmp: T;
  BEGIN
    LOCK cache.mutex DO 
      IF cache.table.get(VIEW(entry.protocolAddress,EtherArpKey.T),eTmp) THEN
        entry := eTmp;
      ELSE
        EVAL cache.table.put(VIEW(entry.protocolAddress,EtherArpKey.T),entry);
      END;
    END;
    IF NOT Status.Permanent IN entry.status THEN 
      EVAL Clock.CancelAlarm(Stale,entry);
      Clock.SetAlarm(THREEMINUTES,Stale,entry);
    END;
  END Add;

PROCEDURE SendRequest(
    dev               : EtherDev.T; 
    READONLY dst      : AddressMapping;
    srcProto          : ProtoAddrT; 
    READONLY srcEther : HardwareAddrT) =
  VAR
    data  : Mbuf.T;
    new_s : SocketAddr.T;
  BEGIN
    (* create a new mbuf to send the arp packet in *)
    data := Mbuf.m_gethdr(Mbuf.M_WAIT,Mbuf.MT_DATA);
    MbufPublic.SetPktHdrLen(data,arp_len);
    data.mh_hdr.mh_len := arp_len;
    Mbuf.MH_ALIGN(data,arp_len);

    WITH data_out = Mbuf.Array(data),
         arp_out = VIEW(data_out^,ArpIp)
      DO
         
       (* setup the arp header *)
       arp_out.header.hrd := Net.htons(1); (* XXX ether hardware type *)
       arp_out.header.pro := Net.htons(EtherPktFormat.ETHERTYPE_IP);
       arp_out.header.hln := BYTESIZE(EtherPktFormat.Address);
       arp_out.header.pln := BYTESIZE(IpPktFormat.Address);
       arp_out.header.op  := ArpRequestType;

       (* setup the arp packet *)
       arp_out.dstHardwareAddress := EtherPktFormat.zero;
       arp_out.srcHardwareAddress := srcEther;

       (* assumed to be in network byte order *)
       arp_out.dstIpAddress := dst.protocolAddress;
       arp_out.srcIpAddress := srcProto;

       (* set the socket family so we can just send raw ether packets *)
       new_s.sa_family := SocketRep.AF_UNSPEC;
       WITH ether_header = VIEW(new_s.sa_data,EtherPktFormat.T) DO
         ether_header.shost := srcEther;
         ether_header.dhost := EtherPktFormat.broadcast;
         ether_header.type  := Net.htons(EtherPktFormat.ETHERTYPE_ARP);
       END;
       (* send it out on the interface where this packet was received from *)
     END;
    EVAL EtherGen.PacketSend(dev,data,new_s);
  END SendRequest;

PROCEDURE Lookup(dev: EtherDev.T; VAR mbuf: Mbuf.T; VAR s: SocketAddr.T) =
  VAR
    srcEther      : EtherDev.EtherAddr;
    entry         : T;
    resolveresult : ResolveResult;

  PROCEDURE InitEntry(READONLY ipaddr: ProtoAddrT) = 
    BEGIN
      entry.status := StatusSet{Status.Nascent};
      entry.proType := Net.htons(EtherPktFormat.ETHERTYPE_IP);
      entry.protocolAddress := ipaddr;
    END InitEntry;

  BEGIN

    (* set ether addr stuff *)
    WITH ip_data = Mbuf.Array(mbuf)^,
         ip = VIEW(ip_data,IpPktFormat.T),
         ipdaddr = VIEW(s.sa_data,ProtoAddrT),
         ipsaddr = VIEW(ip.saddr,ProtoAddrT)
     DO
        (* Get the ether source address. *)
	dev.etherAddr(srcEther);

        resolveresult := Resolve(Net.htons(EtherPktFormat.ETHERTYPE_IP), ip.saddr, entry);
        
        IF resolveresult = ResolveResult.NotResolved THEN
          (* The entry exist, but it isn't resolved yet? *)
          InitEntry(ipdaddr);
          IF mbuf # NIL THEN If.Enqueue(entry.ifq, mbuf); END;
          SendRequest(dev, entry^, ipsaddr, srcEther);

        ELSIF resolveresult = ResolveResult.NotInCache THEN
          (* The entry doesn't exist, yet.  Create one and install in table. *)
          entry := NEW(T);
          InitEntry(ipdaddr);
          Add(entry);
          IF mbuf # NIL THEN If.Enqueue(entry.ifq, mbuf); END;
          SendRequest(dev, entry^, ipsaddr, srcEther);

        ELSE
          (* The entry is already resolved and we'll just send it out. *)
          IF mbuf # NIL THEN
            EVAL EtherGen.PacketSend(dev, mbuf,s);
          END;
        END;
    END;
  END Lookup;

FUNCTIONAL
PROCEDURE ArpGuard(<*UNUSED*> packet: Mbuf.T; 
		   curr: Mbuf.T; 
		   offset: CARDINAL):BOOLEAN =
  BEGIN
    WITH etherHdr = VIEW(SUBARRAY(Mbuf.Array(curr)^, offset, eth_hdr_len),
			 EtherPktFormat.T) DO
      RETURN etherHdr.type = EtherPktFormat.ETHERTYPE_ARP;
    END;
  END ArpGuard;
  
PROCEDURE ArpHandler(packet: Mbuf.T; curr: Mbuf.T; offset: CARDINAL):BOOLEAN =
  BEGIN
    WITH hdr = Mbuf.Array(curr)^,
      arp = VIEW(SUBARRAY(hdr, offset + eth_hdr_len, arp_len), ArpIp) DO 
      IF arp.header.op = ArpRequestType THEN
	RETURN ArpRequestHandler(arp, packet, offset);
      ELSIF arp.header.op = ArpReplyType THEN
	RETURN ArpReplyHandler(arp, packet, offset);
      ELSE
	RETURN FALSE;
      END;
    END;
  END ArpHandler;
  
PROCEDURE ArpRequestHandler (VAR arpDataIn: ArpIp;
			     packet: Mbuf.T; offset: CARDINAL):BOOLEAN =
  VAR
    data : Mbuf.T;
    ifdevea : EtherDev.EtherAddr;
    s : SocketAddr.T;
    dev : EtherDev.T := MbufPublic.GetPktHdrRcvIf(packet);
    ipaddr  := dev.ipAddr();
    ipaddrA: ALIGNED 32 FOR IpPktFormat.AddressArray := VIEW(ipaddr, IpPktFormat.AddressArray);
  BEGIN
    dev.etherAddr(ifdevea);

    IF arpDataIn.dstIpAddress# ipaddrA  THEN RETURN FALSE END;

    data := Mbuf.m_gethdr(Mbuf.M_WAIT,Mbuf.MT_DATA);
    MbufPublic.SetPktHdrLen(data,arp_len);
    data.mh_hdr.mh_len := arp_len;
    Mbuf.MH_ALIGN(data,arp_len);

    WITH etherHeaderOut   = VIEW(s.sa_data,EtherPktFormat.T),
         arpDataOutBuf    = SUBARRAY(Mbuf.Array(data)^,0,arp_len),
         arpDataOut       = VIEW(arpDataOutBuf,ArpIp),
         etherHeaderInBuf = SUBARRAY(Mbuf.Array(packet)^,offset, eth_hdr_len),
         etherHeaderIn    = VIEW(etherHeaderInBuf,EtherPktFormat.T)
     DO
      (* set ethernet header fields *)
      etherHeaderOut.dhost := etherHeaderIn.shost;
      etherHeaderOut.shost := ifdevea;
      etherHeaderOut.type  := Net.htons(EtherPktFormat.ETHERTYPE_ARP);

      (* set arp header fields *)
      arpDataOut.header             := arpDataIn.header;
      arpDataOut.header.op          := ArpReplyType;
      arpDataOut.srcHardwareAddress := etherHeaderOut.shost;
      arpDataOut.dstHardwareAddress := etherHeaderOut.dhost;
      arpDataOut.srcIpAddress       := ipaddrA;
      arpDataOut.dstIpAddress       := arpDataIn.srcIpAddress;
    END;
    (* set the socket family so we can just send raw ether packets *)
    s.sa_family := SocketRep.AF_UNSPEC;
    EVAL EtherGen.PacketSend(dev,data,s);
    RETURN TRUE;
  END ArpRequestHandler;

PROCEDURE ArpReplyHandler(
    VAR arpData: ArpIp;
    packet: Mbuf.T; 
    <*UNUSED*>offset: CARDINAL):BOOLEAN =
  VAR
    entry : T;
    mbuf: Mbuf.T;
    m3s: SocketAddr.T;
    ipsaddr: IpPktFormat.Address;
    ipsaddrA: ALIGNED 32 FOR IpPktFormat.AddressArray;
    resolveresult : ResolveResult;
    ifdevea : EtherDev.EtherAddr;
    dev : EtherDev.T := MbufPublic.GetPktHdrRcvIf(packet);

  PROCEDURE UpdateEntry(READONLY arp: ArpIp) =
    BEGIN
      entry.status  := entry.status + StatusSet{Status.Resolved};
      entry.status  := entry.status - StatusSet{Status.Stale,Status.Nascent};
      entry.proType := arp.header.pro;
      entry.protocolAddress := arp.srcIpAddress;
      entry.hardwareAddress := arp.srcHardwareAddress;
    END UpdateEntry;

  BEGIN
    dev.etherAddr(ifdevea);
    ipsaddrA := arpData.srcIpAddress;
    ipsaddr  := VIEW(ipsaddrA,IpPktFormat.Address); (* XXX copy *)
    
    resolveresult := Resolve(arpData.header.pro, (* already in network order *)
			     ipsaddr, (* already in network order *)
			     entry);
    
    IF resolveresult = ResolveResult.NotInCache THEN
      entry := NEW(T);
      UpdateEntry(arpData);
      Add(entry);
    ELSE
      UpdateEntry(arpData);
    END;
    
    IF arpData.dstIpAddress = ipsaddrA THEN 
      IF arpData.dstHardwareAddress # ifdevea THEN
	IF debug THEN
	  IO.PutError("WARNING EtherArp detected duplicate ip host.\n");
	END;
      END;
    END;
    
    m3s.sa_family := SocketRep.AF_INET;
    SUBARRAY(m3s.sa_data,0,BYTESIZE(entry.protocolAddress)) := entry.protocolAddress;
    
    (* XXX getting interface device in packet header.
       Fix when Mbuf datastructure is opaque.
    *)
    WITH dev=MbufPublic.GetPktHdrRcvIf(packet) DO
      LOOP
	mbuf := If.Dequeue(entry.ifq);
	IF mbuf = NIL THEN EXIT; END;
	EVAL EtherGen.PacketSend(dev,mbuf,m3s);
      END;
    END;
    RETURN TRUE;
  END ArpReplyHandler;

PROCEDURE CacheDelete(READONLY key: AddressMapping) =
  VAR entry: T;
  BEGIN
    LOCK cache.mutex DO 
      EVAL cache.table.delete(VIEW(key.protocolAddress,IpPktFormat.Address),entry);
    END;
  END CacheDelete; 

PROCEDURE CacheDump(proc: PROCEDURE(READONLY entry: AddressMapping)) = 
  VAR entry: T;
      key: EtherArpKey.T;
  BEGIN
    LOCK cache.mutex DO
      WITH iterate = cache.table.iterate() DO
        WHILE iterate.next(key,entry) = TRUE DO
          proc(entry^);
        END;
      END;
    END;
  END CacheDump;

VAR arpBinding: REFANY;

PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
    cache.table := NEW(EtherArpTbl.Default).init();
    cache.mutex := NEW(MUTEX);
    key := NEW(T);
    arpBinding := Ether.Install(Ether.PacketArrived, ArpGuard, ArpHandler);
    cacheentry[TRUE] := fastcache{0,NIL};
    cacheentry[FALSE] := fastcache{0,NIL};
    IF verbose THEN IO.Put("EtherArp initialized.\n"); END;
  END Init;

PROCEDURE Uninit(verbose:BOOLEAN) =
  VAR
    mutex : MUTEX;
  BEGIN
    mutex := cache.mutex;
    LOCK mutex DO
      cache.table := NIL;
      cache.mutex := NIL;
    END;
    FOR i := FIRST(cacheentry) TO LAST(cacheentry) DO
      cacheentry[i] := fastcache{0,NIL};
    END;
    IF arpBinding # NIL THEN
      Ether.Uninstall(arpBinding);
      arpBinding := NIL;
    END;
    IF verbose THEN IO.Put("EtherArp unloaded.\n"); END;
  END Uninit;

BEGIN
END EtherArp.

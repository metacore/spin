(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 05-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	Updating to arp implementation with cache, timeouts, etc.
 *
 * 05-Mar-95  Marc Fiuczynski (mef) at the University of Washington
 *	Makeshift ARP table implementation.
 *)

UNSAFE MODULE An1Arp;
IMPORT ArpPktFormat;
IMPORT SocketRep;
IMPORT IO;
IMPORT Clock;
IMPORT Ctypes;
IMPORT An1Gen;
IMPORT An1;
IMPORT An1PktFormat;
IMPORT An1Trusted;
IMPORT Fmt;
IMPORT IpPktFormat;
IMPORT Mbuf;
IMPORT Net;
IMPORT NetDb;
IMPORT Word;
IMPORT Mutex, Thread;
IMPORT IoctlPosix;
IMPORT If;
IMPORT An1ArpTbl;

(* DynShell support *)
IMPORT Text, SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;

<* UNUSED *> VAR shell := Dispatcher.Install(SpinShell.RunCommand,EC_Guard,RunCommand);

PROCEDURE EC_Guard(argc: INTEGER; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "An1Arp");
  END EC_Guard;

PROCEDURE RunCommand(argc: INTEGER; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
    ipaddr : IpPktFormat.Address;
  BEGIN
    IF argc > 1 AND Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF argc > 1 AND Text.Equal(argv[1], "-a") THEN
      CacheDump();
    ELSIF argc > 1 THEN (* its should be a host name *)
      TRY
        ipaddr := NetDb.GetHostByName(argv[1]);
      EXCEPT
      | NetDb.HostNotFound => 
        IO.Put("An1Arp: Could not find host " & argv[1] & " ip address\n");
      END;
      
    ELSE
      IO.Put("An1Client.RunCommand: no such command ");
      FOR i := 0 TO argc DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;
    RETURN TRUE;
  END RunCommand;

VAR
  debug_level : Net.Level := Net.oLevel.NODEBUG;
 
TYPE ArpIp = RECORD (* must be packed *) (* XXX fix this to work *)
  header              : ArpPktFormat.Header;
  srcHardwareAddress  : An1PktFormat.Address;
  srcIpAddress        : ARRAY [1..BYTESIZE(IpPktFormat.Address)] OF CHAR;
  destHardwareAddress : An1PktFormat.Address;
  destIpAddress        : ARRAY [1..BYTESIZE(IpPktFormat.Address)] OF CHAR;
END;

VAR arprequesttype := Net.htons(ArpPktFormat.REQUEST);
VAR arpreplytype := Net.htons(ArpPktFormat.REPLY);

(* arp table timeout values *)
CONST TWENTYMINUTES = 1024 * 60 * 20;
      THREEMINUTES  = 1024 * 60 * 3;

TYPE Cache = RECORD
  table: An1ArpTbl.Default;
  mutex: MUTEX;
END;

VAR cache : Cache;
    key   : T;

PROCEDURE Resolve(proType		: Ctypes.unsigned_short; 
                  protocolAddress	: ProtoAddrT; 
                  VAR entry :T
                 ) RAISES { NotResolved,NotInCache } =
  VAR inCache: BOOLEAN;
  BEGIN
    LOCK cache.mutex DO
      (* IF Mutex.TryLock(cache.mutex) THEN *)
      key.protocolAddress := protocolAddress;
      key.proType := proType;
      inCache := cache.table.get(key,entry);
      (* Thread.Release(cache.mutex);*)
      IF NOT inCache THEN RAISE NotInCache;
      ELSIF NOT entry.resolved THEN RAISE NotResolved; 
      END;
    END;
  END Resolve;

PROCEDURE Add(entry:T) = 
  VAR timeout:CARDINAL;
  BEGIN
    (* IO.Put("An1Arp.Add"); *)
    LOCK cache.mutex DO 
      (* IO.Put(" got lock -- getting table entry"); *)
      EVAL cache.table.put(entry,entry);
    END;
    (* IO.Put("\n"); *)

    (* XXX need to make sure that clients cannot lie 
       about whan1 an entry is resolved or not. *)
    IF entry.resolved THEN 
      timeout := TWENTYMINUTES;
    ELSE
      timeout := THREEMINUTES;
    END;
    (* Clock.SetAlarm(timeout,Delete,entry); *)
  END Add;

PROCEDURE Delete(entry:T) = 
  BEGIN
    LOCK cache.mutex DO
      (* guard should check for valid protocol address *)
      EVAL cache.table.delete(entry,entry);
    END;
    (* Clock.CancelAlarm(Delete,entry); *) (* deregister alarm *)
  END Delete;

PROCEDURE Lookup(VAR ifp: If.ifnet; VAR mbuf: Mbuf.T; VAR s: SocketRep.M3sockaddr) RAISES { NotResolved } =
  VAR
    data  : Mbuf.T;
    packet: Mbuf.T;
    new_s: SocketRep.M3sockaddr;
    src, dst: T;
    inCache: BOOLEAN;

  PROCEDURE foo(arp_out : UNTRACED REF ArpIp; 
    ipdaddr, ipsaddr: IpPktFormat.Address;
    ) = 
    BEGIN
        If.Enqueue(dst.ifq, mbuf);

        (* setup the arp header *)
        arp_out.header.hrd := Net.htons(1); (* XXX an1 hardware type *)
        arp_out.header.pro := Net.htons(An1PktFormat.AN1TYPE_IP);
        arp_out.header.hln := BYTESIZE(An1PktFormat.Address);
        arp_out.header.pln := BYTESIZE(IpPktFormat.Address);
        arp_out.header.op  := arprequesttype;

        (* setup the arp packet *)
        arp_out.destHardwareAddress := An1PktFormat.Address{0,0,0,0,0,0};
        arp_out.srcHardwareAddress  := src.hardwareAddress;
        (* assumed to be in network byte order *)
        WITH source = ADR(dst.protocolAddress),
             dest   = ADR(arp_out.destIpAddress[FIRST(arp_out.destIpAddress)])
         DO
          bcopy(source,dest,BYTESIZE(IpPktFormat.Address));
        END;
        WITH source = ADR(src.protocolAddress),
             dest   = ADR(arp_out.srcIpAddress[FIRST(arp_out.srcIpAddress)])
         DO
          bcopy(source,dest,BYTESIZE(IpPktFormat.Address));
        END;

        (* set the socket family so we can just send raw an1 packets *)
        new_s.sa_family := SocketRep.AF_UNSPEC;
        WITH an1_header = VIEW(new_s.sa_data^,An1PktFormat.NewT) DO
          an1_header.shost := src.hardwareAddress;
          an1_header.dhost := An1PktFormat.broadcast;
          an1_header.type  := Net.htons(An1PktFormat.AN1TYPE_ARP);
        END;
        (* send it out on the interface where this packet was received from *)
        EVAL An1Gen.PacketSend(ifp,data,new_s);
    END foo;

  BEGIN

    (* create a new mbuf to send the arp packet in *)
    data := Mbuf.m_get(Mbuf.M_WAIT,Mbuf.MT_DATA);
    WITH mh_len = data.mh_hdr.mh_len DO
      mh_len := BYTESIZE(ArpIp);
      Mbuf.M_ALIGN(data,mh_len);
    END;

    (* set an1 addr stuff *)
    WITH data_out = Mbuf.Array(data),
         arp_out = VIEW(data_out), ArpIp),
         ip_data = Mbuf.Array(mbuf), (* get the ip_data area *)
         ip = VIEW(ip_data^,IpPktFormat.NewT),
         ipdaddr = VIEW(s.sa_data,IpPktFormat.Address)
     DO
      TRY
        (* setup the an1net header *)
        Resolve(Net.htons(An1PktFormat.AN1TYPE_IP), ip.saddr,src);
        LOCK cache.mutex DO
          key.protocolAddress := ipdaddr;
          key.proType := Net.htons(An1PktFormat.AN1TYPE_IP);
          inCache := cache.table.get(key,dst);
        END;
        IF inCache AND NOT dst.resolved THEN
          dst.resolved := FALSE;
          dst.proType := Net.htons(An1PktFormat.AN1TYPE_IP);
          dst.protocolAddress := ipdaddr;
          foo(arp_out, ipdaddr, ip.saddr);
        ELSIF NOT inCache THEN 
          dst := NEW(T);
          dst.resolved := FALSE;
          dst.proType := Net.htons(An1PktFormat.AN1TYPE_IP);
          dst.protocolAddress := ipdaddr;
          Add(dst);
          foo(arp_out, ipdaddr, ip.saddr);
        ELSE
          (* hey, its resolved already --- send it out the regular way *)
          EVAL An1Gen.PacketSend(ifp,mbuf,s);
        END;
      EXCEPT
      | NotResolved, NotInCache => 
        Mbuf.m_freem(data); 
        RAISE NotResolved;
      END;
    END;
  END Lookup;


PROCEDURE ArpRequestGuard(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN = 
  BEGIN
    (* WITH eth = VIEW(payload^,T) DO *)
    WITH eth = VIEW(payload^,An1PktFormat.NewT),
         arp_data = Mbuf.Array(packet),
         arp = VIEW(arp_data^,UNTRACED REF ArpIp)
     DO
      RETURN eth.type = An1PktFormat.AN1TYPE_ARP AND   arp.header.op = arprequesttype;
    END;
  END ArpRequestGuard;

PROCEDURE ArpRequestHandler(READONLY packet: Mbuf.T; READONLY payload: An1.NewT) = 
  CONST
    eth_hdr_len = BYTESIZE(An1PktFormat.Header);
    arp_len = BYTESIZE(ArpIp);
  VAR
    data : Mbuf.T;
    header: Mbuf.T;
    an1Address: An1PktFormat.Address;
    ifdevea : If.ifdevea;
  BEGIN
    (*
    WITH ifp = LOOPHOLE(pkthdr.rcvif, UNTRACED REF If.ifnet) DO
      IF ifp.if_ioctl(ifp,IoctlPostix.SIOCRPHYSADR,ADR(ifdevea)) = 0 THEN
        IO.Put("An1Arp succeeded getting an1net address using if_ioctl!\n");
      ELSE
        IO.Put("An1Arp could not get an1net address using if_ioctl!\n");
      END;
    END;

    data := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
    WITH mh_len = data.mh_hdr.mh_len DO
      mh_len := arp_len;
      Mbuf.M_ALIGN(data,mh_len);
      header := Mbuf.m_prepend(data,eth_hdr_len, Mbuf.M_WAIT);
    END;

    WITH header_out = Mbuf.Array(header),
         eth_out = VIEW(header_out^,An1PktFormat.NewT),
         data_out = Mbuf.Array(data),
         arp_out = VIEW(data_out^,ArpIp),
         eth_in = VIEW(payload^,An1PktFormat.NewT),
         arp_data = Mbuf.Array(packet),
         arp_in = VIEW(arp_data^,ArpIp)
     DO
      eth_out.dhost := eth_in.shost;
      eth_out.shost := ifdevea.current_pa;
      eth_out.type  := Net.htons(An1PktFormat.AN1TYPE_ARP);
      arp_out.header := arp_in.header;
      arp_out.header.op := ArpPktFormat.REPLY;
      arp_out.srcHardwareAddress := eth_out.shost;
      arp_out.dstHardwareAddress := eth_out.dhost;
    END;
    *)
  END ArpRequestHandler;

PROCEDURE ArpReplyGuard(READONLY packet: Mbuf.T; READONLY payload: An1.T):BOOLEAN = 
  BEGIN
    (* WITH eth = VIEW(payload^,T) DO *)
    WITH eth = VIEW(payload^,An1PktFormat.NewT),
         arp_data = Mbuf.Array(packet),
         arp = VIEW(arp_data^,UNTRACED REF ArpIp)
     DO
      RETURN eth.type = An1PktFormat.AN1TYPE_ARP AND arp.header.op = arpreplytype;
    END;
  END ArpReplyGuard;


PROCEDURE ArpReplyHandler(READONLY packet: Mbuf.T; READONLY payload: An1.NewT) = 
  VAR
    entry : T;
    inCache : BOOLEAN;
    mbuf: Mbuf.T;
    m3s: SocketRep.M3sockaddr;    
    s: SocketRep.sockaddr;
    arp: UNTRACED REF ArpIp;
    ipdaddr: IpPktFormat.Address;
  PROCEDURE DoEntry() =
    BEGIN
      entry.resolved := TRUE;
      entry.proType  := arp.header.pro;

      WITH source = ADR(arp.srcIpAddress[FIRST(arp.srcIpAddress)]),
           dest   = ADR(entry.protocolAddress)
       DO
        bcopy(source,dest,BYTESIZE(IpPktFormat.Address));
      END;
      entry.hardwareAddress := arp.srcHardwareAddress; (* this is an array copy *)
    END DoEntry;

  BEGIN
    WITH arp_data = Mbuf.Array(packet) DO
      arp := VIEW(arp_data^,ArpIp); (* XXX probably get an UNALIGNED fault here. *)
      TRY
        TRY
          WITH source = ADR(arp.srcIpAddress[FIRST(arp.srcIpAddress)]),
               dest   = ADR(ipdaddr)
           DO
            bcopy(source,dest,BYTESIZE(IpPktFormat.Address));
          END;

          (* XXX probably get an UNALIGNED fault here when access the fields from arp reference. *)
          Resolve(arp.header.pro, (* already in network order *)
                  ipdaddr, (* already in network order *)
                  entry);
          
        EXCEPT
        | NotInCache =>
          entry := NEW(T);
          DoEntry();
          Add(entry);
        | NotResolved =>
          DoEntry();
        END;
      FINALLY
        m3s.sa_family := SocketRep.AF_INET;
        WITH ipdaddr = VIEW(m3s.sa_data,IpPktFormat.Address) DO
          ipdaddr := entry.protocolAddress;
        END;

        (* resend all queued up packets waiting for this to be resolved. *)
        mbuf := If.Dequeue(entry.ifq);
        WITH pkthdr=LOOPHOLE(ADR(packet.M_dat[FIRST(packet.M_dat)]),UNTRACED REF Mbuf.pkthdrT),
             ifp=LOOPHOLE(pkthdr.rcvif, UNTRACED REF If.ifnet)
         DO
          WHILE mbuf # NIL DO
            (* call output function via ifp.if_output() *)
            EVAL An1Gen.PacketSend(ifp^,mbuf,m3s);
            mbuf := If.Dequeue(entry.ifq);
          END;
        END;
      END;
    END;
  END ArpReplyHandler;

PROCEDURE CacheDump() = 
  VAR entry: T;
  BEGIN
    LOCK cache.mutex DO
      WITH iterate = cache.table.iterate() DO
        WHILE iterate.next(key,entry) = TRUE DO
          IO.Put("(" & Fmt.Unsigned(Net.nltoh(entry.protocolAddress)));
          IF entry.resolved THEN 
            IO.Put(" ) resolved at ");
            FOR i := FIRST(entry.hardwareAddress) TO LAST(entry.hardwareAddress) DO
              IO.Putx(entry.hardwareAddress[i]); IO.Put(" ");
            END;
            IO.Put("\n");
          ELSE
            IO.Put(" ) unresolved. \n");
          END
        END;
      END;
    END;
  END CacheDump;

VAR arprequest: REFANY;
VAR arpreply: REFANY;
PROCEDURE Init () =
  VAR entry : T;
  BEGIN
  IO.Put("An1Arp initializing an1net arp cache.\n");
  cache.table := NEW(An1ArpTbl.Default).init();
  cache.mutex := NEW(MUTEX);
  key := NEW(T);

  entry := NEW(T, resolved := TRUE, proType := Net.htons(An1PktFormat.AN1TYPE_IP));
  entry.protocolAddress := NetDb.GetHostByName("spinoff-an1");
  entry.hardwareAddress:=An1PktFormat.Address{16_08, 16_2b, 16_9b, 16_12, 16_6f, 16_fc};
  Add(entry);

  entry := NEW(T, resolved := TRUE, proType := Net.htons(An1PktFormat.AN1TYPE_IP));
  entry.protocolAddress := NetDb.GetHostByName("spincycle-an1");
  entry.hardwareAddress:=An1PktFormat.Address{16_08, 16_2b, 16_0b, 16_82, 16_67, 16_f3};
  Add(entry);

  arpreply := An1Trusted.Install(An1.PacketArrived,
                                     ArpReplyGuard,
                                     ArpReplyHandler);
  IO.Put("An1Arp() an1 arp request handler installed.\n");

  (*
  arprequest := An1Trusted.Install(An1.PacketArrived,
                                     ArpReplyGuard,
                                     ArpReplyHandler);
  IO.Put("An1Arp() an1 arp request handler installed.\n");
  *)
  END Init;

(* 
 * For Generic table
 *)

PROCEDURE Equal(pa1,pa2: T): BOOLEAN =
  BEGIN RETURN pa1.protocolAddress = pa2.protocolAddress AND pa1.proType = pa2.proType; END Equal;

PROCEDURE Hash(pa: T): Word.T = 
  BEGIN RETURN pa.protocolAddress; END Hash;

BEGIN
END An1Arp.

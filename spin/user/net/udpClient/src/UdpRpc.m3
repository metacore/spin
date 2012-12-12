(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel and CPU interfaces
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaning up for release.
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted to use new spin shell commands.
 *	Converted from obsolete Clib interface to IO.
 *
 * 09-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to use corrected mbuf interface.
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread.
 *
 * 08-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	This module test the latency performance of an RPC call and the
 *	bandwidth using an UDP implementation that sends out large
 *	packets with a window size of two.
 *
 *)

(* Untrusted *) 
MODULE UdpRpc;

IMPORT IO,Fmt;
IMPORT Ctypes;
IMPORT IpPktFormat;
IMPORT ThreadExtra;
IMPORT Mbuf;
IMPORT Net;
IMPORT NetDb;
IMPORT Sema;
IMPORT Spy;
IMPORT CPU;
IMPORT Udp;
IMPORT UdpGen;
IMPORT UdpPktFormat;
IMPORT Word;


IMPORT ParseParams;
IMPORT Dispatcher;

PROCEDURE ExceptionPrint(ec:Dispatcher.ErrorCode) = 
  BEGIN
    CASE ec OF
    | Dispatcher.ErrorCode.InvalidProcedure =>
      IO.Put("udprpc invalid procedure installed.\n");
    ELSE
      IO.Put("udprpc dispatcher install error.\n");
    END;
  END ExceptionPrint;

PROCEDURE Install (
    event: Udp.PacketArrivedEvent;
    whenClause: Udp.PacketArrivedEvent; 
    handler: Udp.PacketArrivedEvent): Dispatcher.Binding =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    TRY
      TRY
        binding := Dispatcher.InstallHandler(event,
                                             whenClause, 
                                             handler,
                                             options := Dispatcher.Options{Dispatcher.Opt.First}
                                             );
      EXCEPT
      | Dispatcher.Error(ec) => ExceptionPrint(ec);
      END;
    FINALLY
      RETURN binding;
    END;
  END Install;


PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  VAR j: CARDINAL;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* udprpc *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-burn") THEN
        burntime := pp.getNextInt();
      ELSIF pp.testNext("-send") THEN
        clntudp_send(pp.getNext(), 
                     pp.getNext(), 
                     pp.getNextInt(),
                     pp.getNextInt());
      ELSIF pp.testNext("-cport") THEN
        client_port := Net.htons(pp.getNextInt());
      ELSIF pp.testNext("-sport") THEN
        server_port := Net.htons(pp.getNextInt());
      ELSIF pp.testNext("-ackonly") THEN
        ackonly := TRUE;
      ELSIF pp.testNext("-reflect") THEN
        ackonly := FALSE;
      ELSIF pp.testNext("-installserver") THEN
        (* install server handlers *)
        rpc_server := Udp.Install(Udp.PacketArrived,
                                  rpc_server_WC,
                                  rpc_server_handler);
        IO.Put ("UdpRpc() udp rpc server handler installed.\n");
      ELSIF pp.testNext("-installclient") THEN
        (* install client handlers *)
        rpc_client := Udp.Install(Udp.PacketArrived,
                                  rpc_client_WC,
                                  rpc_client_handler);
        IO.Put ("UdpRpc() udp rpc client handler installed.\n");
      ELSIF pp.testNext("-installguard") THEN
        IF pp.testNext("empty") THEN
          j := pp.getNextInt();
          FOR i:= 1 TO j DO
            INC(currspindle);
            spindles[currspindle] := Install(Udp.PacketArrived,
                                             empty,
                                             pa);
          END;
        ELSIF pp.testNext("full") THEN
          j := pp.getNextInt();
          FOR i:= 1 TO j DO
            INC(currspindle);
            spindles[currspindle] := Install(Udp.PacketArrived,
                                             all_or_none_guard,
                                             pa);
          END;
        END;
      ELSIF pp.testNext("-uninstallguard") THEN
        WHILE currspindle > 0 DO
          Udp.Uninstall(spindles[currspindle]);
          spindles[currspindle] := NIL;
          DEC(currspindle);
        END;
      ELSIF pp.testNext("-addone") THEN
        IF one_spindle = NIL THEN
          one_spindle := Udp.Install(Udp.PacketArrived,
                                     one_guard,
                                     pa);
        ELSE
          IO.Put("Go away! Handler already installed for you.\n");
        END;
      ELSIF pp.testNext("-delone") THEN
        IF one_spindle = NIL THEN
          IO.Put("No handler installed.\n");
        ELSE
          Udp.Uninstall(one_spindle);
          one_spindle := NIL;
        END;
      ELSIF pp.testNext("-toggleall") THEN
        all_or_none := NOT all_or_none;
      ELSIF pp.testNext("-resetcount") THEN
        all_or_none_count := 0;
      ELSIF pp.testNext("-printcount") THEN
        IO.Put("guards evaluated: ");
        IO.PutInt(all_or_none_count);
        IO.Put("\n");
      ELSIF pp.testNext("-togglefire") THEN
        firefirefire := NOT firefirefire;
      ELSIF pp.testNext("-toggleone") THEN
        one := NOT one;
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

CONST
  timing  = TRUE;
  debug   = FALSE;
  iodebug = FALSE;


CONST 
  clientRpcPort : Ctypes.unsigned_short = 8888;
  serverRpcPort : Ctypes.unsigned_short = 4444;

TYPE client_info = RECORD
  ip      : IpPktFormat.Header;
  udp     : UdpPktFormat.Header;
  size    : INTEGER;
  numpkts : INTEGER;
  debug   : INTEGER;
END;

VAR (* spy timers *)
  udptimer       : Spy.T;  
  udpservertimer : Spy.T;
  udpclienttimer : Spy.T;
  clientdataflow : Spy.T;
  serverdataflow : Spy.T;
  mbuftimer : Spy.T;
  burntime : CARDINAL := 10000;

VAR
  client      : client_info;
  ackonly     : BOOLEAN := TRUE;
  rpc_server  : REFANY;
  rpc_client  : REFANY;
  client_port : Ctypes.unsigned_short;
  server_port : Ctypes.unsigned_short;
  sema        : Sema.T;

TYPE Header = RECORD 
  timer: BITS 32 FOR Ctypes.unsigned_int; 
END;

CONST
  ip_hdr_len     = BYTESIZE(IpPktFormat.Header);
  udp_hdr_len    = BYTESIZE(UdpPktFormat.Header);
  udpip_hdr_len = ip_hdr_len + udp_hdr_len;

PROCEDURE getmbuf(totlen:CARDINAL):Mbuf.T = 
  VAR m: Mbuf.T;
  BEGIN
    (* IF timing THEN Spy.Enter(mbuftimer); END; *)
    m := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
    m.mh_hdr.mh_len := totlen;
    IF totlen > Mbuf.MLEN THEN
      IO.Put("PANIC UdpRpc.getmbuf size request too large.\n");
      RETURN NIL;
    END;
    Mbuf.M_ALIGN(m,totlen);
    (* IF timing THEN Spy.Exit(mbuftimer); END; *)
    RETURN m;
  END getmbuf;

(* when clause for server rpc *)
FUNCTIONAL
PROCEDURE rpc_server_WC (
    <* UNUSED *> packet : Mbuf.T;
    curr                : Mbuf.T;
    offset              : CARDINAL):BOOLEAN =
  BEGIN
    WITH  udpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,udp_hdr_len),
          udp_header = VIEW(udpHeaderBuf,NewT) 
     DO
(*
      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO,"UdpRpc_server_WC ");
        Net.Debug(debug_level, Net.oLevel.INFO,Fmt.Int(udp_header.dport));
        Net.Debug(debug_level, Net.oLevel.INFO,Fmt.Int(server_port));
      END;
*)
      RETURN udp_header.dport = server_port;
    END;
  END rpc_server_WC; 

PROCEDURE rpc_server_handler (
    packet : Mbuf.T;
    <*UNUSED*>
    curr   : Mbuf.T; 
    <*UNUSED*>
    offset : CARDINAL):BOOLEAN =
  VAR
    ip		: IpPktFormat.Header;
    udp         : UdpPktFormat.Header;
    data	: Mbuf.T;
  BEGIN
    (* IF timing THEN Spy.Enter(udpservertimer); END; *)
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"UdpRpc_server_PA ");
    END;

    (* get the ip address to send the packet back to *)
    WITH origpkt   = Mbuf.Array(packet)
     DO

      (*
      IF timing THEN 
        WITH timer = VIEW(packet.mh_hdr.mh_union[0],Word.T),
             stop  = CPU.CycleCounter()
         DO
          Spy.Hit(serverdataflow,timer,stop);
        END;
      END;
      *)
      (* XXX assumes that all of the data is in one contiguous mbuf buffer *)
      WITH pkt = SUBARRAY(origpkt^,ip_hdr_len,BYTESIZE(UdpPktFormat.NewT)),
           udp_header = VIEW(pkt,UdpPktFormat.NewT) 
       DO 
        (* flip udp port information *)
        udp.sport := udp_header.dport;
        udp.dport := udp_header.sport;
        IF ackonly THEN
          udp.len := udp_hdr_len + BYTESIZE(Header);
        ELSE
          udp.len := Net.nstoh(udp_header.len);
        END;
      END;

      (* allocate outgoing mbuf and copy rpc timer into it *)
      data := getmbuf(udp.len);
      WITH pkt      = SUBARRAY(origpkt^,udpip_hdr_len,BYTESIZE(Header)),
           rpc_data = VIEW(pkt,Header),
           rpcpkt   = Mbuf.Array(data),
           rpc      = VIEW(rpcpkt^,Header) 
        DO
         rpc.timer := rpc_data.timer;
         IF iodebug THEN IO.Put("{"); IO.PutInt(rpc.timer); IO.Put("}"); END;
       END;

      WITH ip_header = VIEW(origpkt^,IpPktFormat.T) DO
        (* flip ip address *)
        ip.frag_off := 0;
        ip.saddr    := ip_header.daddr; (* in network order *)
        ip.daddr    := ip_header.saddr; (* in network order *)
        ip.protocol := IpPktFormat.IPPROTO_UDP;
        ip.tot_len  := udp.len + BYTESIZE(IpPktFormat.Header);
      END;
    END;

    (* IF timing THEN Spy.Exit(udpservertimer); END; *)

    (* need to send back same amount of data that we received *)
    UdpGen.PacketSend(ip,udp,data,FALSE);
    RETURN FALSE; (* TRUE if consuming packet *)
  END rpc_server_handler; 

(* when clause for client rpc *)
FUNCTIONAL
PROCEDURE rpc_client_WC (
    <*UNUSED*>
    packet: Mbuf.T;
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN =
  BEGIN
    WITH udpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,udp_hdr_len),
         udp_header = VIEW(udpHeaderBuf,NewT) 
     DO
      RETURN udp_header.dport = client_port;
    END;
  END rpc_client_WC; 

PROCEDURE rpc_client_handler (
    packet: Mbuf.T;
    <*UNUSED*>
    curr: Mbuf.T; 
    <*UNUSED*>
    offset: CARDINAL):BOOLEAN =
  BEGIN
    (* IF timing THEN Spy.Enter(udpclienttimer); END; *)
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"UdpRpc_client_PA ");
    END;

    (* for timing only *)
    WITH stop = CPU.CycleCounter(),
         (* extract the rpc timing data *)
         (* XXX assumes that data is in one contiguous mbuf *)
         origpkt = Mbuf.Array(packet),
         pkt = SUBARRAY(origpkt^,udpip_hdr_len,BYTESIZE(Header)),
         rpc_data = VIEW(pkt,Header)
     DO
      IF iodebug THEN IO.Put("["); IO.PutInt(rpc_data.timer); IO.Put("]"); END;
      Spy.Hit(udptimer,rpc_data.timer,stop);
 
      (* NOT IN IX86_SPIN
      IF timing THEN 
        WITH timer = VIEW(packet.mh_hdr.mh_union[0],Word.T) DO
          Spy.Hit(clientdataflow,timer,stop); 
        END;
      END;
      *)
    END;

    (* IF timing THEN Spy.Exit(udpclienttimer); END; *)
    RETURN FALSE; (* TRUE if consuming packet *)
  END rpc_client_handler; 

PROCEDURE sendto(<* UNUSED *> arg: ThreadExtra.ArgT) : ThreadExtra.ResultT = 
  VAR
    ip		: IpPktFormat.Header;
    udp         : UdpPktFormat.Header;
    data	: Mbuf.T;
    size        : CARDINAL;
    numpkts     : CARDINAL;

  BEGIN
    LOOP
      data := NIL;
      Sema.P(sema);
      ip      := client.ip;
      udp     := client.udp;
      numpkts := client.numpkts;
      size    := client.size;

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.INFO,"UdpRpc_sendto numpkts = " &
          Fmt.Int(numpkts) & " size = " & 
          Fmt.Int(size) & "\n");
      END;

      FOR i:= 1 TO numpkts DO
        (* allocate outgoing mbuf and copy rpc timer into it *)
        data := getmbuf(udp.len);

        (* place timestamp into package *)
        WITH rpcpkt = Mbuf.Array(data),
             rpc = VIEW(rpcpkt^,Header) 
         DO
          rpc.timer := Word.And(CPU.CycleCounter(), 16_ffffffff);
          IF iodebug THEN IO.Put("{"); IO.PutInt(rpc.timer); IO.Put("}"); END;
        END;

        (* send packet *)
        UdpGen.PacketSend(ip,udp,data,FALSE);
        FOR j := 1 TO  burntime DO
        END;
      END;
    END;
  END sendto;

PROCEDURE clntudp_send(src, dst :TEXT;numpkts:CARDINAL;size:CARDINAL) = 
  CONST
    udp_hdr_len = BYTESIZE(UdpPktFormat.Header);
  VAR
    ipaddr: IpPktFormat.Address;
  BEGIN

    WITH ip = client.ip DO
      TRY
        ipaddr := NetDb.GetHostByName(src);
        ip.saddr := ipaddr;
      EXCEPT
      | NetDb.HostNotFound =>
        IO.Put("Couldn't resolve " & src & "\n");
      END;
      TRY
        ipaddr := NetDb.GetHostByName(dst);
        ip.daddr := ipaddr;
      EXCEPT
      | NetDb.HostNotFound =>
        IO.Put("Couldn't resolve " & src & "\n");
      END;
      (* XXX need an ip header template generator *)
      ip.tos := 0;
      ip.tot_len := 0;
      ip.id  := 0;
      ip.frag_off := 0;
      ip.ttl := 16_ff;
      ip.protocol := IpPktFormat.IPPROTO_UDP;
    END;

    WITH udp = client.udp DO
      udp.sport := client_port;
      udp.dport := server_port;
      udp.len   := udp_hdr_len + BYTESIZE(Header) + size;
    END;

    client.size := size;
    client.numpkts := numpkts;
    Sema.V(sema);
  END clntudp_send; 

VAR
  spindles     : ARRAY [1..100] OF REFANY;
  one_spindle  : REFANY := NIL;
  currspindle  : CARDINAL := 0;
  all_or_none  : BOOLEAN := FALSE;
  one          : BOOLEAN := FALSE;
  firefirefire : BOOLEAN := FALSE;
  port         := Net.htons(0);

VAR
  all_or_none_count : CARDINAL := 0;  

PROCEDURE all_or_none_guard(
    <*UNUSED*> packet : Mbuf.T;
    curr              : Mbuf.T; 
    offset            : CARDINAL):BOOLEAN =
  BEGIN
    all_or_none_count := all_or_none_count + 1;
    WITH udpBuf = SUBARRAY(Mbuf.Array(curr)^,offset,BYTESIZE(NewT)),
         udp_header = VIEW(udpBuf,NewT) 
     DO
      RETURN udp_header.dport = port;
    END;
  END all_or_none_guard;

PROCEDURE one_guard(
    <*UNUSED*> packet : Mbuf.T;
    curr              : Mbuf.T; 
    offset            : CARDINAL):BOOLEAN =
  BEGIN
    WITH udpBuf = SUBARRAY(Mbuf.Array(curr)^,offset,BYTESIZE(NewT)),
         udp_header = VIEW(udpBuf,NewT) 
     DO
      RETURN udp_header.dport = port;
    END;
  END one_guard;

PROCEDURE empty(
    <*UNUSED*> packet : Mbuf.T;
    <*UNUSED*> curr   : Mbuf.T; 
    <*UNUSED*> offset : CARDINAL):BOOLEAN =
  BEGIN
    RETURN firefirefire;
  END empty; 

PROCEDURE pa(
    <*UNUSED*> packet : Mbuf.T;
    <*UNUSED*> curr   : Mbuf.T; 
    <*UNUSED*> offset : CARDINAL):BOOLEAN =
  BEGIN
    RETURN FALSE;
  END pa;

PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
  IF timing THEN
    udptimer       := Spy.Create("RPC: roundtrip");
    clientdataflow := Spy.Create("RPC: Ip -> RPC");
    serverdataflow := Spy.Create("RPC: Ip -> RPC");
    udpservertimer := Spy.Create("RPC: server_input");
    udpclienttimer := Spy.Create("RPC: client_input");
    mbuftimer := Spy.Create("RPC: getmbuf");
  END;

  sema := Sema.Alloc(0);

  (* default client and server ports *)
  client_port := Net.htons(clientRpcPort);
  server_port := Net.htons(serverRpcPort);
  EVAL ThreadExtra.PFork(sendto, NIL (* , "sendto" *)); 
  IF verbose THEN IO.Put ("UdpRpc module initialized.\n"); END;
  END Init;

BEGIN
END UdpRpc. 

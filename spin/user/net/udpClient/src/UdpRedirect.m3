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
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted to use new spin shell commands.
 *	Converted obsolete Clib interface to IO.
 *
 * 03-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Implements various UDP test programs that reroute UDP packets
 *	between a pair of hosts, or between a pair of ports.
 *      This module forwards udp packets between chiffon and velvet.  	
 *
 * 02-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *	
 *)

(* Untrusted *) 
MODULE UdpRedirect;
IMPORT Udp, UdpPktFormat, UdpGen;
IMPORT Net, Mbuf;
IMPORT IpPktFormat;
IMPORT NetDb;
IMPORT IO,Fmt;
IMPORT Spy;
(* IMPORT SAL; *)

IMPORT ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ipdefault *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-server") THEN
        server := NetDb.GetHostByName(pp.getNext());
      ELSIF pp.testNext("-client") THEN
        client := NetDb.GetHostByName(pp.getNext());
      ELSIF pp.testNext("-sport") THEN
        serverport := Net.htons(pp.getNextInt());
      ELSIF pp.testNext("-mountdport") THEN
        mountdport := Net.htons(pp.getNextInt());
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
  server        := NetDb.GetHostByName("silk");
  client        := NetDb.GetHostByName("denim");
  serverport    := Net.htons(2049);
  mountdport    := Net.htons(1055);
  rpcportmapper := Net.htons(111);

  server_timer  : Spy.T;
  server_udp    : REFANY;
  client_timer  : Spy.T;
  client_udp    : REFANY;
  debug_level   : Net.Level := Net.oLevel.NODEBUG;

CONST
  ip_hdr_len     = BYTESIZE(IpPktFormat.Header);
  udp_hdr_len    = BYTESIZE(UdpPktFormat.Header);
  ip_udp_hdr_len = ip_hdr_len + udp_hdr_len;

FUNCTIONAL
PROCEDURE server_wc(<*UNUSED*>packet: Mbuf.T; curr: Mbuf.T; 
                    offset: CARDINAL): BOOLEAN =
  BEGIN
    RETURN FALSE;
    (* XXXX FIX WHEN VIEW IS FIXED  *)
    WITH udpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,udp_hdr_len),
         udp_header = VIEW(udpHeaderBuf,NewT) 
     DO 
      RETURN udp_header.dport = serverport OR 
             udp_header.dport = mountdport OR
             udp_header.dport = rpcportmapper;
    END;
  END server_wc; 

PROCEDURE server_pa(packet: Mbuf.T; curr: Mbuf.T; offset: CARDINAL): BOOLEAN =
  VAR
    ip		: IpPktFormat.Header;
    udp         : UdpPktFormat.Header;
    data	: Mbuf.T;
    len         : CARDINAL;
  BEGIN

    Spy.Enter(server_timer);

    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"udp_redirect_PA ");
    END;

    (* get the ip address to send the packet back to *)
    WITH origpkt = Mbuf.Array(packet) DO 
      WITH ip_header = VIEW(origpkt^,IpPktFormat.T) DO
        (* flip ip address *)
        ip := ip_header;
        ip.saddr := ip.daddr;
        ip.daddr := server; (* ip_header.saddr; *) (* already in network order *)
        ip.protocol := IpPktFormat.IPPROTO_UDP;
        ip.tot_len := Net.nstoh(ip_header.tot_len);

        (* don't set remaining ip fields --- they are either ignored 
           or reset by the ip layer.
        *)
      END;

      (* #ifdef debug_level != NODEBUG *)
      IF debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.DEBUG,"udp_redirect_PA setting up udp header");
      END;


      WITH pkt = SUBARRAY(origpkt^,ip_hdr_len,BYTESIZE(NewT)),
           udp_header = VIEW(pkt,NewT) 
       DO 
        udp.sport := udp_header.sport;
        udp.dport := udp_header.dport;
        udp.len   := Net.nstoh(udp_header.len);
      END;
    END;

      (* #ifdef debug_level != NODEBUG *)
      IF debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.DEBUG,"udp_redirect_PA computing length of data");
      END;

    (* compute the length of the udp packet data *)
    len := Mbuf.m_length(packet);
    DEC(len,ip_udp_hdr_len);

      (* #ifdef debug_level != NODEBUG *)
      IF debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.DEBUG,"udp_redirect_PA copying data");
      END;

    (* copy data into its own mbuf chain *)
    data := Mbuf.m_copym(packet,ip_udp_hdr_len,len,Mbuf.M_WAIT);
    
    Spy.Exit(server_timer);


      (* #ifdef debug_level != NODEBUG *)
      IF debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.DEBUG,"udp_redirect_PA sending udp packet");
      END;

    (* send it over IP -- needs to become a event raise rather than a proc call *)
    UdpGen.PacketSend(ip,udp,data);

    RETURN FALSE; (* return true if consuming packet *)
  END server_pa; 

FUNCTIONAL
PROCEDURE client_wc(packet: Mbuf.T; <*UNUSED*>curr: Mbuf.T; 
                    <*UNUSED*>offset: CARDINAL): BOOLEAN =
  BEGIN
    RETURN FALSE; (* XXX Fix when view works again *)
(*
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.DEBUG,"udp_client_redirect_WC ");
    END;
*)
    WITH origpkt = Mbuf.Array(packet),
         ip_header = VIEW(origpkt^,IpPktFormat.T)
     DO
(*
      IF debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.DEBUG,"udp_client_redirect_WC ip address = [fix output]" (*&
          Fmt.Unsigned(ip_header.saddr) & " ? " & Fmt.Unsigned(server) & " "*));
      END;
*)
      RETURN ip_header.saddr = server;
    END;
  END client_wc; 

PROCEDURE client_pa(packet: Mbuf.T; <*UNUSED*>curr: Mbuf.T; 
                    <*UNUSED*>offset: CARDINAL): BOOLEAN =
  CONST
    ip_hdr_len     = BYTESIZE(IpPktFormat.Header);
    udp_hdr_len    = BYTESIZE(UdpPktFormat.Header);
    ip_udp_hdr_len = ip_hdr_len + udp_hdr_len;
  VAR
    ip		: IpPktFormat.Header;
    udp         : UdpPktFormat.Header;
    data	: Mbuf.T;
    len         : CARDINAL;
  BEGIN

    Spy.Enter(client_timer);

    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"udp_client_redirect_PA ");
    END;

    (* get the ip address to send the packet back to *)
    WITH origpkt = Mbuf.Array(packet) DO 
      WITH ip_header = VIEW(origpkt^,IpPktFormat.T) DO 
        (* flip ip address *)
        ip := ip_header;
        ip.saddr := ip.daddr;
        ip.daddr := client; (* ip_header.saddr; *) (* already in network order *)
        ip.protocol := IpPktFormat.IPPROTO_UDP;
        ip.tot_len := Net.nstoh(ip_header.tot_len);

        (* don't set remaining ip fields --- they are either ignored 
           or reset by the ip layer.
        *)
      END;

      WITH pkt = SUBARRAY(origpkt^,ip_hdr_len,BYTESIZE(NewT)),
           udp_header = VIEW(pkt,NewT)
       DO 
        udp.sport := udp_header.sport;
        udp.dport := udp_header.dport;
        udp.len   := Net.nstoh(udp_header.len);
      END;
    END;

    (* compute the length of the udp packet data *)
    len := Mbuf.m_length(packet);
    DEC(len,ip_udp_hdr_len);

    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.DEBUG,"udp_client_redirect_PA <" & 
        Fmt.Int(NUMBER(Mbuf.Array(packet)^)) & " " & 
        Fmt.Int(len) & " " & 
        Fmt.Int(ip_udp_hdr_len) & ">");
    END;

    (* copy data into its own mbuf chain *)
    data := Mbuf.m_copym(packet,ip_udp_hdr_len,len,Mbuf.M_WAIT);

    IF debug_level # Net.oLevel.NODEBUG THEN
    WITH outputpacket = Mbuf.Array(packet) DO
      FOR i := FIRST(outputpacket^) TO FIRST(outputpacket^) + 40 DO 
        IO.Put(Fmt.Unsigned(ORD(outputpacket[i])) & " ");
      END;
    END;
    IO.Put("\n");
    END;
    
    Spy.Exit(client_timer);


    (* send it over IP -- needs to become a event raise rather than a proc call *)
    UdpGen.PacketSend(ip,udp,data);

    RETURN FALSE; (* return true if consuming packet *)
  END client_pa; 

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    (* install handlers *)
    client_udp := Udp.Install(Udp.PacketArrived,client_wc,client_pa);
    client_timer := Spy.Create("client udp redirect");

    (* install handlers *)
    server_udp := Udp.Install(Udp.PacketArrived,server_wc,server_pa);
    server_timer := Spy.Create("server udp redirect");

    IF verbose THEN IO.Put("UdpRedirect module initialized\n"); END;

  END Init;

BEGIN
END UdpRedirect. 

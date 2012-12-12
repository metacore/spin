(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 *)

MODULE TcpHTTP;

IMPORT IO;
IMPORT Tcp;
IMPORT TcpPktFormat;
IMPORT TcpGen;
IMPORT IpPktFormat;
IMPORT Net;
IMPORT Mbuf;
IMPORT NetDb;

VAR
  redirect: REFANY;
  httpport:= Net.htons(80);
  debug_level: Net.Level := Net.oLevel.INFO;
  www        := NetDb.GetHostByName("www");

FUNCTIONAL
PROCEDURE Http_WC(READONLY packet: Mbuf.T; READONLY payload: Tcp.T):BOOLEAN =
  BEGIN
    (* WITH tcph = VIEW(payload,TcpPktFormat.T) DO *)
    WITH tcph = VIEW(payload^,TcpPktFormat.NewT) DO
      RETURN tcph.dport = httpport;
    END;
  END Http_WC;

PROCEDURE Http_PA(READONLY packet: Mbuf.T; READONLY payload: Tcp.T):BOOLEAN = 
  CONST
    ip_hdr_len     = BYTESIZE(IpPktFormat.Header);
    tcp_hdr_len    = BYTESIZE(TcpPktFormat.Header);
    ip_tcp_hdr_len = ip_hdr_len + tcp_hdr_len;
  VAR
    ip		: IpPktFormat.Header;
    tcp         : TcpPktFormat.Header;
    data	: Mbuf.T;
    len         : CARDINAL;
  BEGIN

    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"TcpHttp_PA ");
    END;


    (* get the ip address to send the packet back to *)
    WITH origpkt = Mbuf.Array(packet) DO 
      (* WITH ip_header = VIEW(ippkt[0],IpPktFormat.T) DO *)
      WITH ip_header = VIEW(origpkt^,IpPktFormat.NewT) DO
        (* flip ip address *)
        ip.saddr := ip_header.daddr; (* already in network order *)
        ip.daddr := www; (* ip_header.saddr; *) (* already in network order *)
        ip.ttl   := ip_header.ttl;
        ip.id    := ip_header.id;  (* send back same ip id field *)
        ip.tos   := ip_header.tos;
        ip.protocol := IpPktFormat.IPPROTO_TCP;
        ip.tot_len := Net.nstoh(ip_header.tot_len);

        (* don't set remaining ip fields --- they are either ignored 
           or reset by the ip layer.
        *)
      END;

      WITH pkt = SUBARRAY(origpkt^,ip_hdr_len,BYTESIZE(TcpPktFormat.NewT)),
           tcp_header = VIEW(pkt,TcpPktFormat.NewT)
       DO 
        tcp.sport := tcp_header.sport;
        tcp.dport := tcp_header.dport;
        tcp.seq   := tcp_header.seq;
        tcp.ack_seq := tcp_header.ack_seq;
        tcp.x2 := tcp_header.x2;
        tcp.xoff := tcp_header.xoff;
        tcp.flags := tcp_header.flags;
        tcp.window := tcp_header.window;
        tcp.urg_ptr := tcp_header.urg_ptr;

      END;
    END;

    (* compute the length of the tcp packet data *)
    len := Mbuf.total_length(packet);
    DEC(len,ip_tcp_hdr_len);
    (* copy data into its own mbuf chain *)
    data := Mbuf.m_copym(packet,ip_tcp_hdr_len,len,Mbuf.M_WAIT);
    
    (* send it over IP -- needs to become a event raise rather than a proc call *)
    TcpGen.PacketSend(ip,tcp,data);

    RETURN FALSE; (* return true if consuming packet *)
  END Http_PA;

PROCEDURE Init() = 
  BEGIN
    (* install handlers *)
    redirect := Tcp.Install(Tcp.PacketArrived,Http_WC, Http_PA);
    IO.Put("TcpHTTP() tcp http redirector installed\n");
  END Init;

BEGIN
END TcpHTTP.

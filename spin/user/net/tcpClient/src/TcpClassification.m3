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
 *	Converted to new spin shell command style.
 *      Converted from obsolete Clib to IO interface.
 *
 * 03-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Implements various TCP test programs that reroute TCP packets
 *	between a pair of hosts, or between a pair of ports.
 *      This module forwards tcp packets between chiffon and velvet.  	
 *
 * 02-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *	
 *)

(* Untrusted *) 
MODULE TcpClassification;
IMPORT Tcp, TcpPktFormat, TcpGen;
IMPORT Net, Mbuf;
IMPORT IpPktFormat;
IMPORT NetDb;
IMPORT IO;
IMPORT Ctypes;

IMPORT ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* tcpclassification *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-portredirect") THEN
        EVAL Redirect(NetDb.GetHostByName(pp.getNext()),
              NetDb.GetHostByName(pp.getNext()),
              pp.getNextInt(),FALSE);
      ELSIF pp.testNext("-hostredirect") THEN
        EVAL Redirect(NetDb.GetHostByName(pp.getNext()),
              NetDb.GetHostByName(pp.getNext()),0,TRUE);
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

TYPE redirect = RECORD
  ipsaddr: ALIGNED 32 FOR IpPktFormat.Address;
  ipdaddr: ALIGNED 32 FOR IpPktFormat.Address;
  port : Ctypes.unsigned_short;
END;

VAR
  debug_level : Net.Level := Net.oLevel.NODEBUG;

PROCEDURE Redirect(ipsaddr,ipdaddr: IpPktFormat.Address; port:Ctypes.unsigned_short; hostonly:BOOLEAN): REFANY =
  VAR closure: REF redirect;
  BEGIN
    closure := NEW(REF redirect);
    closure.ipsaddr := ipsaddr; (* assuming already in network order *)
    closure.ipdaddr := ipdaddr; (* assuming already in network order *)
    closure.port := Net.htons(port); (* not assumed in network order :( *)
    IF hostonly THEN 
      RETURN Tcp.InstallWithClosure(Tcp.PacketArrived,host_redirect_WC,redirect_PA,closure,closure);
    ELSE
      RETURN Tcp.InstallWithClosure(Tcp.PacketArrived,port_redirect_WC,redirect_PA,closure,closure);
    END;
  END Redirect;

FUNCTIONAL
PROCEDURE host_redirect_WC (closure: REFANY; packet, curr: Mbuf.T; offset: CARDINAL):BOOLEAN = 
  BEGIN
    WITH (* get the ip address to send the packet back to *)
         origpkt = Mbuf.Array(curr),
         ip_header = VIEW(origpkt^,IpPktFormat.T)
     DO
          RETURN ip_header.saddr = NARROW(closure,REF redirect).ipsaddr;
    END;
  END host_redirect_WC; 

FUNCTIONAL
PROCEDURE port_redirect_WC (closure: REFANY; packet, curr: Mbuf.T; offset: CARDINAL):BOOLEAN = 
  CONST
    tcp_hdr_len    = BYTESIZE(TcpPktFormat.Header);
  BEGIN
    WITH tcpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,tcp_hdr_len),
         tcpHeader = VIEW(tcpHeaderBuf,TcpPktFormat.NewT),
         (* get the ip address to send the packet back to *)
         origpkt = Mbuf.Array(packet),
         ipHeader = VIEW(origpkt^,IpPktFormat.T),
	 cl = NARROW(closure,REF redirect)
     DO
          RETURN ipHeader.saddr = cl.ipsaddr AND tcpHeader.dport = cl.port;
    END;
  END port_redirect_WC; 

PROCEDURE redirect_PA (closure: REFANY; packet, curr: Mbuf.T; offset: CARDINAL):BOOLEAN =
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
      Net.Debug(debug_level, Net.oLevel.INFO,"redirect_PA ");
    END;

    (* get the ip address to send the packet back to *)
    WITH origpkt = Mbuf.Array(packet) DO 
      WITH ip_header = VIEW(origpkt^,IpPktFormat.T) DO
        (* flip ip address *)
        ip := ip_header;
        ip.saddr := ip_header.daddr; (* already in network order *)
        ip.daddr := NARROW(closure,REF redirect).ipdaddr; (* ip_header.saddr; *) (* already in network order *)
        ip.protocol := IpPktFormat.IPPROTO_TCP;
        ip.tot_len := Net.nstoh(ip_header.tot_len);
        (* don't set remaining ip fields --- they are either ignored 
           or reset by the ip layer.
        *)
      END;

      WITH tcp_header = VIEW(origpkt^,TcpPktFormat.NewT) DO 
        tcp := tcp_header;
      END;
    END;

    (* compute the length of the tcp packet data *)
    len := Mbuf.m_length(packet);
    DEC(len,ip_tcp_hdr_len);

    (* copy data into its own mbuf chain *)
    data := Mbuf.m_copym(packet,ip_tcp_hdr_len,len,Mbuf.M_WAIT);
    (* send it over IP -- needs to become a event raise rather than a proc call *)
    TcpGen.PacketSend(ip,tcp,data);
    RETURN FALSE; (* return true if consuming packet *)
  END redirect_PA; 

PROCEDURE Init(<* UNUSED *> verbose:BOOLEAN) = 
  BEGIN
  END Init;

BEGIN
END TcpClassification. 

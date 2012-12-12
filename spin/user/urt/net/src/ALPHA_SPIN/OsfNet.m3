(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 02-June-97  Tsutomu Owa (owa) at the University of Washington
 *	Added Pffindproto and dummy IcmpError.
 *
 * 31-May-97  David Becker at the University of Washington
 *      Replace ifp net interface with NetDev objects
 *      Add RegisterInterface to keep bsd net code informed of interfaces
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made Spy-s untraced to be able to measure the collector.
 *
 * 10-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Using SPL to synchronize with other threads, interrupt handlers,
 *	and timers. TEMPORARY: we need to understand how to use
 *	NETSYNC_LOCK without UNIX_LOCKS, to turn on synchronization in
 *	tcp networking code. 
 *
 * 04-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initializes the tcp module and acts as a bridge from SPIN M3 code
 *	to OSF/1 C implementation of TCP/IP.
 *	Created.
 *
 *)

UNSAFE (* to import interface with EXTERNAL symbols *)
MODULE OsfNet;
FROM Ctypes IMPORT int, unsigned_short, char_star, unsigned_int, unsigned_char;

IMPORT OsfNetExtern, OsfNet, Mbuf, IO, Clock, ULockForSAL, SocketRep,
       SocketAddrIn, Protosw, Word, M3toC, Spy (*, StrongRef *);
IMPORT Fmt;
IMPORT NetDev;

<*UNUSED*>
CONST 
  debug = FALSE;
  timing = TRUE;

(* add ifp to in_ifaddr list for lookups by BSD udp and tcp *)
PROCEDURE RegisterInterface(dev: NetDev.T; src: SocketAddrIn.in_addrT) =
  BEGIN
    OsfNetExtern.addifp(dev.bsdIfp(),src);
  END RegisterInterface;

PROCEDURE AddMultiAddr(dev: NetDev.T; VAR multiaddr: ARRAY[0..5] OF unsigned_char) =
  BEGIN
    OsfNetExtern.addmultiaddr(dev.bsdIfp(),multiaddr);
  END AddMultiAddr;

PROCEDURE SetTcpMss(func: tcp_mss_func) =
  BEGIN
    OsfNetExtern.tcp_URT_mss := func;
  END SetTcpMss;

PROCEDURE GetTcpStats(VAR tcpstat: tcpstatT) = 
  BEGIN
    tcpstat := OsfNetExtern.tcpstat;
  END GetTcpStats;

PROCEDURE SetUdpIpOutputUpcall(
  ipOutput: PROCEDURE(READONLY m: Mbuf.T;rt: REFANY)) = 
  BEGIN
    OsfNetExtern.udpIpOutput := ipOutput;
  END SetUdpIpOutputUpcall;

PROCEDURE SetTcpIpOutputUpcall(
  ipOutput: PROCEDURE(READONLY m: Mbuf.T; rt: REFANY)) = 
  BEGIN
    OsfNetExtern.tcpIpOutput := ipOutput;
  END SetTcpIpOutputUpcall;

PROCEDURE TcpInput(m:Mbuf.T; iphlen: CARDINAL) = 
  BEGIN 
    OsfNetExtern.tcp_input(m,iphlen); 
  END TcpInput;

PROCEDURE UdpInput(m:Mbuf.T; iphlen: CARDINAL) = 
  BEGIN 
    OsfNetExtern.udp_input(m,iphlen); 
  END UdpInput;

PROCEDURE AddTcpPortHandler(<* UNUSED *> port: unsigned_short) =
  BEGIN
    (* event to be handled by Plexus *)
  END AddTcpPortHandler;

PROCEDURE RaiseAddTcpPortHandler(port: unsigned_short) =
  BEGIN
    OsfNet.AddTcpPortHandler(port);
  END RaiseAddTcpPortHandler;

PROCEDURE DelTcpPortHandler(<* UNUSED *> port: unsigned_short) = 
  BEGIN
    (* event to be handled by Plexus *)
  END DelTcpPortHandler;

PROCEDURE RaiseDelTcpPortHandler(port: unsigned_short) =
  BEGIN
    OsfNet.DelTcpPortHandler(port);
  END RaiseDelTcpPortHandler;

PROCEDURE AddUdpPortHandler(<* UNUSED *> port: unsigned_short) =
  BEGIN
    (* event to be handled by Plexus *)
  END AddUdpPortHandler;

PROCEDURE RaiseAddUdpPortHandler(port: unsigned_short) =
  BEGIN
    OsfNet.AddUdpPortHandler(port);
  END RaiseAddUdpPortHandler;

PROCEDURE DelUdpPortHandler(<* UNUSED *> port: unsigned_short) = 
  BEGIN
    (* event to be handled by Plexus *)
  END DelUdpPortHandler;

PROCEDURE RaiseDelUdpPortHandler(port: unsigned_short) =
  BEGIN
    OsfNet.DelUdpPortHandler(port);
  END RaiseDelUdpPortHandler;

PROCEDURE UdpCtlInput(
    code: unsigned_short;
    VAR sin: SocketAddrIn.T;
    ip: ADDRESS) =
  BEGIN
    OsfNetExtern.udp_ctlinput(code+Protosw.PRC_UNREACH_NET,sin,ip);
  END UdpCtlInput; 

(*
  TCP protocol fast timeout routine called every 200 ms for processing
  delayed acks.
*)
PROCEDURE TcpFastTimo(<* UNUSED *> arg: REFANY) = 
  BEGIN
    OsfNetExtern.tcp_fasttimo();
    Clock.SetAlarm(1024 DIV 5 (*ms*), TcpFastTimo, NIL);
  END TcpFastTimo;

(*
  TCP protocol slow timeout routine called every 500 ms.
*)
PROCEDURE TcpSlowTimo(<* UNUSED *> arg: REFANY) = 
  BEGIN
    OsfNetExtern.tcp_slowtimo();
    Clock.SetAlarm(1024 DIV 2 (*ms*), TcpSlowTimo, NIL);
  END TcpSlowTimo; 

PROCEDURE Pffindtype(family,type:CARDINAL): UNTRACED REF Protosw.T =
  BEGIN
    (* IO.Put("Pffindtype called.\n"); *)
    FOR i := FIRST(inetsw^) TO LAST(inetsw^) DO
      IF inetsw[i].pr_type = type AND 
        inetsw[i].pr_domain.dom_family = family THEN
        RETURN LOOPHOLE(ADR(inetsw[i]),UNTRACED REF Protosw.T);
      END;
    END;
    RETURN NIL;
  END Pffindtype;

PROCEDURE Pffindproto(family,protocol,type:CARDINAL): UNTRACED REF Protosw.T =
  BEGIN
    (* IO.Put("Pffindtype called.\n"); *)
    FOR i := FIRST(inetsw^) TO LAST(inetsw^) DO
      IF inetsw[i].pr_type = type AND 
	inetsw[i].pr_protocol = protocol AND
        inetsw[i].pr_domain.dom_family = family THEN
	(* 
	IO.Put("Pffindproto returns inetsw[" & Fmt.Int(i) & "]\n");
	*)
        RETURN LOOPHOLE(ADR(inetsw[i]),UNTRACED REF Protosw.T);
      END;
    END;
    (* XXX Should not reach here *)
    IO.Put("OsfNet.m3:Pffindproto(). Unknown protocol\n");
    IO.Put("  family " & Fmt.Int(family) & ", protocol "
	   & Fmt.Int(protocol) & ", type " & Fmt.Int(type) & "\n");
    RETURN NIL;
  END Pffindproto;


PROCEDURE IcmpError(m: Mbuf.T; i1, i2: int; ia0: unsigned_int) =
  BEGIN
  (*
  IO.Put("OsfNet::IcmpError is called\n");
  *)

  END IcmpError;

PROCEDURE SpyCreate1(text: TEXT): Spy.T = 
  VAR
    spy: Spy.T;
  BEGIN
    spy := Spy.Create(text);
    (* StrongRef.Add(spy);*)
    RETURN spy;
  END SpyCreate1;

PROCEDURE SpyCreate(name: char_star): Spy.T  = 
  VAR
    text: TEXT;
  BEGIN
    IF name = NIL THEN
      text := "anonymous";
    ELSE
      text := M3toC.CopyStoT(name);
    END;
    RETURN SpyCreate1(text);
  END SpyCreate;

PROCEDURE SpyEnter(spy: Spy.T) = 
  BEGIN
    Spy.Enter(spy);
  END SpyEnter;

PROCEDURE SpyExit(spy: Spy.T) = 
  BEGIN
    Spy.Exit(spy);
  END SpyExit;


VAR inetsw : UNTRACED REF ARRAY OF Protosw.T;
VAR inetdomain: UNTRACED REF Protosw.domainT;
  
PROCEDURE Init(verbose:BOOLEAN) = 
  CONST 
    IPPROTO_TCP = 6;  (* tcp - netinet/in.h*)
    IPPROTO_UDP = 17; (* tcp - netinet/in.h*)
    NUMPROTO    = 2;
  BEGIN

    OsfNetExtern.SpyCreate_upcall := SpyCreate;
    OsfNetExtern.SpyEnter_upcall := SpyEnter;
    OsfNetExtern.SpyExit_upcall := SpyExit;

    OsfNetExtern.tcp_output_checksum := SpyCreate1("TcpOutputChecksum");
    OsfNetExtern.tcp_output_presend := SpyCreate1("TcpOutputPresend");
    OsfNetExtern.tcp_output_send := SpyCreate1("TcpOutputSend");
    OsfNetExtern.tcp_input_checksum := SpyCreate1("TcpInputChecksum");

    (* optimize Mbuf.M_PREPEND() for ip *)
    OsfNetExtern.max_linkhdr := 20;

    IF verbose THEN IO.Put("\tOsfNet initializing inetsw.\n"); END;

    inetdomain := NEW(UNTRACED REF Protosw.domainT);
    inetsw := NEW(UNTRACED REF ARRAY OF Protosw.T, NUMPROTO);

    WITH i = inetsw[0] DO
      i.pr_type := SocketRep.SOCK_STREAM;
      i.pr_domain := inetdomain;
      i.pr_protocol := IPPROTO_TCP;
      i.pr_flags:= Word.Or(Protosw.PR_CONNREQUIRED,Protosw.PR_WANTRCVD);
      i.pr_input:= LOOPHOLE(OsfNetExtern.tcp_input,PROCEDURE());
      i.pr_output:= LOOPHOLE(NIL,PROCEDURE():int);
      i.pr_ctlinput:= LOOPHOLE(OsfNetExtern.tcp_ctlinput,PROCEDURE());
      i.pr_ctloutput:= LOOPHOLE(OsfNetExtern.tcp_ctloutput,PROCEDURE():int);
      i.pr_usrreq:= LOOPHOLE(OsfNetExtern.tcp_usrreq,
                             PROCEDURE(so:ADDRESS; req: int; m,name,control:Mbuf.T):int);
      i.pr_init:= LOOPHOLE(OsfNetExtern.tcp_init,PROCEDURE());
      i.pr_fasttimo:= LOOPHOLE(OsfNetExtern.tcp_fasttimo,PROCEDURE());
      i.pr_slowtimo:= LOOPHOLE(OsfNetExtern.tcp_slowtimo,PROCEDURE());
      i.pr_drain:= LOOPHOLE(OsfNetExtern.tcp_drain,PROCEDURE());
    END;

    WITH i = inetsw[1] DO
      i.pr_type := SocketRep.SOCK_DGRAM;
      i.pr_domain := inetdomain;
      i.pr_protocol := IPPROTO_UDP;
      i.pr_flags:= Word.Or(Protosw.PR_ATOMIC,Protosw.PR_ADDR);
      i.pr_input:= LOOPHOLE(OsfNetExtern.udp_input,PROCEDURE());
      i.pr_output:= NIL;
      i.pr_ctlinput:= LOOPHOLE(OsfNetExtern.udp_ctlinput,PROCEDURE());
      i.pr_ctloutput:= LOOPHOLE(OsfNetExtern.udp_ctloutput,PROCEDURE():int);
      i.pr_usrreq:= LOOPHOLE(OsfNetExtern.udp_usrreq,
                             PROCEDURE(so:ADDRESS; req: int; m,name,control:Mbuf.T):int);
      i.pr_init:= LOOPHOLE(OsfNetExtern.udp_init,PROCEDURE());
      i.pr_fasttimo:= NIL;
      i.pr_slowtimo:= NIL;
      i.pr_drain:= NIL;
    END;

    IF verbose THEN IO.Put("\tOsfNet initializing inetdomain.\n"); END;
    inetdomain.dom_family          := SocketRep.AF_INET;
    inetdomain.dom_name            := M3toC.CopyTtoS("internet");
    inetdomain.dom_init            := NIL;
    inetdomain.dom_externalize     := NIL;
    inetdomain.dom_dispose         := NIL;
    inetdomain.dom_protosw         := LOOPHOLE(ADR(inetsw[FIRST(inetsw^)]), UNTRACED REF Protosw.T);
    inetdomain.dom_protoswNPROTOSW := LOOPHOLE(ADR(inetsw[LAST(inetsw^)]), UNTRACED REF Protosw.T);
    inetdomain.dom_next            := NIL;
    inetdomain.dom_refcnt          := 0;
    inetdomain.dom_funnel          := NIL;
    inetdomain.dom_funcfrc         := NIL;

    OsfNetExtern.inetsw[0] := inetsw[0]; 
    OsfNetExtern.inetsw[1] := inetsw[1]; 

    IF verbose THEN IO.Put("\tOsfNet redirecting pffindtype().\n"); END;
    OsfNetExtern.pffindtype_upcall := Pffindtype;

    IF verbose THEN IO.Put("\tOsfNet redirecting pffindproto().\n"); END;
    OsfNetExtern.pffindproto_upcall := Pffindproto;

    IF verbose THEN IO.Put("\tOsfNet redirecting icmp_error().\n"); END;
    OsfNetExtern.icmp_error_upcall := IcmpError;
    
    IF verbose THEN IO.Put("\tOsfNet initializing networking locks.\n"); END;
    ULockForSAL.ulock_setup(OsfNetExtern.route_lock,NIL,TRUE);
    ULockForSAL.ulock_setup(OsfNetExtern.inifaddr_lock,NIL,TRUE);
    ULockForSAL.ulock_setup(OsfNetExtern.igmp_lock,NIL,TRUE);
    ULockForSAL.ulock_setup(OsfNetExtern.inp_udp_li,NIL,TRUE);

    
    IF verbose THEN IO.Put("\tOsfNet initializing tcp bind/unbind upcalls.\n"); END;
    OsfNetExtern.udp_URT_bind_upcall := RaiseAddUdpPortHandler;
    OsfNetExtern.udp_URT_unbind_upcall := RaiseDelUdpPortHandler;

    IF verbose THEN IO.Put("\tOsfNet initializing UNREACH_NET -> UNREACH_PORT inetctlerrmap.\n"); END;
    FOR i := Protosw.PRC_UNREACH_NET TO Protosw.PRC_UNREACH_PORT DO
      OsfNetExtern.inetctlerrmap[i] := 1; (* enable *)
    END;

    IF verbose THEN IO.Put("\tOsfNet initializing tcp bind/unbind upcalls.\n"); END;

    OsfNetExtern.tcp_URT_bind_upcall := RaiseAddTcpPortHandler;
    OsfNetExtern.tcp_URT_unbind_upcall := RaiseDelTcpPortHandler;

    IF verbose THEN IO.Put("\tOsfNet initializing mbuf_URT_csum upcall.\n"); END;
    OsfNetExtern.mbuf_URT_csum_upcall := Mbuf.Checksum;

    IF verbose THEN IO.Put("\tOsfNet initializing Digital Unix TCP.\n"); END;
    (* call the tcp internal initialization function *)
    OsfNetExtern.tcp_init();

    IF verbose THEN IO.Put("\tOsfNet setting up TCP timeouts.\n"); END;
    TcpFastTimo(NIL);
    TcpSlowTimo(NIL);

    IF verbose THEN IO.Put("\tOsfNet initializing Digital Unix UDP.\n"); END;
    (* call the tcp internal initialization function *)
    OsfNetExtern.udp_init();

    IF verbose THEN IO.Put("OsfNet module initialized.\n"); END;
  END Init;

BEGIN
END OsfNet.

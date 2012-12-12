(*
 * Copyright 1994-1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 24-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made Spy-s untraced to be able to measure the collector.
 *
 * 02-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support for spy timing.
 *
 * 03-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Interface to the OSF/1 3.x implementation of TCP/IP.
 *)

UNSAFE 
INTERFACE OsfNetExtern;
IMPORT Mbuf, Ctypes, Protosw, SocketAddrIn, Spy, OsfNet;

<* EXTERNAL "tcpip_output_upcall" *>
VAR tcpIpOutput: PROCEDURE(READONLY m: Mbuf.T; rt: REFANY);

<* EXTERNAL "udpip_output_upcall" *>
VAR udpIpOutput: PROCEDURE(READONLY m: Mbuf.T; rt: REFANY);

(* these are private *)
<* EXTERNAL *> PROCEDURE tcp_init(); (* tcp_subr.c *)
<* EXTERNAL *> PROCEDURE tcp_input(m:Mbuf.T; iphlen:CARDINAL); (* tcp_input.c *)
<* OBSOLETE *> <* EXTERNAL *> PROCEDURE tcp_output(tcpcb: ADDRESS): INTEGER; (* tcp_output.c *)
<* EXTERNAL *> PROCEDURE tcp_fasttimo(); (* tcp_timer.c *)
<* EXTERNAL *> PROCEDURE tcp_slowtimo(); (* tcp_timer.c *)

<* EXTERNAL *> PROCEDURE tcp_ctlinput();
<* EXTERNAL *> PROCEDURE tcp_ctloutput();
<* EXTERNAL *> PROCEDURE tcp_usrreq();
<* EXTERNAL *> PROCEDURE tcp_drain();

<* EXTERNAL *> PROCEDURE udp_init(); (* udp_subr.c *)
<* EXTERNAL *> PROCEDURE udp_input(m:Mbuf.T; iphlen:CARDINAL); (* udp_input.c *)
<* EXTERNAL *> PROCEDURE udp_output(udpcb: ADDRESS): INTEGER; (* udp_output.c *)
<* EXTERNAL *> PROCEDURE udp_ctlinput(cmd: Ctypes.unsigned_int; VAR sa: SocketAddrIn.T; ip: ADDRESS);
<* EXTERNAL *> PROCEDURE udp_ctloutput();
<* EXTERNAL *> PROCEDURE udp_usrreq();

<* EXTERNAL *> PROCEDURE addifp(ifp: ADDRESS; src: SocketAddrIn.in_addrT); (* net_upcalls.c *)

(* patchable parameters for tcp : defined in tcp_subr.c *)
<* EXTERNAL *> VAR tcp_ttl:Ctypes.int;
<* EXTERNAL *> VAR tcp_mssdflt:Ctypes.int;
<* EXTERNAL *> VAR tcp_rttdflt:Ctypes.int;
<* EXTERNAL *> VAR tcp_compat_42:Ctypes.int;
<* EXTERNAL *> VAR tcp_urgent_42:Ctypes.int;
<* EXTERNAL *> VAR tcp_dont_winscale:Ctypes.int;
<* EXTERNAL *> VAR tcp_rptr2dflt:Ctypes.int;
(* Connect R2 conforms to 1122 *)
<* EXTERNAL *> VAR tcp_connr2_conf:Ctypes.int;
(* Negative gives old behaviour if conformance not true *)
<* EXTERNAL *> VAR tcp_connr2_shift:Ctypes.int;

<* EXTERNAL *> VAR max_linkhdr : Ctypes.int; (* bsd/uipc_mbuf.c *)

(* These come from net_upcalls.c *)
<* EXTERNAL *> VAR inetctlerrmap: ARRAY [1..24] OF Ctypes.unsigned_char;
<* EXTERNAL *> VAR inetsw: ARRAY [0..1] OF Protosw.T;
<* EXTERNAL *> VAR pffindtype_upcall: PROCEDURE(family,type:CARDINAL): UNTRACED REF Protosw.T;
<* EXTERNAL *> VAR pffindproto_upcall: PROCEDURE(family,proto,type:CARDINAL): UNTRACED REF Protosw.T;

<* EXTERNAL *> VAR tcp_URT_bind_upcall: PROCEDURE(port: Ctypes.unsigned_short);
<* EXTERNAL *> VAR tcp_URT_unbind_upcall: PROCEDURE(port: Ctypes.unsigned_short);
<* EXTERNAL *> VAR udp_URT_bind_upcall: PROCEDURE(port: Ctypes.unsigned_short);
<* EXTERNAL *> VAR udp_URT_unbind_upcall: PROCEDURE(port: Ctypes.unsigned_short);

(* upcall variables for timing support *)
<* EXTERNAL *> VAR SpyCreate_upcall: PROCEDURE(name: Ctypes.char_star): Spy.T;
<* EXTERNAL *> VAR SpyEnter_upcall: PROCEDURE(timer: Spy.T);
<* EXTERNAL *> VAR SpyExit_upcall: PROCEDURE(timer: Spy.T);

<* EXTERNAL *> VAR tcp_output_checksum: ADDRESS;
<* EXTERNAL *> VAR tcp_output_presend: ADDRESS;
<* EXTERNAL *> VAR tcp_output_send: ADDRESS;
<* EXTERNAL *> VAR tcp_input_checksum: ADDRESS;

<* EXTERNAL *> VAR mbuf_URT_csum_upcall: 
                 PROCEDURE(
                     m:Mbuf.T; 
                     csum:Ctypes.unsigned_short; 
                     len: CARDINAL): Ctypes.unsigned_short;

<* EXTERNAL *> VAR tcp_URT_mss: OsfNet.tcp_mss_func;

<* EXTERNAL *> VAR tcpstat: OsfNet.tcpstatT;

END OsfNetExtern.

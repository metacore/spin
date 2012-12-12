(*
 * Copyright 1994-1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 04-Nov-97  Vinh Lam (vkl) at the University of Washington
 *	Added AF_INET6 family
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to be SAL independent.
 *
 * 03-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added inpcb type to access the protocol control block.
 *
 * 02-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Clean up of interface.
 *
 * 09-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Got rid of m3sockaddr hack.
 *
 * 12-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE SocketRep;
IMPORT SocketDep, SocketAddr, SocketAddrIn, Word;

CONST (* Address families. *)
  AF_UNSPEC      = 0;  (* unspecified                     *)
  AF_UNIX        = 1;  (* local to host (pipes, portals)  *)
  AF_INET        = 2;  (* internetwork: UDP, TCP, etc.    *)
  AF_IMPLINK     = 3;  (* arpanet imp addresses           *)
  AF_PUP         = 4;  (* pup protocols: e.g. BSP         *)
  AF_CHAOS       = 5;  (* mit CHAOS protocols             *)
  AF_NS          = 6;  (* XEROX NS protocols              *)
  AF_NBS         = 7;  (* nbs protocols                   *)
  AF_ECMA        = 8;  (* european computer manufacturers *)
  AF_DATAKIT     = 9;  (* datakit protocols               *)
  AF_CCITT       = 10; (* CCITT protocols, X.25 etc       *)
  AF_INET6       = 10; (* taken from linux socket.h       *)
  AF_SNA         = 11; (* IBM SNA                         *)
  AF_DECnet      = 12; (* DECnet                          *)
  AF_DLI         = 13; (* Direct data link interface      *)
  AF_LAT         = 14; (* LAT                             *)
  AF_HYLINK      = 15; (* NSC Hyperchannel                *)
  AF_APPLETALK   = 16; (* Apple talk                      *)
  AF_BSC         = 17; (* BISYNC 2780/3780                *)
  AF_DSS         = 18; (* Distributed system services     *)
  AF_MAX         = 19;

CONST (* Socket Types *)
  SOCK_STREAM    = 1;  (* stream socket                   *)
  SOCK_DGRAM     = 2;  (* datagram socket                 *)
  SOCK_RAW       = 3;  (* raw-protocol interface          *)
  SOCK_RDM       = 4;  (* reliably-delivered message      *)
  SOCK_SEQPACKET = 5;  (* sequenced packet stream         *)

CONST MSG_OOB       = 16_1;    (* process out-of-band data *)
CONST MSG_PEEK      = 16_2;    (* peek at incoming message *)
<* UNUSED *> CONST MSG_DONTROUTE = 16_4;    (* send without using routing tables *)
CONST      MSG_EOR       = 16_8;    (* data completes record *)
<* UNUSED *> CONST MSG_TRUNC     = 16_10;   (* data discarded before delivery *)
<* UNUSED *> CONST MSG_CTRUNC    = 16_20;   (* control data lost before delivery *)
<* UNUSED *> CONST MSG_WAITALL   = 16_40;   (* wait for full request or error *)
CONST MSG_NONBLOCK  = 16_4000; (* nonblocking request *)
<* UNUSED *> CONST MSG_COMPAT    = 16_8000; (* 4.3-format sockaddr *)

(* XXX The following types and constants have moved *)
<*OBSOLETE*>
CONST SALEN      = SocketAddr.SALEN;
<*OBSOLETE*>
TYPE sockaddr    = SocketAddr.T;
<*OBSOLETE*>
TYPE sockaddr_in = SocketAddrIn.T;
<*OBSOLETE*>
TYPE in_addrT    = SocketAddrIn.in_addrT;
<*OBSOLETE*>
TYPE in_portT    = SocketAddrIn.in_portT;
<*OBSOLETE*>
CONST SIN_ZERO = SocketAddrIn.SIN_ZERO;

(* Kernel structures per socket.  Contains send and receive buffer
  queues (sockbufT), handle on protocol and pointer to protocol
  private data and error information.  *)
TYPE socketT = SocketDep.socketT;
TYPE sockbufT = SocketDep.sockbufT;

CONST SB_MAX     = SocketDep.SB_MAX; (* default for max chars in sockbuf  *)
CONST SB_LOCK    = 16_01;      (* lock on data queue                *)
CONST SB_WANT    = 16_02;      (* someone is waiting to lock        *)
CONST SB_WAIT    = 16_04;      (* someone is waiting for data/space *)
CONST SB_SEL     = 16_08;      (* someone is selecting              *)
CONST SB_ASYNC   = 16_10;      (* ASYNC I/O, need signals           *)
CONST SB_NOTIFY  = Word.Or(SB_WAIT,Word.Or(SB_SEL,SB_ASYNC));
CONST SB_COLL    = 16_20;      (* collision selecting (UNIX)        *)
CONST SB_NOINTR  = 16_40;      (* operations not interruptible      *)
CONST SB_WAKEONE = 16_80;      (* wakeup only one on notify         *)
CONST SB_WAITING = 16_100;     (* MSG_WAITALL receive in progress   *)
CONST SB_INHERIT = Word.Or(SB_NOINTR,SB_WAKEONE);

(*
 * Socket state bits.
 *)
CONST SS_NOFDREF         = 16_001; (* no file table ref any more        *)
CONST SS_ISCONNECTED     = 16_002; (* socket connected to a peer        *)
CONST SS_ISCONNECTING    = 16_004; (* in process of connecting to peer  *)
CONST SS_ISDISCONNECTING = 16_008; (* in process of disconnecting       *)
CONST SS_CANTSENDMORE    = 16_010; (* can't send more data to peer      *)
CONST SS_CANTRCVMORE     = 16_020; (* can't receive more data from peer *)
CONST SS_RCVATMARK       = 16_040; (* at mark on input                  *)
CONST SS_PRIV            = 16_080; (* privileged for broadcast, raw...  *)
CONST SS_NBIO            = 16_100; (* non-blocking ops                  *)
CONST SS_ASYNC           = 16_200; (* async i/o notify                  *)
CONST SS_ISCONFIRMING    = 16_400; (* deciding to accept connection req *)

(*
 * Special socket state bits.
 *)
CONST SP_PIPE     = 16_0001;        (* socket is unnamed pipe (obsolete) *)
CONST SP_WATOMIC  = 16_0002;        (* pipe write atomicity (obsolete)   *)
CONST SP_NOUAREA  = 16_0004;        (* no u-area available (XTI - XXX)   *)
CONST SP_LOCKABLE = 16_0008;        (* locking is active for socket      *)
CONST SP_CLOSING  = 16_0010;        (* closing a listening socket        *)
CONST SP_RWAKEUP  = 16_0020;        (* sorwakeup pending release of lock *)
CONST SP_WWAKEUP  = 16_0040;        (* sowwakeup pending release of lock *)
CONST SP_EXTPRIV  = 16_0080;        (* SS_PRIV managed externally        *)
CONST SP_FREEABLE = 16_8000;        (* free socket on unlock             *)
CONST SP_INHERIT  = Word.Or(SP_PIPE,Word.Or(SP_WATOMIC,SP_LOCKABLE));

END SocketRep.

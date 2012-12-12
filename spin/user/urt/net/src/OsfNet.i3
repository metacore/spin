(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace ifp net interface with NetDev objects
 *      Add RegisterInterface to keep bsd net code informed of interfaces
 *
 * 15-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added SPIN specific field in the tcp protocol control block.
 *
 * 10-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added a type for the TCP protocol control block.
 *
 * 01-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to dynamically add and remove event handlers for
 *	udp/tcp ports.
 *
 * 01-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Both the udp and tcp osf clients can set their ip output
 *	functions via this interface.  We only borrow the C code for tcp
 *	and some for udp.  The latter can be trivially replaced.
 *
 * 04-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created. Provides an interface for the borrowed DEC OSF/1 TCP code.
 *)

INTERFACE OsfNet;
FROM Ctypes IMPORT unsigned_short, unsigned_int, unsigned_char,
                   short, int, char;
IMPORT Mbuf, SocketAddrIn, Net, NetDev;

(* Add interface to BSDs in_ifaddr, the global list of interfaces *)
PROCEDURE RegisterInterface(dev: NetDev.T; src: SocketAddrIn.in_addrT);

(* vkl added. *)
PROCEDURE AddMultiAddr(dev: NetDev.T; VAR multiaddr: ARRAY[0..5] OF unsigned_char);

(* Interface to Digital Unix tcp_input and udp_input. *)
PROCEDURE TcpInput(m:Mbuf.T; iphlen: CARDINAL);
PROCEDURE UdpInput(m:Mbuf.T; iphlen: CARDINAL);

(* port handler event type *)
TYPE PortHandlerT = PROCEDURE(port: unsigned_short);
(* Events to dynamically add/del ports *)
PROCEDURE AddTcpPortHandler(port: unsigned_short);
PROCEDURE DelTcpPortHandler(port: unsigned_short);

(* Events to dynamically add/del ports *)
PROCEDURE AddUdpPortHandler(port: unsigned_short);
PROCEDURE DelUdpPortHandler(port: unsigned_short);

(* XXX should be able to import IpPktFormat to get ip type *)
TYPE Address = ARRAY [0..3] OF CHAR;
TYPE MyIpHeader = RECORD
  hlen: Net.nible;
  vers: Net.nible;
  tos       : BITS 8 FOR unsigned_char;
  tot_len   : BITS 16 FOR unsigned_short;
  id        : BITS 16 FOR unsigned_short;
  frag_off  : BITS 16 FOR unsigned_short;
  ttl       : BITS 8 FOR unsigned_char;
  protocol  : BITS 8 FOR unsigned_char;
  check     : BITS 16 FOR unsigned_short;
  saddr     : Address;
  daddr     : Address;
END;

(* Support to set the ip_output procedure variables used by the
   Digital Unix networking code. *)

PROCEDURE SetUdpIpOutputUpcall(
  IpOutput: PROCEDURE(READONLY m: Mbuf.T;rt: REFANY));

PROCEDURE SetTcpIpOutputUpcall(
  IpOutput: PROCEDURE(READONLY m: Mbuf.T;rt: REFANY));

(* tcp protocol statistics *)
TYPE tcpstatT = RECORD
  tcps_connattempt     : unsigned_int;  (* u_int   tcps_connattempt;       /* connections initiated */              *)
  tcps_accepts         : unsigned_int;  (* u_int   tcps_accepts;           /* connections accepted */               *)
  tcps_connects        : unsigned_int;  (* u_int   tcps_connects;          /* connections established */            *)
  tcps_drops           : unsigned_int;  (* u_int   tcps_drops;             /* connections dropped */                *)
  tcps_conndrops       : unsigned_int;  (* u_int   tcps_conndrops;         /* embryonic connections dropped */      *)
  tcps_closed          : unsigned_int;  (* u_int   tcps_closed;            /* conn. closed (includes drops) */      *)
  tcps_segstimed       : unsigned_int;  (* u_int   tcps_segstimed;         /* segs where we tried to get rtt */     *)
  tcps_rttupdated      : unsigned_int;  (* u_int   tcps_rttupdated;        /* times we succeeded */                 *)
  tcps_delack          : unsigned_int;  (* u_int   tcps_delack;            /* delayed acks sent */                  *)
  tcps_timeoutdrop     : unsigned_int;  (* u_int   tcps_timeoutdrop;       /* conn. dropped in rxmt timeout */      *)
  tcps_rexmttimeo      : unsigned_int;  (* u_int   tcps_rexmttimeo;        /* retransmit timeouts */                *)
  tcps_persisttimeo    : unsigned_int;  (* u_int   tcps_persisttimeo;      /* persist timeouts */                   *)
  tcps_keeptimeo       : unsigned_int;  (* u_int   tcps_keeptimeo;         /* keepalive timeouts */                 *)
  tcps_keepprobe       : unsigned_int;  (* u_int   tcps_keepprobe;         /* keepalive probes sent */              *)
  tcps_keepdrops       : unsigned_int;  (* u_int   tcps_keepdrops;         /* connections dropped in keepalive */   *)

  tcps_sndtotal        : unsigned_int;  (* u_int   tcps_sndtotal;          /* total packets sent */                 *)
  tcps_sndpack         : unsigned_int;  (* u_int   tcps_sndpack;           /* data packets sent */                  *)
  tcps_sndbyte         : unsigned_int;  (* u_int   tcps_sndbyte;           /* data bytes sent */                    *)
  tcps_sndrexmitpack   : unsigned_int;  (* u_int   tcps_sndrexmitpack;     /* data packets retransmitted */         *)
  tcps_sndrexmitbyte   : unsigned_int;  (* u_int   tcps_sndrexmitbyte;     /* data bytes retransmitted */           *)
  tcps_sndacks         : unsigned_int;  (* u_int   tcps_sndacks;           /* ack-only packets sent */              *)
  tcps_sndprobe        : unsigned_int;  (* u_int   tcps_sndprobe;          /* window probes sent */                 *)
  tcps_sndurg          : unsigned_int;  (* u_int   tcps_sndurg;            /* packets sent with URG only */         *)
  tcps_sndwinup        : unsigned_int;  (* u_int   tcps_sndwinup;          /* window update-only packets sent */    *)
  tcps_sndctrl         : unsigned_int;  (* u_int   tcps_sndctrl;           /* control (SYN|FIN|RST) packets sent */ *)

  tcps_rcvtotal        : unsigned_int;  (* u_int   tcps_rcvtotal;          /* total packets received */             *)
  tcps_rcvpack         : unsigned_int;  (* u_int   tcps_rcvpack;           /* packets received in sequence */       *)
  tcps_rcvbyte         : unsigned_int;  (* u_int   tcps_rcvbyte;           /* bytes received in sequence */         *)
  tcps_rcvbadsum       : unsigned_int;  (* u_int   tcps_rcvbadsum;         /* packets received with ccksum errs */  *)
  tcps_rcvbadoff       : unsigned_int;  (* u_int   tcps_rcvbadoff;         /* packets received with bad offset */   *)
  tcps_rcvshort        : unsigned_int;  (* u_int   tcps_rcvshort;          /* packets received too short */         *)
  tcps_rcvduppack      : unsigned_int;  (* u_int   tcps_rcvduppack;        /* duplicate-only packets received */    *)
  tcps_rcvdupbyte      : unsigned_int;  (* u_int   tcps_rcvdupbyte;        /* duplicate-only bytes received */      *)
  tcps_rcvpartduppack  : unsigned_int;  (* u_int   tcps_rcvpartduppack;    /* packets with some duplicate data */   *)
  tcps_rcvpartdupbyte  : unsigned_int;  (* u_int   tcps_rcvpartdupbyte;    /* dup. bytes in part-dup. packets */    *)
  tcps_rcvoopack       : unsigned_int;  (* u_int   tcps_rcvoopack;         /* out-of-order packets received */      *)
  tcps_rcvoobyte       : unsigned_int;  (* u_int   tcps_rcvoobyte;         /* out-of-order bytes received */        *)
  tcps_rcvpackafterwin : unsigned_int;  (* u_int   tcps_rcvpackafterwin;   /* packets with data after window */     *)
  tcps_rcvbyteafterwin : unsigned_int;  (* u_int   tcps_rcvbyteafterwin;   /* bytes rcvd after window */            *)
  tcps_rcvafterclose   : unsigned_int;  (* u_int   tcps_rcvafterclose;     /* packets rcvd after "close" */         *)
  tcps_rcvwinprobe     : unsigned_int;  (* u_int   tcps_rcvwinprobe;       /* rcvd window probe packets */          *)
  tcps_rcvdupack       : unsigned_int;  (* u_int   tcps_rcvdupack;         /* rcvd duplicate acks */                *)
  tcps_rcvacktoomuch   : unsigned_int;  (* u_int   tcps_rcvacktoomuch;     /* rcvd acks for unsent data */          *)
  tcps_rcvackpack      : unsigned_int;  (* u_int   tcps_rcvackpack;        /* rcvd ack packets */                   *)
  tcps_rcvackbyte      : unsigned_int;  (* u_int   tcps_rcvackbyte;        /* bytes acked by rcvd acks */           *)
  tcps_rcvwinupd       : unsigned_int;  (* u_int   tcps_rcvwinupd;         /* rcvd window update packets */         *)
END;
PROCEDURE GetTcpStats(VAR tcpstat: tcpstatT);
  

CONST TF_ACKNOW     =    16_01;           (* ack peer immediately *)
CONST TF_DELACK     =    16_02;           (* ack, but try to delay it *)
CONST TF_NODELAY    =    16_04;           (* don't delay packets to coalesce *)
CONST TF_NOOPT      =    16_08;           (* don't use tcp options *)
CONST TF_SENTFIN    =    16_10;           (* have sent FIN *)
CONST TF_REQ_SCALE  =    16_0020;         (* have/will request window scaling *)
CONST TF_RCVD_SCALE =    16_0040;         (* other side has requested scaling *)

CONST TCPOOB_HAVEDATA =  16_01;
CONST TCPOOB_HADDATA  =  16_02;

TYPE INCOMPLETE = ADDRESS;
TYPE tcp_seq = unsigned_int;
CONST TCPT_NTIMERS = 4;
(*
 * Tcp control block, one per tcp; fields:
 *)

TYPE tcpcbT = RECORD 
  seg_next: INCOMPLETE; (* struct  ovtcpiphdr *seg_next;   /* sequencing queue */ *)
  seg_prev: INCOMPLETE; (* struct  ovtcpiphdr *seg_prev; *)
  t_state: short;       (* short   t_state;                /* state of this connection */ *)
  t_softerror: short;   (* short   t_softerror;            /* possible error not yet reported */ *)
  t_timer: ARRAY [1..TCPT_NTIMERS] OF short;   (* short   t_timer[TCPT_NTIMERS];  /* tcp timers */ *)
  t_rxtshift: short;        (* short   t_rxtshift;             /* log(2) of rexmt exp. backoff */ *)
  t_rxtcur: short;          (* short   t_rxtcur;               /* current retransmit value */ *)
  t_dupacks: short;         (* short   t_dupacks;              /* consecutive dup acks recd */ *)
  t_maxseg: unsigned_short; (* u_short t_maxseg;               /* maximum segment size */ *)
  t_force: char;            (* char    t_force;                /* 1 if forcing out a byte */ *)
  t_flags: unsigned_char;   (* u_char  t_flags; *)

  (* out-of-band data *)
  t_oobflags: char; (* char    t_oobflags;             /* have some */ *)
  t_iobc: char;     (* char    t_iobc;                 /* input character */ *)

  t_template : INCOMPLETE; (* struct  tcpiphdr t_template;    /* skeletal packet for transmit (used to be mbuf) */ *)
  t_inpcb: INCOMPLETE;     (* struct  inpcb *t_inpcb;         /* back pointer to internet pcb */ *)
  t_timestamp: tcp_seq;    (* tcp_seq t_timestamp;            /* used by slowtimo */ *)

  (*
   * The following fields are used as in the protocol specification.
   * See RFC793, Sep. 1981, page 21.
   *)

  (* send sequence variables *)
  snd_una: tcp_seq; (* tcp_seq snd_una;                /* send unacknowledged */ *)
  snd_nxt: tcp_seq; (* tcp_seq snd_nxt;                /* send next */ *)
  snd_up: tcp_seq;  (* tcp_seq snd_up;                 /* send urgent pointer */ *)
  snd_wl1: tcp_seq; (* tcp_seq snd_wl1;                /* window update seg seq number */ *)
  snd_wl2: tcp_seq; (* tcp_seq snd_wl2;                /* window update seg ack number */ *)
  iss: tcp_seq;     (* tcp_seq iss;                    /* initial send sequence number */ *)
  snd_wnd: tcp_seq; (* tcp_seq snd_wnd;                /* send window */ *)

  (* receive sequence variables *)
  rcv_wnd: tcp_seq; (* tcp_seq rcv_wnd;                /* receive window */ *)
  rcv_nxt: tcp_seq; (* tcp_seq rcv_nxt;                /* receive next */ *)
  rcv_up: tcp_seq;  (* tcp_seq rcv_up;                 /* receive urgent pointer */ *)
  irs: tcp_seq;     (* tcp_seq irs;                    /* initial receive sequence number */ *)

  (*
   * Additional variables for this implementation.
   *)

  (* receive variables *)
  rcv_adv: tcp_seq;   (* tcp_seq rcv_adv;                /* advertised window */ *)

  (* retransmit variables *)
  snd_max: tcp_seq;   (* tcp_seq snd_max;                /* highest sequence number sent; used to recognize retransmits */ *)

  (* congestion control (for slow start, source quench, retransmit after loss) *)
  snd_cwnd: tcp_seq;     (* tcp_seq snd_cwnd;               /* congestion-controlled window */ *)
  snd_ssthresh: tcp_seq; (* tcp_seq snd_ssthresh;           /* snd_cwnd size threshhold for slow start exponential to linear switch */ *)

  (*
   * transmit timing stuff.  See below for scale of srtt and rttvar.
   * "Variance" is actually smoothed difference.
   *)

  t_idle: short;                    (* short   t_idle;                 /* inactivity time */ *)
  t_rtt: short;                     (* short   t_rtt;                  /* round trip time */ *)
  t_rtseq: tcp_seq;                 (* tcp_seq t_rtseq;                /* sequence number being timed */ *)
  t_srtt: short;                    (* short   t_srtt;                 /* smoothed round-trip time */ *)
  t_rttvar: short;                  (* short   t_rttvar;               /* variance in round-trip time */ *)
  t_rttmin: unsigned_short;         (* u_short t_rttmin;               /* minimum rtt allowed */ *)
  t_msslimit: unsigned_short;       (* u_short t_msslimit;             /* user-specified max segment size */ *)
  max_rcvd: tcp_seq;                (* tcp_seq max_rcvd;               /* most peer has sent into window */ *)
  max_sndwnd: tcp_seq;              (* tcp_seq max_sndwnd;             /* largest window peer has offered */ *)
  snd_scale: unsigned_char;         (* u_char  snd_scale;              /* window scaling for send window */ *)
  rcv_scale: unsigned_char;         (* u_char  rcv_scale;              /* window scaling for recv window */ *)
  request_r_scale: unsigned_char;   (* u_char  request_r_scale;        /* pending window scaling */ *)
  requested_s_scale: unsigned_char; (* u_char  requested_s_scale; *)
  t_rptr2rxt: int;                  (* int     t_rptr2rxt;             /* Repeat counter for R2 RXT timer */ *)
  t_rptr2cur: int;                  (* int     t_rptr2cur;             /* Current repeat counter for R2 timer */ *)
  (* SPIN SPECIFIC *)
  t_timer_upcall: PROCEDURE (tp: UNTRACED REF tcpcbT): int;
END;

PROCEDURE UdpCtlInput(
    code: unsigned_short;
    VAR sin: SocketAddrIn.T;
    ip: ADDRESS);

TYPE tcp_mss_func = 
      PROCEDURE(VAR tp: tcpcbT; 
                offer: unsigned_short): unsigned_short;

PROCEDURE SetTcpMss(func: tcp_mss_func);


PROCEDURE Init(verbose:BOOLEAN);

END OsfNet.

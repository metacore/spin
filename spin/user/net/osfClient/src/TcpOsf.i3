(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Oct-96  Robert Grimm (rgrimm) at the University of Washington
 *      Added PacketSend to interface
 *
 * 01-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Remove Listen() interface.
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support for new spin shell commands.
 *
 * 03-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Interface to the OSF/1 3.x implementation of TCP/IP.
 *)

INTERFACE TcpOsf;
IMPORT TcpPktFormat;
IMPORT IpPktFormat;
IMPORT Ctypes;
IMPORT Mbuf;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "tcposf";
      CommandHelp = "-- -debug level";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;
TYPE T = TcpPktFormat.NewT;

CONST TF_ACKNOW     = 16_01;   (* /* ack peer immediately */ *)
CONST TF_DELACK     = 16_02;   (* /* ack, but try to delay it */ *)
CONST TF_NODELAY    = 16_04;   (* /* don't delay packets to coalesce */ *)
CONST TF_NOOPT      = 16_08;   (* /* don't use tcp options */ *)
CONST TF_SENTFIN    = 16_10;   (* /* have sent FIN */ *)
CONST TF_REQ_SCALE  = 16_0020; (* /* have/will request window scaling */ *)
CONST TF_RCVD_SCALE = 16_0040; (* /* other side has requested scaling */ *)

CONST TCPOOB_HAVEDATA = 16_01; (*  *)
CONST TCPOOB_HADDATA  = 16_02; (*  *)

TYPE tcp_seq = Ctypes.unsigned_int;
TYPE ovtcpiphdrT = RECORD END;
TYPE tcpiphdr = RECORD
  ipHdr: IpPktFormat.T;
  tcpHdr: T;
END;

TYPE tcpcb = RECORD
  seg_next          : UNTRACED REF ovtcpiphdrT; (* struct  ovtcpiphdr *seg_next;   /* sequencing queue */ *)
  seg_prev          : UNTRACED REF ovtcpiphdrT; (* struct  ovtcpiphdr *seg_prev; *)
  t_state           : Ctypes.short;             (* short   t_state;                /* state of this connection */ *)
  t_softerror       : Ctypes.short;             (* short   t_softerror;            /* possible error not yet reported */ *)
  t_timer           : Ctypes.short;             (* short   t_timer[TCPT_NTIMERS];  /* tcp timers */ *)
  t_rxtshift        : Ctypes.short;             (* short   t_rxtshift;             /* log(2) of rexmt exp. backoff */ *)
  t_rxtcur          : Ctypes.short;             (* short   t_rxtcur;               /* current retransmit value */ *)
  t_dupacks         : Ctypes.short;             (* short   t_dupacks;              /* consecutive dup acks recd */ *)
  t_maxseg          : Ctypes.unsigned_short;    (* u_short t_maxseg;               /* maximum segment size */ *)
  t_force           : Ctypes.char;              (* char    t_force;                /* 1 if forcing out a byte */ *)
  t_flags           : Ctypes.unsigned_char;     (* u_char  t_flags; *)

                                                (* /* out-of-band data */ *)
  t_oobflags        : Ctypes.char;              (* char    t_oobflags;             /* have some */ *)
  t_iobc            : Ctypes.char;              (* char    t_iobc;                 /* input character */ *)

  t_template        : tcpiphdr;                 (* struct  tcpiphdr t_template;    /* skeletal packet for transmit (used to be mbuf) */ *)
  t_inpcb           : ADDRESS;                  (* struct  inpcb *t_inpcb;         /* back pointer to internet pcb */ *)
  t_timestamp       : tcp_seq;                  (* tcp_seq t_timestamp;            /* used by slowtimo */ *)
  (*
   * The following fields are used as in the protocol specification.
   * See RFC793, Sep. 1981, page 21.
   *)
  (* /* send sequence variables */ *)
  snd_una           : tcp_seq;                  (* tcp_seq snd_una;                /* send unacknowledged */ *)
  snd_nxt           : tcp_seq;                  (* tcp_seq snd_nxt;                /* send next */ *)
  snd_up            : tcp_seq;                  (* tcp_seq snd_up;                 /* send urgent pointer */ *)
  snd_wl1           : tcp_seq;                  (* tcp_seq snd_wl1;                /* window update seg seq number */ *)
  snd_wl2           : tcp_seq;                  (* tcp_seq snd_wl2;                /* window update seg ack number */ *)
  iss               : tcp_seq;                  (* tcp_seq iss;                    /* initial send sequence number */ *)
  snd_wnd           : tcp_seq;                  (* tcp_seq snd_wnd;                /* send window */ *)
                                                (* /* receive sequence variables */ *)
  rcv_wnd           : tcp_seq;                  (* tcp_seq rcv_wnd;                /* receive window */ *)
  rcv_nxt           : tcp_seq;                  (* tcp_seq rcv_nxt;                /* receive next */ *)
  rcv_up            : tcp_seq;                  (* tcp_seq rcv_up;                 /* receive urgent pointer */ *)
  irs               : tcp_seq;                  (* tcp_seq irs;                    /* initial receive sequence number */ *)
  (* /* Additional variables for this implementation. */ *)
  (* /* receive variables */ *)
  rcv_adv           : tcp_seq;                  (* tcp_seq rcv_adv;                /* advertised window */ *)
                                                (* /* retransmit variables */ *)
  snd_max           : tcp_seq;                  (* tcp_seq snd_max;                /* highest sequence number sent; used to recognize retransmits *)
                                                (* /* congestion control (for slow start, source quench, retransmit after loss) */ *)
  snd_cwnd          : tcp_seq;                  (* tcp_seq snd_cwnd;               /* congestion-controlled window */ *)
  snd_ssthresh      : tcp_seq;                  (* tcp_seq snd_ssthresh;           /* snd_cwnd size threshhold for slow start exponential to linear switch */ *)
  (*
   * transmit timing stuff.  See below for scale of srtt and rttvar.
   * "Variance" is actually smoothed difference.
   *)
  t_idle            : Ctypes.short;             (* short   t_idle;                 /* inactivity time */ *)
  t_rtt             : Ctypes.short;             (* short   t_rtt;                  /* round trip time */ *)
  t_rtseq           : tcp_seq;                  (* tcp_seq t_rtseq;                /* sequence number being timed */ *)
  t_srtt            : Ctypes.short;             (* short   t_srtt;                 /* smoothed round-trip time */ *)
  t_rttvar          : Ctypes.short;             (* short   t_rttvar;               /* variance in round-trip time */ *)
  t_rttmin          : Ctypes.unsigned_short;    (* u_short t_rttmin;               /* minimum rtt allowed */ *)
  t_msslimit        : Ctypes.unsigned_short;    (* u_short t_msslimit;             /* user-specified max segment size */ *)
  max_rcvd          : tcp_seq;                  (* tcp_seq max_rcvd;               /* most peer has sent into window */ *)
  max_sndwnd        : tcp_seq;                  (* tcp_seq max_sndwnd;             /* largest window peer has offered */ *)
  snd_scale         : Ctypes.unsigned_char;     (* u_char  snd_scale;              /* window scaling for send window */ *)
  rcv_scale         : Ctypes.unsigned_char;     (* u_char  rcv_scale;              /* window scaling for recv window */ *)
  request_r_scale   : Ctypes.unsigned_char;     (* u_char  request_r_scale;        /* pending window scaling */ *)
  requested_s_scale : Ctypes.unsigned_char;     (* u_char  requested_s_scale; *)
  t_rptr2rxt        : Ctypes.int;               (* int     t_rptr2rxt;             /* Repeat counter for R2 RXT timer */ *)
  t_rptr2cur        : Ctypes.int;               (* int     t_rptr2cur;             /* Current repeat counter for R2 timer */ *)
END;

PROCEDURE PacketSend(READONLY packet : Mbuf.T; rt : REFANY);
PROCEDURE Init();
END TcpOsf.

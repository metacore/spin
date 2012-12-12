(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  FreeBSD dependent constants and types.
 *
 *)

INTERFACE SocketDep;
IMPORT Ctypes, Mbuf, Protosw;

TYPE sb_selT = RECORD (* process selecting read/write *)
  si_pid   : Ctypes.unsigned_long;
  si_flags : Ctypes.unsigned_short;
END;

TYPE sockbufT = RECORD
  sb_cc    : Ctypes.unsigned_long; (* u_long sb_cc;          /* actual chars in buffer       */ *)
  sb_hiwat : Ctypes.unsigned_long; (* u_long sb_hiwat;       /* max actual char count        */ *)
  sb_mbcnt : Ctypes.unsigned_long; (* u_long sb_mbcnt;       /* chars of mbufs used          */ *)
  sb_mbmax : Ctypes.unsigned_long; (* u_long sb_mbmax;       /* max chars of mbufs to use    */ *)
  sb_lowat : Ctypes.long;          (* long   sb_lowat;       /* low water mark               */ *)
  sb_mb    : Mbuf.T;               (* struct mbuf *sb_mb;    /* the mbuf chain               */ *)
  sb_selq  : sb_selT;              (* struct selinfo sb_sel; /* process selecting read/write */ *)
  sb_flags : Ctypes.short;         (* short  sb_flags;       /* flags, see below             */ *)
  sb_timo  : Ctypes.short;         (* short  sb_timeo;       /* timeout for read/write       */ *)
END;

(*
 * Kernel structure per socket.
 * Contains send and receive buffer queues,
 * handle on protocol and pointer to protocol
 * private data and error information.
 *)
TYPE socketT = RECORD
  so_type    : Ctypes.short;           (* int     so_type;            /* generic type, see socket.h        */ *)
  so_options : Ctypes.short;           (* int     so_options;         /* from socket call, see socket.h    */ *)
  so_linger  : Ctypes.short;           (* short   so_linger;          /* time to linger while closing      */ *)
  so_state   : Ctypes.short;           (* int     so_state;           /* internal state flags  SS_*, below */ *)
  so_pcb     : ADDRESS;                (* caddr_t so_pcb;             /* protocol control block            */ *)
  so_proto   : UNTRACED REF Protosw.T; (* struct  protosw *so_proto;  /* protocol handle                   */ *)
  (*
   * Variables for connection queueing.
   * Socket where accepts occur is so_head in all subsidiary sockets.
   * If so_head is 0, socket is not related to an accept.
   * For head socket so_q0 queues partially completed connections,
   * while so_q is a queue of connections ready to be accepted.
   * If a connection is aborted and it has so_head set, then
   * it has to be pulled out of either so_q0 or so_q.
   * We allow connections to queue up based on current queue lengths
   * and limit on number of queued connections for this socket.
   *)
  so_head    : UNTRACED REF socketT;  (* struct  socket *so_head; /* back pointer to accept socket */ *)
  so_q0      : UNTRACED REF socketT;  (* struct  socket *so_q0;   /* queue of partial  connections */ *)
  so_q       : UNTRACED REF socketT;  (* struct  socket *so_q;    /* queue of incoming connections */ *)
  so_q0len   : Ctypes.short;          (* short   so_q0len;        /* partials on so_q0             */ *)
  so_qlen    : Ctypes.short;          (* short   so_qlen;         /* number of connections on so_q */ *)
  so_qlimit  : Ctypes.short;          (* short   so_qlimit;       /* max number queued connections */ *)
  so_timeo   : Ctypes.short;          (* short   so_timeo;        /* connection timeout            */ *)
  so_error   : Ctypes.unsigned_short; (* u_short so_error;        /* error affecting  connection   */ *)
  so_pgid    : Ctypes.unsigned_long;  (* pid_t   so_pgid;         /* pgid for signals              */ *)
  so_oobmark : Ctypes.unsigned_long;  (* u_int   so_oobmark;      /* chars to oob mark             */ *)
  (*
   * Variables for socket buffering.
   *)
  sb_rcv: sockbufT;
  sb_snd: sockbufT;
  so_tpcb: ADDRESS;                 (* /* Wisc. protocol control block XXX */ *)
  so_upcall: PROCEDURE(             (* void ( *so_upcall) *)
                 VAR so: socketT;   (*  (struct socket *so, *) 
                 arg:ADDRESS;       (* caddr_t arg, *) 
                 waitf:Ctypes.int); (* int waitf)); *)
  so_upcallarg: ADDRESS;            (* /* Arg for above */ *)
END;  

CONST SB_MAX     = (256*1024); (* default for max chars in sockbuf  *)

END SocketDep.

(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created. SAL dependent types and constants.
 *
 *)

INTERFACE SocketDep;
IMPORT ULockForSAL, Ctypes, Mbuf, Protosw;

TYPE sb_selT = RECORD (* process selecting read/write *)
  next, prev : UNTRACED REF sb_selT;
END;

TYPE sockbufT = RECORD
  sb_cc    : Ctypes.unsigned_int; (* u_int sb_cc;          /* actual chars in buffer    */ *)
  sb_hiwat : Ctypes.unsigned_int; (* u_int sb_hiwat;       /* max actual char count     */ *)
  sb_mbcnt : Ctypes.unsigned_int; (* u_int sb_mbcnt;       /* chars of mbufs used       */ *)
  sb_mbmax : Ctypes.unsigned_int; (* u_int sb_mbmax;       /* max chars of mbufs to use */ *)
  sb_lowat : Ctypes.int;          (* int   sb_lowat;       /* low water mark            */ *)
  sb_mb    : Mbuf.T;              (* struct mbuf *sb_mb;   /* the mbuf chain            */ *)
  sb_selq  : sb_selT;
  sb_flags: Ctypes.int;           (* int   sb_flags;       /* flags, see below          */ *)
  sb_timo: Ctypes.int;            (* int   sb_timeo;       /* timeout for read/write    */ *)
  sb_wakeup: PROCEDURE(a:ADDRESS; i:Ctypes.int); (* void ( *sb_wakeup)(caddr_t, int); /* upcall instead of sowakeup  */ *)
  sb_wakearg: ADDRESS;            (* caddr_t sb_wakearg;   /* ( *sb_wakeup)(sb_wakearg, state) */ *)
  sb_lock: ADDRESS;               (* sb_lock_t sb_lock;    /* sockbuf lock (in socklocks)      */ *)
END;

(* XXX this changes if NETSYNCLOCK is turned on in the C code *)
TYPE socklocks = ULockForSAL.T;

(*
 * Kernel structure per socket.
 * Contains send and receive buffer queues,
 * handle on protocol and pointer to protocol
 * private data and error information.
 *)
TYPE socketT = RECORD
  so_type    : Ctypes.int;             (* int     so_type;            /* generic type, see socket.h        */ *)
  so_options : Ctypes.int;             (* int     so_options;         /* from socket call, see socket.h    */ *)
  so_linger  : Ctypes.short;           (* short   so_linger;          /* time to linger while closing      */ *)
  so_timeo   : Ctypes.short;           (* short   so_timeo;           /* connection timeout                */ *)
  so_state   : Ctypes.int;             (* int     so_state;           /* internal state flags  SS_*, below */ *)
  so_pcb     : ADDRESS;                (* caddr_t so_pcb;             /* protocol control block            */ *)
  so_proto   : UNTRACED REF Protosw.T; (* struct  protosw *so_proto;  /* protocol handle                   */ *)
  so_lock    : UNTRACED REF socklocks; (* struct  socklocks *so_lock; /* socket structure lock (s)         */ *)
  (*
   * Variables for connection queueing.
   * Socket where accepts occur is so_head in all subsidiary sockets.
   * If so_head is 0, socket is not related to an accept.
   * For head socket so_q0 queues partially completed connections,
   * while so_q is a queue of connections ready to be accepted.
   * If a connection is aborted and it has so_head set, then
   * it has to be pulled out of either so_q0 or so_q.
   * We allow connections to queue up based on current queue lengths
   * and limit on number of queued connections for this: Ctypes.struct;  socket.
   *)
  so_head    : UNTRACED REF socketT; (* struct  socket *so_head; /* back pointer to accept socket    */ *)
  so_q0      : UNTRACED REF socketT; (* struct  socket *so_q0;   /* queue of partial  connections    */ *)
  so_q       : UNTRACED REF socketT; (* struct  socket *so_q;    /* queue of incoming connections    */ *)
  so_dq      : UNTRACED REF socketT; (* struct  socket *so_dq;   /* queue of defunct connections     */ *)
  so_q0len   : Ctypes.short;         (* short   so_q0len;        /* partials on so_q0                */ *)
  so_qlen    : Ctypes.short;         (* short   so_qlen;         /* number of connections on so_q    */ *)
  so_qlimit  : Ctypes.short;         (* short   so_qlimit;       /* max number queued connections    */ *)
  so_dqlen   : Ctypes.short;         (* short   so_dqlen;        /* listener dequeues in progress    */ *)
  so_error   : Ctypes.int;           (* int     so_error;        /* error affecting  connection      */ *)
  so_special : Ctypes.int;           (* int     so_special;      /* special state flags SP_ *, below */ *)
  so_pgid    : Ctypes.int;           (* pid_t   so_pgid;         /* pgid for signals                 */ *)
  so_oobmark : Ctypes.unsigned_int;  (* u_int   so_oobmark;      /* chars to oob mark                */ *)
  (*
   * Variables for socket buffering.
   *)
  sb_rcv: sockbufT;
  sb_snd: sockbufT;
  (* NOT USED IN OUR KERNEL *) (* tag_t   so_tag[SEC_TAG_COUNT]; /* security policy tags */ *)
END;  

CONST SB_MAX     = (128*1024); (* default for max chars in sockbuf  *)
END SocketDep.

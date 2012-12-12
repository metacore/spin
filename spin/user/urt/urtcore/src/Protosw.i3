(*
 * Copyright 1995-1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to be SAL independent.
 *
 * 19-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added domainT type.
 *
 * 08-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Based on sys/protosw.h from DEC OSF/1 v3.2
 *
 *)

(* Protocol switch table.

   Each protocol has a handle initializing one of these structures,
   which is used for protocol-protocol and system-protocol
   communication.
 
   A protocol is called through the pr_init entry before any other.
   Thereafter it is called every 200ms through the pr_fasttimo entry
   and every 500ms through the pr_slowtimo for timer based actions.
   The system will call the pr_drain entry if it is low on space and
   this should throw away any non-critical data.

   Protocols pass data between themselves as chains of mbufs using the
   pr_input and pr_output hooks.  Pr_input passes data up (towards
   UNIX) and pr_output passes it down (towards the imps); control
   information passes up and down on pr_ctlinput and pr_ctloutput.
   The protocol is responsible for the space occupied by any the
   arguments to these entries and must dispose it.

   The userreq routine interfaces protocols to the system and is
   described below. *)


INTERFACE Protosw;
IMPORT Mbuf, ProtoswDep;
FROM Ctypes IMPORT short, int, char_star;
TYPE domainT = RECORD
  dom_family          : int;                              (* int     dom_family;             /* AF_xxx */                   *)
  dom_name            : char_star;                        (* char    *dom_name;                                             *)
  dom_init            : PROCEDURE();                      (* void    ( *dom_init)(void);                                    *)
  dom_externalize     : PROCEDURE(m:Mbuf.T);              (* int     ( *dom_externalize)(struct mbuf * );                   *)
  dom_dispose         : PROCEDURE(m:Mbuf.T);              (* void    ( *dom_dispose)(struct mbuf * );                       *)
  dom_protosw         : UNTRACED REF T;                   (* struct  protosw CONST *dom_protosw, *dom_protoswNPROTOSW;      *)
  dom_protoswNPROTOSW : UNTRACED REF T;                   (* struct  protosw CONST *dom_protosw, *dom_protoswNPROTOSW;      *)
  dom_next            : UNTRACED REF domainT;             (* struct  domain *dom_next;                                      *)
  dom_refcnt          : int;                              (* int     dom_refcnt;             /* # sockets in this domain */ *)
  dom_funnel          : PROCEDURE(domain_funnel:ADDRESS); (* void    ( *dom_funnel)(struct domain_funnel * );               *)
  dom_funcfrc         : PROCEDURE(domain_funnel:ADDRESS); (* void    ( *dom_funfrc)(struct domain_funnel * );               *)
  (* #if  NETSYNC_LOCK
     simple_lock_data_t      dom_rc_lock;
     #endif *)
END;




TYPE T = RECORD
  pr_type: short;                  (* short   pr_type;            /* socket type used for            */          *)
  pr_domain: UNTRACED REF domainT; (* struct  domain *pr_domain;  /* domain protocol a member of     */          *)
  pr_protocol: short;              (* short   pr_protocol;        /* protocol number                 */          *)
  pr_flags: short;                 (* short   pr_flags;           /* see below                       */          *)
                                   (*                             /* protocol-protocol hooks         */          *)
  pr_input: PROCEDURE();           (* void    ( *pr_input)();     /* prototypes per-domain           */          *)
  pr_output: PROCEDURE():int;      (* int     ( *pr_output)();    /* prototypes per-domain           */          *)
  pr_ctlinput: PROCEDURE();        (* void    ( *pr_ctlinput)(int, struct sockaddr *, caddr_t);                  *)
  pr_ctloutput: PROCEDURE():int;   (* int     ( *pr_ctloutput)(int, struct socket *, int, int, struct mbuf ** ); *)
                                   (* XXX circular dependency for the first argument to pr_usrreq.               *)
  (* int     ( *pr_usrreq)(struct socket *, int, struct mbuf *, struct mbuf *, struct mbuf * );                  *)
  pr_usrreq: PROCEDURE(so: ADDRESS; req: int; m,nam,control: Mbuf.T):int;

  pr_init: PROCEDURE();            (* void    ( *pr_init)(void);                                                 *)
  pr_fasttimo: PROCEDURE();        (* void    ( *pr_fasttimo)(void);                                             *)
  pr_slowtimo: PROCEDURE();        (* void    ( *pr_slowtimo)(void);                                             *)
  pr_drain: PROCEDURE();           (* void    ( *pr_drain)(void);                                                *)
END;

CONST 
 PR_SLOWHZ = 2; (* #define PR_SLOWHZ       2               /* 2 slow timeouts per second */ *)
 PR_FASTHZ = 5; (* #define PR_FASTHZ       5               /* 5 fast timeouts per second */ *)

 (*
   * Values for pr_flags.
   * PR_ADDR requires PR_ATOMIC;
   * PR_ADDR and PR_CONNREQUIRED are mutually exclusive.
 *)

CONST
 PR_ATOMIC       = 16_01; (* #define PR_ATOMIC       0x01 /* exchange atomic messages only */      *)
 PR_ADDR         = 16_02; (* #define PR_ADDR         0x02 /* addresses given with messages */      *)
 PR_CONNREQUIRED = 16_04; (* #define PR_CONNREQUIRED 0x04 /* connection required by protocol */    *)
 PR_WANTRCVD     = 16_08; (* #define PR_WANTRCVD     0x08 /* want PRU_RCVD calls */                *)
 PR_RIGHTS       = 16_10; (* #define PR_RIGHTS       0x10 /* passes capabilities */                *)
 PR_SEQPACKET    = 16_20; (* #define PR_SEQPACKET    0x20 /* sequenced-packet protocol */          *)
 PR_READZEROLEN  = 16_40; (* #define PR_READZEROLEN  0x40 /* protocol supports zero-length reads*/ *)

(*
  The arguments to usrreq are:
 	( *protosw[].pr_usrreq)(up, req, m, nam, control);
  where up is a (struct socket * ), req is one of these requests,
  m is a optional mbuf chain containing a message,
  nam is an optional mbuf chain containing an address,
  and control is a pointer to a control chain or nil.
  The protocol is responsible for disposal of the mbuf chain m,
  the caller is responsible for any space held by nam and control.
  A non-zero return from usrreq gives an
  UNIX error number which should be passed to higher level software. *)

CONST
 PRU_ATTACH     = 0;  (* #define PRU_ATTACH              0       /* attach protocol to up */           *)
 PRU_DETACH     = 1;  (* #define PRU_DETACH              1       /* detach protocol from up */         *)
 PRU_BIND       = 2;  (* #define PRU_BIND                2       /* bind socket to address */          *)
 PRU_LISTEN     = 3;  (* #define PRU_LISTEN              3       /* listen for connection */           *)
 PRU_CONNECT    = 4;  (* #define PRU_CONNECT             4       /* establish connection to peer */    *)
 PRU_ACCEPT     = 5;  (* #define PRU_ACCEPT              5       /* accept connection from peer */     *)
 PRU_DISCONNECT = 6;  (* #define PRU_DISCONNECT          6       /* disconnect from peer */            *)
 PRU_SHUTDOWN   = 7;  (* #define PRU_SHUTDOWN            7       /* won't send any more data */        *)
 PRU_RCVD       = 8;  (* #define PRU_RCVD                8       /* have taken data; more room now */  *)
 PRU_SEND       = 9;  (* #define PRU_SEND                9       /* send this data */                  *)
 PRU_ABORT      = 10; (* #define PRU_ABORT               10      /* abort (fast DISCONNECT, DETACH) */ *)
 PRU_CONTROL    = 11; (* #define PRU_CONTROL             11      /* control operations on protocol */  *)
 PRU_SENSE      = 12; (* #define PRU_SENSE               12      /* return status into m */            *)
 PRU_RCVOOB     = 13; (* #define PRU_RCVOOB              13      /* retrieve out of band data */       *)
 PRU_SENDOOB    = 14; (* #define PRU_SENDOOB             14      /* send out of band data */           *)
 PRU_SOCKADDR   = 15; (* #define PRU_SOCKADDR            15      /* fetch socket's address */          *)
 PRU_PEERADDR   = 16; (* #define PRU_PEERADDR            16      /* fetch peer's address */            *)
 PRU_CONNECT2   = 17; (* #define PRU_CONNECT2            17      /* connect two sockets */             *)
                      (* /* begin for protocols internal use */                                        *)
 PRU_FASTTIMO   = 18; (* #define PRU_FASTTIMO            18      /* 200ms timeout */                   *)
 PRU_SLOWTIMO   = 19; (* #define PRU_SLOWTIMO            19      /* 500ms timeout */                   *)
 PRU_PROTORCV   = 20; (* #define PRU_PROTORCV            20      /* receive from below */              *)
 PRU_PROTOSEND  = 21; (* #define PRU_PROTOSEND           21      /* send to below */                   *)

 PRU_SEND_EOF   = ProtoswDep.PRU_SEND_EOF;
 PRU_NREQ       = ProtoswDep.PRU_NREQ;

    (* char    *prurequests[] = { 
	"ATTACH",	"DETACH",	"BIND",		"LISTEN",
	"CONNECT",	"ACCEPT",	"DISCONNECT",	"SHUTDOWN",
	"RCVD",		"SEND",		"ABORT",	"CONTROL",
	"SENSE",	"RCVOOB",	"SENDOOB",	"SOCKADDR",
	"PEERADDR",	"CONNECT2",	"FASTTIMO",	"SLOWTIMO",
	"PROTORCV",	"PROTOSEND"
    *)

(*
 * The arguments to the ctlinput routine are
 *	( *protosw[].pr_ctlinput)(cmd, sa, arg);
 * where cmd is one of the commands below, sa is a pointer to a sockaddr,
 * and arg is an optional caddr_t argument used within a protocol family.
 *)
CONST
 PRC_IFDOWN           = 0;  (* #define PRC_IFDOWN              0  /* interface transition */               *)
 PRC_ROUTEDEAD        = 1;  (* #define PRC_ROUTEDEAD           1  /* select new route if possible ??? */   *)
 PRC_QUENCH2          = 3;  (* #define PRC_QUENCH2             3  /* DEC congestion bit says slow down */  *)
 PRC_QUENCH           = 4;  (* #define PRC_QUENCH              4  /* some one said to slow down */         *)
 PRC_MSGSIZE          = 5;  (* #define PRC_MSGSIZE             5  /* message size forced drop */           *)
 PRC_HOSTDEAD         = 6;  (* #define PRC_HOSTDEAD            6  /* host appears to be down */            *)
 PRC_HOSTUNREACH      = 7;  (* #define PRC_HOSTUNREACH         7  /* deprecated (use PRC_UNREACH_HOST) */  *)
 PRC_UNREACH_NET      = 8;  (* #define PRC_UNREACH_NET         8  /* no route to network */                *)
 PRC_UNREACH_HOST     = 9;  (* #define PRC_UNREACH_HOST        9  /* no route to host */                   *)
 PRC_UNREACH_PROTOCOL = 10; (* #define PRC_UNREACH_PROTOCOL    10 /* dst says bad protocol */              *)
 PRC_UNREACH_PORT     = 11; (* #define PRC_UNREACH_PORT        11 /* bad port # */                         *)
                            (* /* was  PRC_UNREACH_NEEDFRAG    12         (use PRC_MSGSIZE) */             *)
 PRC_UNREACH_SRCFAIL  = 13; (* #define PRC_UNREACH_SRCFAIL     13 /* source route failed */                *)
 PRC_REDIRECT_NET     = 14; (* #define PRC_REDIRECT_NET        14 /* net routing redirect */               *)
 PRC_REDIRECT_HOST    = 15; (* #define PRC_REDIRECT_HOST       15 /* host routing redirect */              *)
 PRC_REDIRECT_TOSNET  = 16; (* #define PRC_REDIRECT_TOSNET     16 /* redirect for type of service & net */ *)
 PRC_REDIRECT_TOSHOST = 17; (* #define PRC_REDIRECT_TOSHOST    17 /* redirect for tos & host */            *)
 PRC_TIMXCEED_INTRANS = 18; (* #define PRC_TIMXCEED_INTRANS    18 /* packet lifetime expired in transit */ *)
 PRC_TIMXCEED_REASS   = 19; (* #define PRC_TIMXCEED_REASS      19 /* lifetime expired on reass q */        *)
 PRC_PARAMPROB        = 20; (* #define PRC_PARAMPROB           20 /* header incorrect */                   *)

 
 (* on DEC OSF/1 only *)
 PRC_NEWADDRSET       = 21; (* #define PRC_NEWADDRSET          21 /* NSAP change */                        *)
 PRC_EVENT            = 22; (* #define PRC_EVENT               22 /* net event notification via CML */     *)
 PRC_NMADD            = 23; (* #define PRC_NMADD               23 /* proto registration with CML */        *)

 PRC_NCMDS            = ProtoswDep.PRC_NCMDS;

 (* #define PRC_IS_REDIRECT(cmd) ((cmd) >= PRC_REDIRECT_NET && (cmd) <= PRC_REDIRECT_TOSHOST) *)

(*
  The arguments to ctloutput are:
 	( *protosw[].pr_ctloutput)(req, so, level, optname, optval);
  req is one of the actions listed below, so is a (struct socket * ),
  level is an indication of which protocol layer the option is intended.
  optname is a protocol dependent socket option request,
  optval is a pointer to a mbuf-chain pointer, for value-return results.
  The protocol is responsible for disposal of the mbuf chain *optval
  if supplied,
  the caller is responsible for any space held by *optval, when returned.
  A non-zero return from usrreq gives an
  UNIX error number which should be passed to higher level software. *)

CONST
 PRCO_GETOPT = 0; (* #define PRCO_GETOPT     0                                   *)
 PRCO_SETOPT = 1; (* #define PRCO_SETOPT     1                                   *)
 PRCO_PIF    = 2; (* #define PRCO_PIF        2 /* proto info notification */     *)

 (* on DEC OSF/1 only *)
 PRCO_NWMGT  = 3; (* #define PRCO_NWMGT      3 /* net mgmt event notification */ *)
 PRCO_TRACE  = 4; (* #define PRCO_TRACE      4 /* trace event (non-CTF) */       *)

 PRCO_NCMDS  = ProtoswDep.PRCO_NCMDS;
END Protosw.


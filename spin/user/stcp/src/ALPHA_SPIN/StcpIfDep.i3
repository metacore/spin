(*
 * Copyright 1994-1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 02-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  This file is based on DEC OSF/1 V3.2 net/if.h
 *)

INTERFACE StcpIfDep;

FROM Ctypes IMPORT int, short, char_star, unsigned_short,
                   unsigned_char, unsigned_int;
IMPORT StcpTime, StcpSocketAddr, StcpMbuf;

(* ALERT: The following type is used where the actual type has not
   been defined yet.  To fully use the structures, fields with this
   type need to be updated with references to a defined type.
*)
TYPE INCOMPLETE = ADDRESS;

CONST IFNET_SLOWHZ = 1;

(* interface name length. *)
CONST IFNAMSIZ = 16;

(* ifaddr structures *)
(*
 * The ifaddr structure contains information about one address
 * of an interface.  They are maintained by the different address families,
 * are allocated and attached when an address is set, and are linked
 * together so all addresses for an interface can be located.
 *)

TYPE ifaddr = RECORD
  (* #define ifa_broadaddr   ifa_dstaddr /* broadcast address interface */ *)
  ifa_addr      : UNTRACED REF StcpSocketAddr.T;  (* struct  sockaddr *ifa_addr;     /* address of interface */          *)
  ifa_dstaddr   : UNTRACED REF StcpSocketAddr.T;  (* struct  sockaddr *ifa_dstaddr;  /* other end of p-to-p link */      *)
  ifa_netmask   : UNTRACED REF StcpSocketAddr.T;  (* struct  sockaddr *ifa_netmask;  /* used to determine subnet */      *)
  ifa_ifp       : UNTRACED REF ifnet;               (* struct  ifnet *ifa_ifp;         /* back-pointer to interface */     *)
  ifa_next      : UNTRACED REF ifaddr;              (* struct  ifaddr *ifa_next;       /* next address for interface */    *)
  ifa_rtrequest : PROCEDURE(                        (* void    ( *ifa_rtrequest)                                           *)
                      arg1: int;                    (* (int,                                                               *)
                      VAR rtentry: INCOMPLETE;      (* struct rtentry *,                                                   *)
                      VAR arg3:StcpSocketAddr.T); (* struct sockaddr * );                                                *)
  ifa_rt        : INCOMPLETE;                       (* struct  rtentry *ifa_rt;        /* ??? for ROUTETOIF */             *)
  ifa_flags     : unsigned_short;                   (* u_short ifa_flags;              /* mostly rt_flags for cloning */   *)
  ifa_llinfolen : unsigned_short;                   (* u_short ifa_llinfolen;          /* extra to malloc for link info */ *)
END;

(* TYPE rtentryT = RECORD (* actually defined in OsfTcp.i3 *) END; *)
TYPE ifnet = RECORD
  if_next        : UNTRACED REF ifnet;  (* struct  ifnet *if_next; /* the next structure in the list */       *)
  if_name        : char_star;           (* char    *if_name; /* name, e.g. ``en'' or ``lo */''                *)
  if_version     : char_star;           (* char    *if_version; /* The version string */.                     *)
  if_addr        : StcpSocketAddr.T;  (* struct  sockaddr if_addr; /* address of interface */               *)
  if_flags       : int;                 (* int     if_flags; /* up/down, broadcast, etc */.                   *)
  if_unit        : short;               (* short   if_unit; /* sub-unit for lower level driver */             *)
  if_mtu         : unsigned_short;      (* u_short if_mtu; /* maximum IP transmission unit */                 *)
  if_mediamtu    : unsigned_short;      (* u_short if_mediamtu; /* maximum MTU of the media */                *)
  if_timer       : short;               (* short   if_timer; /* time 'til if_watchdog called */               *)
  if_metric      : int;                 (* int     if_metric; /* routing metric (external only */)            *)
  if_addrlist    : UNTRACED REF ifaddr; (* struct  ifaddr *if_addrlist; /* linked list of addresses per if */ *)
  if_multiaddr   : INCOMPLETE;          (* struct  ifmulti *if_multiaddrs; /* list of multicast addrs */      *)
  if_multicnt    : int;                 (* int     if_multicnt; /* number of multicast addrs in list */       *)   
  if_allmulticnt : int;                 (* int     if_allmulticnt; /* number of allmulti requests */          *) 

  if_init        : PROCEDURE(unit: int):int;            (* int ( *if_init)(int);                  *)
  if_output      : PROCEDURE(                           (* int     ( *if_output)                  *)
                       VAR ifp: ifnet;                  (* (struct ifnet *,                       *)
                       m: StcpMbuf.T;                       (* struct mbuf *,                         *)
                       VAR s: StcpSocketAddr.T;       (* struct sockaddr *,                     *)
                       rt: INCOMPLETE):int;             (* struct rtentry * );                    *)
  if_start       : PROCEDURE(READONLY ifp: ifnet): int; (* int     ( *if_start)(struct ifnet * ); *)
  if_done        : PROCEDURE(READONLY ifp: ifnet): int; (* int     ( *if_done)(struct ifnet * );  *)
  if_ioctl       : PROCEDURE(                           (* int     ( *if_ioctl)                   *)
                       READONLY ifp: ifnet;             (*  (struct ifnet *,                      *)
                       cmd: unsigned_int;                    (* unsigned int,                          *)
                       data: ADDRESS): int;             (* caddr_t);                              *)
  if_reset       : PROCEDURE(arg1, arg2: int): int;     (* int     ( *if_reset)(int, int);        *)
  if_watchdog    : PROCEDURE(arg1:int);                 (* int     ( *if_watchdog)(int);          *)

                                  (* generic interface statistics                                       *)
  if_ipackets    : int;           (* int     if_ipackets; /* packets received on interface */          *)
  if_ierrors     : int;           (* int     if_ierrors; /* input errors on interface */               *)
  if_opackets    : int;           (* int     if_opackets; /* packets sent on interface */              *)
  if_oerrors     : int;           (* int     if_oerrors; /* output errors on interface */              *)
  if_collisions  : int;           (* int     if_collisions; /* collisions on csma interfaces */        *)
  if_sysid_type  : int;           (* int     if_sysid_type; /* MOP SYSID device code */                *)

                                  (* SNMP statistics                                                   *)
  if_lastchange  : StcpTime.timeval;  (* struct  timeval if_lastchange; /* last updated */                 *)
  if_ibytes      : int;           (* total number of octets received                                   *)
  if_obytes      : int;           (* total number of octets sent                                       *)
  if_imcasts     : int;           (* packets received via multicast                                    *)
  if_omcasts     : int;           (* packets sent via multicast                                        *)
  if_iqdrops     : int;           (* dropped on input, this interface                                  *)
  if_noproto     : int;           (* destined for unsupported protocol                                 *)
  if_baudrate    : int;           (* linespeed                                                         *)
                                  (* end statistics                                                    *)
  if_type        : unsigned_char; (* ethernet, tokenring, etc                                          *)
  if_addrlen     : unsigned_char; (* media address length                                              *)
  if_hdrlen      : unsigned_char; (* media header length                                               *)
  if_index       : unsigned_char; (* numeric abbreviation for this if                                  *)
                                  (* For future expansion                                              *)
  futureuse_4    : int;           (* to be used for future expansion                                   *)
  futureuse_3    : int;           (* to be used for future expansion                                   *)
  futureuse_2    : int;           (* to be used for future expansion                                   *)
  futureuse_1    : int;           (* to be used for future expansion                                   *)
  if_affinity    : int;           (* which CPU to run on (master, all)                                 *)
  if_snd         : ifqueue;

  if_funhide     : INCOMPLETE;    (* to hide driver routines for those drivers who aren't parallelized *)
  
                                  (* #ifdef LOCK_NETSTATS                                              *)
  if_slock       : SimpleLockT;  
                                  (* #if !defined(_KERNEL) || !(NETSYNC_LOCK)                          *)
  lk_softc       : UNTRACED REF SimpleLockT;
END;

CONST 
  IFF_MULTICAST   = 16_400;  (* supports multicast               *)
  IFF_OACTIVE     = 16_1000; (* transmission in progress         *)

TYPE ifqueue = RECORD
  ifq_head   : StcpMbuf.T;
  ifq_tail   : StcpMbuf.T;
  ifq_len    : CARDINAL;
  ifq_maxlen : CARDINAL;
  ifq_drops  : CARDINAL;
  (* #if !defined(_KERNEL) || (!NETSYNC_LOCK) *)
  lock       : SimpleLockT;
END;

(* locks used by if *)
TYPE SimpleLockT = RECORD
  sl_data : BITS 32 FOR unsigned_int;
  sl_info : BITS 16 FOR unsigned_short;
  sl_cpuid: BITS  8 FOR unsigned_char;
  sl_lifms: BITS  8 FOR unsigned_char;
END;

END StcpIfDep.

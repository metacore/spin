(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 12-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Borrowed from urt/urtcore/src/IX86_SPIN.  Removed Ioctl.
 *
 * 02-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  This file is based on FreeBSD net/if.h
 *)

INTERFACE StcpIfDep;
FROM Ctypes IMPORT int, short, char_star, unsigned_long, 
                   unsigned_short, unsigned_char, unsigned_int;
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

(* The ifaddr structure contains information about one address of an
   interface.  They are maintained by the different address families,
   are allocated and attached when an address is set, and are linked
   together so all addresses for an interface can be located.  *)

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
  ifa_flags     : unsigned_short;                   (* u_short ifa_flags;              /* mostly rt_flags for cloning */   *)
  ifa_refcnt    : short;
  ifa_metric    : int;
END;

(* TYPE rtentryT = RECORD (* actually defined in OsfTcp.i3 *) END; *)
TYPE ifnet = RECORD
  if_name        : char_star;                           (* char    *if_name;                name, e.g. ``en'' or ``lo''     *)
  if_next        : UNTRACED REF ifnet;                  (* struct  ifnet *if_next;          the next structure in the list  *)
  if_addrlist    : UNTRACED REF ifaddr;                 (* struct  ifaddr *if_addrlist;     linked list of addresses per if *)
  if_pcount      : INTEGER;                             (* int     if_pcount;               number of promiscuous listeners *)
  if_bpf         : ADDRESS;                             (* caddr_t if_bpf;                  packet filter structure         *)
  if_index       : unsigned_short;                      (* u_short if_index;                numeric abbreviation for this if*)
  if_unit        : short;                               (* short   if_unit;                 sub-unit for lower level driver *)
  if_timer       : short;                               (* short   if_timer;                time 'til if_watchdog called    *)
  if_flags       : short;                               (* short   if_flags;                up/down, broadcast, etc.        *)
  if_type        : unsigned_char;                       (* ethernet, tokenring, etc                                         *)
  if_physical    : unsigned_char;
  if_addrlen     : unsigned_char;                       (* media address length                                             *)
  if_hdrlen      : unsigned_char;                       (* media header length                                              *)
  if_mtu         : unsigned_long;                       (* u_long if_mtu;                  maximum IP transmission unit    *)
  if_metric      : unsigned_long;                       (* u_long if_metric;               routing metric (external only)  *)
  if_baudrate    : unsigned_long;                       (* linespeed                                                        *)
  if_ipackets    : unsigned_long;                       (* u_long if_ipackets;             packets received on interface   *)
  if_ierrors     : unsigned_long;                       (* u_long if_ierrors;              input errors on interface       *)
  if_opackets    : unsigned_long;                       (* u_long if_opackets;             packets sent on interface       *)
  if_oerrors     : unsigned_long;                       (* u_long if_oerrors;              output errors on interface      *)
  if_collisions  : unsigned_long;                       (* u_long if_collisions;           collisions on csma interfaces   *)
  if_ibytes      : unsigned_long;                       (* total number of octets received                                  *)
  if_obytes      : unsigned_long;                       (* total number of octets sent                                      *)
  if_imcasts     : unsigned_long;                       (* packets received via multicast                                   *)
  if_omcasts     : unsigned_long;                       (* packets sent via multicast                                       *)
  if_iqdrops     : unsigned_long;                       (* dropped on input, this interface                                 *)
  if_noproto     : unsigned_long;                       (* destined for unsupported protocol                                *)
  if_lastchange  : StcpTime.timeval;                        (* struct  timeval if_lastchange;  last updated                     *)

  (* procedure handles                                                *)
  if_init        : PROCEDURE(unit: int):int;   (* int ( *if_init)(int); *)
  if_output      : PROCEDURE(                           (* int     ( *if_output)                                            *)
                       VAR ifp: ifnet;                  (* (struct ifnet *,                                                 *)
                       m: StcpMbuf.T;                       (* struct mbuf *,                                                   *)
                       VAR s: StcpSocketAddr.T;       (* struct sockaddr *,                                               *)
                       rt: INCOMPLETE):int;             (* struct rtentry * );                                              *)
  if_start       : PROCEDURE(READONLY ifp: ifnet): int; (* int     ( *if_start)(struct ifnet * );                           *)
  if_done        : PROCEDURE(READONLY ifp: ifnet): int; (* int     ( *if_done)(struct ifnet * );                            *)
  if_ioctl       : PROCEDURE(                           (* int     ( *if_ioctl)                                             *)
                       READONLY ifp: ifnet;             (*  (struct ifnet *,                                                *)
                       cmd: unsigned_int;                    (* unsigned int,                                                    *)
                       data: ADDRESS): int;             (* caddr_t);                                                        *)
  if_reset       : PROCEDURE(arg1, arg2: int): int;     (* int     ( *if_reset)(int, int);                                  *)
  if_watchdog    : PROCEDURE(arg1:int);                 (* int     ( *if_watchdog)(int);                                    *)
  if_snd         : ifqueue;
END;

CONST 
  IFF_MULTICAST   = 16_400;  (* supports multicast               *)
  IFF_OACTIVE     = 16_1000; (* transmission in progress         *)

TYPE ifqueue = RECORD
  ifq_head   : StcpMbuf.T;
  ifq_tail   : StcpMbuf.T;
  ifq_len    : int;
  ifq_maxlen : int;
  ifq_drops  : int;
END;

END StcpIfDep.

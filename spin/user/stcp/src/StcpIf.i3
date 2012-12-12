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
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Borrowed from urt/urtcore/src.
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to be SAL independent.
 *
 * 11-Jun-96 oystr at the University of Washington
 *	Added ifconf struct.
 *
 * 08-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Add SetNetOutput function which permits the unsafe interface
 *	StcpIfExtern to be hidden rather than visible.
 *
 * 11-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	This interface assumes that the following #defines are true in sys/if.h
 *		_KERNEL = TRUE
 *		NETSYNCLOCK = TRUE
 *		LOCK_NETSTATS = TRUE
 *       which are defined based on other #defines (_KERNEL) in Makefile
 *       and (UNIX_LOCKS) unix_locks.h.
 *	
 * 10-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	This interface is a close representation of net/if.h.
 *	Due to interfacing with C code, this interface does not define
 *	fields of unix structs using traced references.
 *
 * 07-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created to be compatible with net/if.h in Unix.
 *
 *)

INTERFACE StcpIf;

FROM Ctypes IMPORT int, char, unsigned_int;
IMPORT StcpSocketAddr, StcpSocketAddrIn, StcpMbuf, Word, StcpIfDep;

(* NOTE: The following type is used where the actual type has not
   been defined yet.  To fully use the structures, fields with this
   type need to be updated with references to a defined type.
*)
TYPE INCOMPLETE = ADDRESS;


CONST IFNET_SLOWHZ = StcpIfDep.IFNET_SLOWHZ;

(* interface name length. *)
CONST IFNAMSIZ = StcpIfDep.IFNAMSIZ;

(* Interface request structure used for socket ioctl's.  All interface
   ioctl's must have parameter definitions which begin with ifr_name.
   The remainder may be interface specific.  *)

TYPE ifreq = RECORD (* net/if.h ifreq *)
  ifr_name : ARRAY [1..IFNAMSIZ] OF CHAR; (* if name, e.g. "en0" *)
  ifr_ifru : ARRAY [1..BYTESIZE(StcpSocketAddr.T)] OF CHAR;
  (* XXX: assuming sockaddr is largest struct. 
     M3 doesn't support variant records. 
     Use VIEW to look at bytes as a record. *)
END;

(* structure used to query de and qe for physical addresses *)
TYPE ifdevea = RECORD (* net/if.h ifdevea *)
  ifr_name   : ARRAY [1..IFNAMSIZ] OF CHAR;             (* if name, e.g. "en0" *)
  default_pa : ARRAY [1..6] OF CHAR;                  (* default hardware address *)
  current_pa : ARRAY [1..6] OF CHAR;                  (* current physical address *)
END;

TYPE ifaliasreq = RECORD (* net/if.h ifaliasreq *)
  ifra_name: ARRAY [1..IFNAMSIZ] OF char; (* char    ifra_name[IFNAMSIZ];            /* if name, e.g. "en0" */ *)
  ifra_addr: StcpSocketAddrIn.T;       (* struct  sockaddr_in ifra_addr;                                    *)
  ifra_broadaddr: StcpSocketAddrIn.T;  (* struct  sockaddr_in ifra_broadaddr;                               *)
  ifra_mask: StcpSocketAddrIn.T;       (* struct  sockaddr_in ifra_mask;                                    *)
END;

(*
 * N.B. - This structure is currently (May '96) only used
 * ``remotely'' by ioctl routines dropping down from user
 * space.
 *)
TYPE ifconf = RECORD (* net/if.h ifconf *)
  ifc_len : int;
  (*
    union {
      caddr_t ifcu_buf;
      struct  ifreq *ifcu_req;
    }
    N.B. - ifc_buf is, in general, the user level
    address of the output array.
  *)
  ifc_buf : Word.T;
END;

(* The ifaddr structure contains information about one address of an
   interface.  They are maintained by the different address families,
   are allocated and attached when an address is set, and are linked
   together so all addresses for an interface can be located.  *)
TYPE ifaddr = StcpIfDep.ifaddr;

(* Structure describing information about an interface which may be of
   interest to management entities. *)
TYPE ifnet = StcpIfDep.ifnet;

CONST 
  IFF_UP          = 16_1;    (* interface is up                  *)
  IFF_BROADCAST   = 16_2;    (* broadcast address valid          *)
  IFF_DEBUG       = 16_4;    (* turn on debugging                *)
  IFF_LOOPBACK    = 16_8;    (* is a loopback net                *)
  IFF_POINTOPOINT = 16_10;   (* interface is point-to-point link *)
  IFF_NOTRAILERS  = 16_20;   (* avoid use of trailers            *)
  IFF_RUNNING     = 16_40;   (* resources allocated              *)
  IFF_NOARP       = 16_80;   (* no address resolution protocol   *)
  IFF_PROMISC     = 16_100;  (* receive all packets              *)
  IFF_ALLMULTI    = 16_200;  (* receive all multicast packets    *)
  IFF_SIMPLEX     = 16_800;  (* can't hear own transmissions     *)
  IFF_MULTICAST   = StcpIfDep.IFF_MULTICAST;
  IFF_OACTIVE     = StcpIfDep.IFF_OACTIVE;

(* flags set internally only: *)
CONST
  IFF_CANTCHANGE:unsigned_int = Word.Or(IFF_BROADCAST, 
                    Word.Or(IFF_POINTOPOINT,
                    Word.Or(IFF_SIMPLEX,
                    Word.Or(IFF_ALLMULTI,
                    Word.Or(IFF_RUNNING, 
                    Word.Or(IFF_OACTIVE,IFF_MULTICAST))))));

(* ifqueue data structure, constants and procedures *)
CONST MAXLEN = 512;
TYPE ifqueue = StcpIfDep.ifqueue;
PROCEDURE Enqueue(VAR ifq:ifqueue; m: StcpMbuf.T);
PROCEDURE Dequeue(VAR ifq:ifqueue):StcpMbuf.T;
END StcpIf.

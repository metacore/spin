(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at University of Washington
 *	Copied from user/net/ether/src.
 *)


(* Untrusted *)
INTERFACE StcpEtherPktFormat;
IMPORT Ctypes;

TYPE EtherType = BITS 16 FOR Ctypes.unsigned_short;
CONST
  ETHERTYPE_PUP    : EtherType= 16_0200; (* PUP protocol *)
  ETHERTYPE_IP     : EtherType= 16_0800; (* IP protocol *)
  ETHERTYPE_ARP    : EtherType= 16_0806; (* Addr. resolution protocol *)
  ETHERTYPE_LAT    : EtherType= 16_6004; (* Local Area Transport (LAT) *)
  ETHERTYPE_DECNET : EtherType= 16_6003; (* Phase IV DECnet *)
  ETHERTYPE_MOPRC  : EtherType= 16_6002; (* MOP CCR protocol type *)
  ETHERTYPE_MOPDL  : EtherType= 16_6001; (* MOP Downline Load protocol type *)
  ETHERTYPE_LBACK  : EtherType= 16_9000; (* MOP loopback protocol type *)
  ETHERTYPE_AM     : EtherType= 16_08ff; (* ACTIVE MESSAGE OVER ETHERNET *)

(*
   Harbison p258: packed types only need to be packed in records, objects, and arrays.
*)
TYPE Address = ARRAY [0..5] OF Ctypes.unsigned_char; 
TYPE Header = RECORD
  (* XXX M3 Compiler implementation problem
     TYPE T is untraced because M3 3.3 prepends type information.
     This only works with structures that are created by the M3 3.3
     run-time and we are dealing with a structure created by the
     network.  Two things need to get fixed:

     1. M3 3.3's assumption that type information is prepended to the
     structure that this REF points to needs to be fixed.  Type
     information should be allocated somewhere else so that M3 3.3 work
     with memory objects not created by its run-time.  I.e., a memory
     object CAST from another language or the network.

     2. TYPE Payload needs to be a special TRACED REF to an internal
     field of a some REF. I.e., the GC needs to know about these special
     REF to work correctly.
  *)

  dhost   : Address;
  shost   : Address;
  type    : EtherType;
END;
TYPE T = Header;

     (* Ethernet Broadcast Address *)
CONST broadcast : Address =Address{ VAL(16_ff, Ctypes.unsigned_char),
                                   VAL(16_ff, Ctypes.unsigned_char),
                                   VAL(16_ff, Ctypes.unsigned_char),
                                   VAL(16_ff, Ctypes.unsigned_char),
                                   VAL(16_ff, Ctypes.unsigned_char),
                                   VAL(16_ff, Ctypes.unsigned_char)};
CONST multicast : Address =Address{ VAL(16_01, Ctypes.unsigned_char),
                                   VAL(16_00, Ctypes.unsigned_char),
                                   VAL(16_53, Ctypes.unsigned_char),
                                   VAL(16_00, Ctypes.unsigned_char),
                                   VAL(16_00, Ctypes.unsigned_char),
                                   VAL(16_00, Ctypes.unsigned_char)};
CONST zero      : Address =Address{  VAL(16_00,Ctypes.unsigned_char),
                                   VAL(16_00,Ctypes.unsigned_char),
                                   VAL(16_00,Ctypes.unsigned_char),
                                   VAL(16_00,Ctypes.unsigned_char),
                                   VAL(16_00,Ctypes.unsigned_char),
                                   VAL(16_00,Ctypes.unsigned_char)};

END StcpEtherPktFormat.

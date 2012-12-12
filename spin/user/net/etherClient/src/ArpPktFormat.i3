(*
 * Copyright 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	ARP header (rfc 826) specification.  
 *	
 *	Created.
 *
 *)

INTERFACE ArpPktFormat;
IMPORT Ctypes;


(*
  Packet format:
  --------------
  16.bit : (ar$hrd) Hardware address space.
  16.bit : (ar$pro) Protocol address space.  For Ethernet
    hardware, this is from the set of type
    fields ether_typ$<protocol>.
  8.bit  : (ar$hln) byte length of each hardware address
  8.bit  : (ar$pln) byte length of each protocol address
  16.bit : (ar$op)  opcode (ares_op$REQUEST | ares_op$REPLY)
  nbytes : (ar$sha) Hardware address of sender of this
    packet, n from the ar$hln field.
  mbytes : (ar$spa) Protocol address of sender of this
    packet, m from the ar$pln field.
  nbytes : (ar$tha) Hardware address of target of this
    packet (if known).
  mbytes : (ar$tpa) Protocol address of target.
*)

CONST
  REQUEST : Ctypes.unsigned_short = 1;
  REPLY   : Ctypes.unsigned_short = 2;

TYPE Header = RECORD
  hrd: BITS 16 FOR Ctypes.unsigned_short; (* 16.bit: (ar$hrd) Hardware address space.                  *)
  pro: BITS 16 FOR Ctypes.unsigned_short; (* 16.bit: (ar$pro) Protocol address space.                  *)
  hln: BITS 8 FOR Ctypes.unsigned_char;  (*  8.bit: (ar$hln) byte length of each hardware address.    *)
  pln: BITS 8 FOR Ctypes.unsigned_char;  (*  8.bit: (ar$pln) byte length of each protocol address.    *)
  op : BITS 16 FOR Ctypes.unsigned_short; (* 16.bit: (ar$op)  opcode (ares_op$REQUEST | ares_op$REPLY) *)
END;

<* OBSOLETE *> TYPE T = UNTRACED REF Header;
TYPE NewT = Header;
CONST HeaderLength = BYTESIZE(Header);
END ArpPktFormat. 

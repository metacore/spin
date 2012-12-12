(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

INTERFACE Ip6PktFormat;
IMPORT Ctypes;
IMPORT Net;

(* Standard well-defined IP protocols.  *)
CONST
  IPPROTO_IP   : Ctypes.unsigned_char =  0;   (* Dummy protocol for TCP               *)
  IPPROTO_ICMP : Ctypes.unsigned_char =  1;   (* Internet Control Message Protocol    *)
  IPPROTO_IGMP : Ctypes.unsigned_char =  2;   (* Internet Gateway Management Protocol *)
  IPPROTO_TCP  : Ctypes.unsigned_char =  6;   (* Transmission Control Protocol        *)
  IPPROTO_EGP  : Ctypes.unsigned_char =  8;   (* Exterior Gateway Protocol            *)
  IPPROTO_PUP  : Ctypes.unsigned_char =  12;  (* PUP protocol                         *)
  IPPROTO_UDP  : Ctypes.unsigned_char =  17;  (* User Datagram Protocol               *)
  IPPROTO_IDP  : Ctypes.unsigned_char =  22;  (* XNS IDP protocol                     *)
  IPPROTO_RAW  : Ctypes.unsigned_char =  255; (* Raw IP packets                       *)
  IPPROTO_ICMP6 : Ctypes.unsigned_char =  58;   (* Internet Control Message Protocol v6   *)

(* IPv6 Extension Headers *)

  IPPROTO_EXT_HOP : Ctypes.unsigned_char =  0;   (* Hop-by-Hop options *)
  IPPROTO_EXT_ROUTING : Ctypes.unsigned_char =  43;   (* Routing header *)
  IPPROTO_EXT_FRAG : Ctypes.unsigned_char =  44;   (* Fragment header *)
  IPPROTO_EXT_DEST : Ctypes.unsigned_char =  60;   (* Destination options *)
  IPPROTO_EXT_NONE : Ctypes.unsigned_char =  59;   (* No Next Header *)

(* 
   Harbison p258: packed types only need to be packed in records,
   objects, and arrays.
*)

TYPE AddressArray = BITS 128 FOR ARRAY [0..15] OF Net.BYTE;
TYPE Address = BITS 128 FOR ARRAY [0..3] OF Ctypes.unsigned_int;


TYPE Header = RECORD
  (* 
     The order of the following two fields are CPU dependent.
     On the alpha these two nibbles need to be swapped.

     prio: BITS 4 FOR [16_0 .. 16_f]; alpha/x86 only 
     vers: BITS 4 FOR [16_0 .. 16_f]; alpha/x86 only
  *)
  prio      : Net.nible;
  vers      : Net.nible;
  flow      : BITS 24 FOR [0..16_ffffff];
  payload   : BITS 16 FOR Ctypes.unsigned_short;
  next_head : BITS 8 FOR Ctypes.unsigned_char;
  hop_limit : BITS 8 FOR Ctypes.unsigned_char;
  (* 32-bit aligned to be compatible with mbuf data *) 
  saddr     : ALIGNED 32 FOR Address;
  daddr     : ALIGNED 32 FOR Address;
END;
TYPE T = Header;


(* Extension Headers *)

(* fragment header *)
TYPE FragmentHeader = RECORD
  next_head : BITS 8 FOR Ctypes.unsigned_char;
  reserved  : BITS 8 FOR Ctypes.unsigned_char;
  offset    : BITS 16 FOR Ctypes.unsigned_short;
  id        : BITS 32 FOR Ctypes.unsigned_int;
END;


(* overlay header used for icmp, tcp, udp checksums *)  
TYPE OverlayHeader = RECORD
  saddr          : ALIGNED 32 FOR Address;
  daddr          : ALIGNED 32 FOR Address;
  paylen         : BITS 32 FOR Ctypes.unsigned_int;
  zero_next_head : BITS 32 FOR Ctypes.unsigned_int;
END;

END Ip6PktFormat.

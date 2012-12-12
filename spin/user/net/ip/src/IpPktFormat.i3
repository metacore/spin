(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 01-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Added Ip header overlay used to compute tcp/udp checksum.
 *	
 * 15-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE IpPktFormat;
IMPORT Ctypes;
IMPORT Net;

CONST 
  IP_CE     : Ctypes.unsigned_short = 16_8000; (* flag: congestion *)
  IP_DF     : Ctypes.unsigned_short = 16_4000; (* flag: donot frament *)
  IP_MF     : Ctypes.unsigned_short = 16_2000; (* flag: more fragment *)
  IP_OFFSET : Ctypes.unsigned_short = 16_1FFF; (* Fragment offset *)

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


(* 
   Harbison p258: packed types only need to be packed in records,
   objects, and arrays.
*)

TYPE AddressArray = BITS 32 FOR ARRAY [0..3] OF Net.BYTE;
TYPE Address = BITS 32 FOR Ctypes.unsigned_int;

TYPE Header = RECORD
  (* 
     The order of the following two fields are CPU dependent.
     On the alpha these two nibbles need to be swapped.

     hlen: BITS 4 FOR [16_0 .. 16_f]; alpha/x86 only 
     vers: BITS 4 FOR [16_0 .. 16_f]; alpha/x86 only
  *)
  hlen: Net.nible;
  vers: Net.nible;
  tos       : BITS 8 FOR Ctypes.unsigned_char;
  tot_len   : BITS 16 FOR Ctypes.unsigned_short;
  id        : BITS 16 FOR Ctypes.unsigned_short;
  frag_off  : BITS 16 FOR Ctypes.unsigned_short;
  ttl       : BITS 8 FOR Ctypes.unsigned_char;
  protocol  : BITS 8 FOR Ctypes.unsigned_char;
  check     : BITS 16 FOR Ctypes.unsigned_short;
  saddr     : ALIGNED 32 FOR Address;
  daddr     : ALIGNED 32 FOR Address;
END;

(*
 * Overlay for ip header used by other protocols (tcp, udp).
 * For 64-bit pointers, the next and previous fields are stored in the
 * mbuf header instead of in the ipovly.
 *)

TYPE  OverlayHeader = RECORD
  (* make same size as basic IP header *)
  fill      : ARRAY [0..1] OF BITS 32 FOR Ctypes.unsigned_int;
  ih_x1     : BITS 8 FOR Ctypes.unsigned_char;
  ih_pr     : BITS 8 FOR Ctypes.unsigned_char;  
  ih_len    : BITS 16 FOR Ctypes.unsigned_short;
  ih_src    : ALIGNED 32 FOR Address;
  ih_dst    : ALIGNED 32 FOR Address;
END;

TYPE T = Header;
END IpPktFormat.

(*
  Copyright 1994, University of Washington
  All rights reserved.
  See COPYRIGHT file for a full description

  HISTORY
*)

INTERFACE Icmp6PktFormat;
IMPORT EtherPktFormat, Ip6PktFormat;
IMPORT Ctypes;

(* RFC 1885 *)

CONST 
(* Neighbor Discovery Message Types *)
(* ICMPv6 error messages *)

  DEST_UNREACH       : Ctypes.unsigned_char = 1; (* Destination Unreachable      *)
  PACKET_TOO_BIG     : Ctypes.unsigned_char = 2; (* Packet Too Big               *)
  TIME_EXCEEDED      : Ctypes.unsigned_char = 3; (* Time Exceeded                *)
  PARAMETERPROB      : Ctypes.unsigned_char = 4; (* Parameter Problem            *)

(* ICMPv6 informational messages *)

  ECHO               : Ctypes.unsigned_char = 128; (* Echo Request               *)
  ECHOREPLY          : Ctypes.unsigned_char = 129; (* Echo Reply                 *)
  GROUP_MEM_QUERY    : Ctypes.unsigned_char = 130; (* Group Membership Query     *)
  GROUP_MEM_REPORT   : Ctypes.unsigned_char = 131; (* Group Membership Report    *)
  GROUP_MEM_REDUCT   : Ctypes.unsigned_char = 132; (* Group Membership Reduction *)

(* ICMPv6 Neighbor Discovery messages *)

  ROUTER_SOLICT      : Ctypes.unsigned_char = 133; (* Router Solicitation        *)
  ROUTER_ADV         : Ctypes.unsigned_char = 134; (* Router Advertisement       *)
  NEIGHBOR_SOLICT    : Ctypes.unsigned_char = 135; (* Neighbor Solicitation      *)
  NEIGHBOR_ADV       : Ctypes.unsigned_char = 136; (* Neighbor Advertisement     *)
  REDIRECT           : Ctypes.unsigned_char = 137; (* Redirect                   *)

(* Codes for Dest. Unreach. *)  
  NO_ROUTE           : Ctypes.unsigned_char = 0;   (* No route to destination    *)
  COMM_PROHIBITED    : Ctypes.unsigned_char = 1;   (* Communication prohibited   *)
  NOT_NEIGHBOR       : Ctypes.unsigned_char = 2;   (* Not a neighbor             *)
  ADDR_UNREACH       : Ctypes.unsigned_char = 3;   (* Addr unreachable           *)
  PORT_UNREACH       : Ctypes.unsigned_char = 4;   (* Port unreachable           *)

(* Codes for Param Prob. *)  
  ERR_HDR_FIELD      : Ctypes.unsigned_char = 0;   (* Erroneous header field     *)
  ERR_NEXT_HDR       : Ctypes.unsigned_char = 0;   (* Unrecognized next header   *)
  ERR_OPTION         : Ctypes.unsigned_char = 0;   (* Unrecognized IPv6 option   *)

(* XXX
  (* Codes for UNREACH. *)
  NET_UNREACH        : Ctypes.unsigned_char = 0; (* Network Unreachable          *)
  HOST_UNREACH       : Ctypes.unsigned_char = 1; (* Host Unreachable             *)
  PROT_UNREACH       : Ctypes.unsigned_char = 2; (* Protocol Unreachable         *)
  PORT_UNREACH       : Ctypes.unsigned_char = 3; (* Port Unreachable             *)
  FRAG_NEEDED        : Ctypes.unsigned_char = 4; (* Fragmentation Needed/DF set  *)
  SR_FAILED          : Ctypes.unsigned_char = 5; (* Source Route failed          *)
  NET_UNKNOWN        : Ctypes.unsigned_char = 6;
  HOST_UNKNOWN       : Ctypes.unsigned_char = 7;
  HOST_ISOLATED      : Ctypes.unsigned_char = 8;
  NET_ANO            : Ctypes.unsigned_char = 9;
  HOST_ANO           : Ctypes.unsigned_char = 10;
  NET_UNR_TOS        : Ctypes.unsigned_char = 11;
  HOST_UNR_TOS       : Ctypes.unsigned_char = 12;
  
  (* Codes for REDIRECT. *)
  REDIR_NET          : Ctypes.unsigned_char = 0; (* Redirect Net                 *)
  REDIR_HOST         : Ctypes.unsigned_char = 1; (* Redirect Host                *)
  REDIR_NETTOS       : Ctypes.unsigned_char = 2; (* Redirect Net for TOS         *)
  REDIR_HOSTTOS      : Ctypes.unsigned_char = 3; (* Redirect Host for TOS        *)
  
  (* Codes for TIME_EXCEEDED. *)
  EXC_TTL            : Ctypes.unsigned_char = 0; (* TTL count exceeded           *)
  EXC_FRAGTIME       : Ctypes.unsigned_char = 1; (* Fragment Reass time exceeded *)
XXX *)
  (* 
     Harbison p258: packed types only need to be packed in records,
     objects, and arrays.
  *)

TYPE Header = RECORD
  type: BITS 8 FOR Ctypes.unsigned_char;
  code: BITS 8 FOR Ctypes.unsigned_char;
  csum: BITS 16 FOR Ctypes.unsigned_short;
END;

TYPE T = Header;

(* RFC 1970 *)

CONST 

(* Option headers *)

TYPE PacketTooBig = RECORD
  MTU: BITS 32 FOR Ctypes.unsigned_int;
END;

TYPE ParamProblem = RECORD
  pointer: BITS 32 FOR Ctypes.unsigned_int;
END;

TYPE LinkAddress = RECORD
  type: BITS 8 FOR Ctypes.unsigned_char;
  len : BITS 8 FOR Ctypes.unsigned_char;
  (* XXX addr should be variable len to be compatible with all link layers *)
  (* it is now set to a fixed 48-bit ethernet addr space *)
  addr: EtherPktFormat.Address;

END;

(* ND message types *)
TYPE NeighborSolicitation = RECORD
  reserved: BITS 32 FOR Ctypes.unsigned_int;  (* first 3 bits are flags, the rest need to be set to zero *)
  addr    : ALIGNED 32 FOR Ip6PktFormat.Address;
  options : ALIGNED 32 FOR LinkAddress;

END;

TYPE NeighborAdvertisement= RECORD
  reserved: BITS 32 FOR Ctypes.unsigned_int;  (* first 3 bits are flags, the rest need to be set to zero *)
  addr    : ALIGNED 32 FOR Ip6PktFormat.Address;
  options : ALIGNED 32 FOR LinkAddress;

END;

END Icmp6PktFormat.





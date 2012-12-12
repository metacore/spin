(*
 * Copyright 1994 - 1996 , University of Washington
 *  All rights reserved.
 *  See COPYRIGHT file for a full description
 * 
 * HISTORY
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new spin shell command style.
 *
*)

INTERFACE IcmpDefault;
IMPORT Ctypes;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "icmpdefault";
      CommandHelp = "-- -debug level| -packets";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

CONST 
  ECHOREPLY          : Ctypes.unsigned_char = 0; (* Echo Reply                   *)
  DEST_UNREACH       : Ctypes.unsigned_char = 3; (* Destination Unreachable      *)
  SOURCE_QUENCH      : Ctypes.unsigned_char = 4; (* Source Quench                *)
  REDIRECT           : Ctypes.unsigned_char = 5; (* Redirect (change route)      *)
  ECHO               : Ctypes.unsigned_char = 8; (* Echo Request                 *)
  TIME_EXCEEDED      : Ctypes.unsigned_char = 11;(* Time Exceeded                *)
  PARAMETERPROB      : Ctypes.unsigned_char = 12;(* Parameter Problem            *)
  TIMESTAMP          : Ctypes.unsigned_char = 13;(* Timestamp Request            *)
  TIMESTAMPREPLY     : Ctypes.unsigned_char = 14;(* Timestamp Reply              *)
  INFO_REQUEST       : Ctypes.unsigned_char = 15;(* Information Request          *)
  INFO_REPLY         : Ctypes.unsigned_char = 16;(* Information Reply            *)
  ADDRESSMASK        : Ctypes.unsigned_char = 17;(* Address Mask Request         *)
  ADDRESSMASKREPLY   : Ctypes.unsigned_char = 18;(* Address Mask Reply           *)
  
  
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

  (* 
     Harbison p258: packed types only need to be packed in records,
     objects, and arrays.
  *)

TYPE Header = RECORD
  type: Ctypes.unsigned_char;
  code: Ctypes.unsigned_char;
  csum: Ctypes.unsigned_short;
END;

TYPE T = UNTRACED REF Header;

TYPE EchoRequestReply = RECORD
  id: BITS 16 FOR Ctypes.unsigned_short;
  seq: BITS 16 FOR Ctypes.unsigned_short;
  (* optional data *)
END;

TYPE DestinationUnreachable = RECORD
END;

TYPE SourceQuench = RECORD
END;

TYPE Redirect = RECORD
END;

TYPE TimeExceededForDatagram = RECORD
END;

TYPE ParameterProblemOnDatagram = RECORD
END;

TYPE TimestampRequestReply = RECORD
END;

TYPE InformationRequestReply = RECORD
END;

TYPE AddressMaskRequestReply = RECORD
END;

PROCEDURE Init(verbose:BOOLEAN);

END IcmpDefault.

(*
 * HISTORY
 *)
 
INTERFACE Incarnation;
IMPORT Ctypes, IpPktFormat;
IMPORT Word;

TYPE T = RECORD 
  lport : BITS 16 FOR Ctypes.unsigned_short;
  rport : BITS 16 FOR Ctypes.unsigned_short;
  laddr : IpPktFormat.Address;
  raddr : IpPktFormat.Address;
END;

(* stuff for hash table generic *)
CONST Brand = "Key";
PROCEDURE Equal(READONLY k1: T; READONLY k2: T): BOOLEAN;
PROCEDURE Hash(READONLY k: T): Word.T;

END Incarnation.

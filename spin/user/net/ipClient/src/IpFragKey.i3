(*
 * HISTORY
 * 22-Apr-96  Trent Piepho (tap) at the University of Washington
 *	Created.
 *
 *)

INTERFACE IpFragKey;
IMPORT Ctypes;
IMPORT Word;

(* 
 * For Generic table 
 *)
TYPE Key = RECORD
  id        : Ctypes.unsigned_short;
  saddr     : Ctypes.unsigned_int;
  daddr     : Ctypes.unsigned_int;
  protocol  : Ctypes.unsigned_short;
END;
TYPE T = Key;
PROCEDURE Equal(READONLY pa1: T; READONLY pa2: T): BOOLEAN;
PROCEDURE Hash (READONLY pa: T): Word.T;
CONST Brand = "IpFragKey";
END IpFragKey.

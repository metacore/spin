(*
 * HISTORY
 * 29-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)
 
INTERFACE Key;
IMPORT Ctypes;
IMPORT Word;
TYPE T = Ctypes.unsigned_short;
PROCEDURE Equal(k1: T; k2: T): BOOLEAN;
PROCEDURE Hash(k: T): Word.T;
CONST Brand = "Key";
END Key.

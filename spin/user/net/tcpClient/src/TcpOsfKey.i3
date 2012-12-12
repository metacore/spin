(*
 * HISTORY
 * 29-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)
 
INTERFACE TcpOsfKey;
IMPORT Ctypes;
IMPORT Word;

TYPE T = Ctypes.unsigned_short;
CONST Brand = "TcpOsfKey";

PROCEDURE Equal(READONLY k1, k2: T): BOOLEAN;
PROCEDURE Hash(READONLY k: T): Word.T;

END TcpOsfKey.

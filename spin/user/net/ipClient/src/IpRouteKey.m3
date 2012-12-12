(* 
 * HISTORY
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE IpRouteKey;
IMPORT Word;
IMPORT Ctypes;

(* 
 * For Generic table
 *)
PROCEDURE Equal(rt1,rt2: T): BOOLEAN =
  BEGIN
    RETURN rt1 = rt2;
  END Equal;

PROCEDURE Hash(route: T): Word.T = 
  BEGIN
    RETURN VIEW(route,BITS 32 FOR Ctypes.unsigned_int);
  END Hash;

BEGIN
END IpRouteKey.

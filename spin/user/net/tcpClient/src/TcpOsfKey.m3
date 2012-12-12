(* 
 * HISTORY
 * 29-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE TcpOsfKey;
IMPORT Word;

PROCEDURE Equal(READONLY k1, k2: T): BOOLEAN =
  BEGIN
    RETURN k1 = k2;
  END Equal;

PROCEDURE Hash(READONLY k: T): Word.T =
  BEGIN
    RETURN k;
  END Hash;

BEGIN
END TcpOsfKey.

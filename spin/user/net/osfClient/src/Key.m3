(* 
 * HISTORY
 * 29-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE Key;
IMPORT Word;

PROCEDURE Equal(k1, k2: T): BOOLEAN =
  BEGIN
    RETURN k1 = k2;
  END Equal;

PROCEDURE Hash(k: T): Word.T =
  BEGIN
    RETURN k;
  END Hash;

BEGIN
END Key.




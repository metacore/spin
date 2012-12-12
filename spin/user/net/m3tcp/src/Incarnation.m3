(* 
 * HISTORY
 * 29-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE Incarnation;
IMPORT Word;

PROCEDURE Equal(READONLY userKey, tableKey: T): BOOLEAN =
  BEGIN
    RETURN 
      userKey.lport = tableKey.lport AND
      userKey.rport = tableKey.rport AND
      userKey.raddr = tableKey.raddr;
  END Equal;

PROCEDURE Hash(READONLY key: T): Word.T =
  BEGIN
    RETURN key.lport + key.rport;
  END Hash;

BEGIN
END Incarnation.




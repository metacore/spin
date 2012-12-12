(* 
 * HISTORY
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE NfsKey;
IMPORT Word;
IMPORT Text;
(* FOR GENERIC TABLE *)

PROCEDURE Equal(READONLY rt1: T; READONLY rt2: T): BOOLEAN =
  BEGIN
    RETURN rt1.fh = rt2.fh AND Text.Equal(rt1.name,rt2.name);
  END Equal;

PROCEDURE Hash(READONLY rt: T): Word.T = 
  VAR hash : Word.T := 0;
  BEGIN
    (* XXX make this faster *)
    FOR i := FIRST(rt.fh.data) TO LAST(rt.fh.data) DO
      hash := hash + ORD(rt.fh.data[i]);
    END;
    RETURN hash;
  END Hash;

BEGIN
END NfsKey.

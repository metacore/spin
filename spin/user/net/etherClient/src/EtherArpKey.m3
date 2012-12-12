(*
 * HISTORY
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE EtherArpKey;
IMPORT Word;

(* 
 * For Generic table
 *)

PROCEDURE Equal(pa1,pa2: T): BOOLEAN =
  BEGIN 
    RETURN pa1 = pa2;
    (*
    RETURN pa1.protocolAddress = pa2.protocolAddress AND
    pa1.proType = pa2.proType; 
    *)
  END Equal;

PROCEDURE Hash(pa: T): Word.T = 
  BEGIN 
    RETURN pa;
    (*
    RETURN VIEW(pa.protocolAddress,BITS 32 FOR Ctypes.unsigned_int); 
    *)
  END Hash;

BEGIN
END EtherArpKey.

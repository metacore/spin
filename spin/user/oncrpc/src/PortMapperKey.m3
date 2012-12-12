(*
 * HISTORY
 * 23-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

MODULE PortMapperKey;
IMPORT Word;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN a.prog = b.prog AND a.prot = b.prot;
  END Equal;

PROCEDURE Hash(a: T): Word.T =
  BEGIN
    RETURN a.prog;
  END Hash;

PROCEDURE Compare(a, b: T): [-1..1] =
  BEGIN
    IF a.prog < b.prog THEN RETURN -1;
    ELSIF a.prog = b.prog THEN
    ELSE RETURN 1;
    END;
  END Compare;

BEGIN
END PortMapperKey.

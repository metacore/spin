MODULE DhtIP6;
IMPORT Word;

PROCEDURE Equal(READONLY pa1: T; READONLY pa2: T): BOOLEAN =
  BEGIN
    RETURN pa1 = pa2;
  END Equal;

PROCEDURE Hash (READONLY pa: T): Word.T = 
  BEGIN
    (* use the low 32 bits of the 6 address *)
    RETURN pa[LAST(pa)];
  END Hash;

BEGIN
END DhtIP6.

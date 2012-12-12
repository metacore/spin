(* 
 * HISTORY
 * 22-Apr-96  Trent Piepho (tap) at the University of Washington
 *	Created.
 *
 *)

MODULE IpFragKey;
IMPORT Word;

PROCEDURE Equal(READONLY ft1,ft2: T): BOOLEAN =
  BEGIN
    RETURN ((ft1.id = ft2.id) AND (ft1.saddr = ft1.saddr) AND
            (ft1.daddr = ft2.daddr) AND (ft1.protocol = ft2.protocol));
  END Equal;

PROCEDURE Hash(READONLY frag: T): Word.T = 
  BEGIN
    RETURN frag.id;
  END Hash;

BEGIN
END IpFragKey.

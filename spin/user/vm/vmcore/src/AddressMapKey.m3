(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *)

MODULE AddressMapKey;

(* Returns TRUE if the two entries overlap, FALSE otherwise *)
PROCEDURE Equal(READONLY me1, me2: T): BOOLEAN =
  BEGIN
    IF me1.from >= me2.end OR
       me2.from >= me1.end THEN
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Equal;

PROCEDURE Hash(me: T): CARDINAL =
  BEGIN
    RETURN me.from;
  END Hash;

PROCEDURE Compare(READONLY me1, me2: T): [-1 .. +1] =
  BEGIN
    IF me1.from >= me2.end THEN
      RETURN +1;
    ELSE
      IF me2.from >= me1.end THEN
        RETURN -1;
      ELSE
        RETURN 0;
      END;
    END;
  END Compare;

BEGIN
END AddressMapKey.



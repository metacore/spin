(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *)
MODULE Auxv;
IMPORT IO;

PROCEDURE Init (VAR auxv: ARRAY OF T) =
  BEGIN
    FOR i := 0 TO LAST(auxv) DO
      auxv[i].type := AT_NULL;
    END;
  END Init;
  
PROCEDURE AddInt (VAR auxv: ARRAY OF T; type: Type; data: INTEGER) =
  VAR i: CARDINAL := 0;
  BEGIN
    WHILE i < LAST(auxv) DO
      IF auxv[i].type = AT_NULL THEN
	auxv[i].type := type;
	auxv[i].data := data;
	RETURN;
      END;
      INC(i);
    END;
    IO.Put("too many auxvs.\n");
    <*ASSERT FALSE*>
  END AddInt;

PROCEDURE AddText (VAR auxv: ARRAY OF T; type: Type; string: TEXT) =
  VAR i: CARDINAL := 0;
  BEGIN
    WHILE i < LAST(auxv) DO
      IF auxv[i].type = AT_NULL THEN
	auxv[i].type := type;
	auxv[i].string := string;
	RETURN;
      END;
      INC(i);
    END;
    IO.Put("too many auxvs.\n");
    <*ASSERT FALSE*>
  END AddText;


BEGIN
END Auxv.

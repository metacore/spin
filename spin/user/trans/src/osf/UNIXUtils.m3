(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE UNIXUtils;
IMPORT Extern;
IMPORT M3toC;
IMPORT Text;
PROCEDURE Tokenize (line: TEXT): REF ARRAY OF TEXT =
  VAR
    begin:= 0;
    i := 0;
    len  := Text.Length(line);
    argv := NEW(REF ARRAY OF TEXT, len);
    argc := 0;
    ch: CHAR;
    tmp: REF ARRAY OF TEXT;
  BEGIN
    LOOP 
      WHILE i < len AND Text.GetChar(line, i) = ' ' DO INC(i); END;
      IF i >= len THEN EXIT; END;
      
      begin := i;
      ch := Text.GetChar(line, i);
      IF ch = '"' THEN
	INC(i);
	WHILE i < len AND Text.GetChar(line, i) # '"' DO INC(i); END;
      ELSE 
	WHILE i < len AND Text.GetChar(line, i) # ' ' DO INC(i); END;
      END;
      argv[argc] := Text.Sub(line, begin, i-begin);
      INC(argc);
      INC(i);
    END;
    tmp := NEW(REF ARRAY OF TEXT, argc);
    tmp^ := SUBARRAY(argv^, 0, argc);
    RETURN tmp;
  END Tokenize;


PROCEDURE Perror (t: TEXT) =
  BEGIN
    Extern.perror(M3toC.TtoS(t));
  END Perror;
  
BEGIN
END UNIXUtils.

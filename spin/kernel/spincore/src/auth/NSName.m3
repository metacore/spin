(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE NSName;
IMPORT Text;
IMPORT Word;

PROCEDURE FromText (txt: TEXT): T =
  VAR name: T;
  BEGIN
    name.from := 0;
    name.end := Text.Length(txt);
    name.str := NEW(REF ARRAY OF CHAR, name.end);
    Text.SetChars(name.str^, txt);
    RETURN name;
  END FromText;

PROCEDURE ToText (READONLY name: T): TEXT =
  BEGIN
    RETURN Text.FromChars(SUBARRAY(name.str^, name.from, name.end-name.from));
  END ToText;
  
PROCEDURE FromRefArrayChar (str: REF ARRAY OF CHAR): T =
  VAR name: T;
  BEGIN
    name.from := 0;
    name.end := NUMBER(str^);
    name.str := str;
    RETURN name;
  END FromRefArrayChar;
  
PROCEDURE FromArrayChar (READONLY str: ARRAY OF CHAR): T =
  VAR name: T;
  BEGIN
    name.from := 0;
    name.end := NUMBER(str);
    name.str := NEW(REF ARRAY OF CHAR, name.end);
    name.str^ := str;
    RETURN name;
  END FromArrayChar;

PROCEDURE DeepCopy (READONLY src: T): T =
  VAR dest: T;
  BEGIN
    dest.str := NEW(REF ARRAY OF CHAR, src.end-src.from);
    dest.str^ := SUBARRAY(src.str^, src.from, src.end-src.from);
    dest.from := 0;
    dest.end := src.end-src.from;
    RETURN dest;
  END DeepCopy;


PROCEDURE Hash (READONLY name: T): INTEGER =
  VAR
    h := 0;
    itr := MIN(name.end-1, name.from+3);
  BEGIN
    FOR i := name.from TO itr DO
      h := Word.Plus(Word.LeftShift(h, 8), ORD(name.str[i]));
    END;
    RETURN h;
  END Hash;

  
PROCEDURE Equal (READONLY a, b: T): BOOLEAN =
  VAR len := a.end-a.from;
  BEGIN
    IF len # b.end-b.from THEN RETURN FALSE;
    ELSE
      FOR i := 0 TO len-1 DO
	IF a.str[i + a.from] # b.str[i + b.from] THEN
	  RETURN FALSE;
	END;
      END;
      RETURN TRUE;
    END;
  END Equal;

  

BEGIN
END NSName.

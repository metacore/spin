(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Eliminated warnings.
 * 08-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *)
MODULE Od EXPORTS CoreCommands;
IMPORT Commands;
IMPORT ParseParams;
IMPORT Fmt;
IMPORT File, FileSystem;
IMPORT IO;
IMPORT Text;
IMPORT Scan;
IMPORT Error, Lex;

(* 
   Od: hexadecimal dump 
   
   od [-n NBYTES] [-h FROM] FILE

   NBYTES specifies the number of bytes to be dumped. Default is 128 bytes.
   FROM specifies the starting offset from which the file is dumped.
   Default is 0.

   *)
PROCEDURE Od (file: TEXT; head, nBytes: INTEGER) =
  VAR
    fp: File.T;
    pos: INTEGER;
    buf: ARRAY [0 .. 1023] OF CHAR;
    n, m: INTEGER;
  BEGIN
    TRY
      fp := FileSystem.Open(file);
      pos := head;
      WHILE pos < head + nBytes DO
	n := fp.read(buf, pos);
	m := 0;
	n := MIN(head+nBytes-pos, n);
	n := MIN(n, LAST(buf));
	FOR i := 0 TO n DO
	  IF m MOD 16 = 0 THEN
	    IO.Put("\n");
	    IO.Put(Fmt.Pad(Fmt.Int(pos+m, 16), 8) & " : ");
	  END;
	  IO.Put(Fmt.Pad(Fmt.Int(ORD(buf[i]), 16), 2, '0'));
	  IF ORD(buf[i]) >= 32 AND ORD(buf[i]) < 128 THEN
	  IO.Put(" " & Text.FromChar(buf[i]) & " ");
	  ELSE
	    IO.Put("   ");
	  END;
	  INC(m);
	END;
	INC(pos, n);
	IF n < NUMBER(buf) THEN EXIT; END;
      END;
    EXCEPT
    | Error.E(e) =>
      IO.PutError("od:" & file & ":" & e.message());
    END;
  END Od;

PROCEDURE ParseInt (s: TEXT): INTEGER =
  BEGIN
    TRY
      IF Text.Length(s) < 2 THEN
	RETURN Scan.Int(s);
      END;
    
      IF Text.Equal(Text.Sub(s, 0, 2), "0x") THEN
	RETURN Scan.Int(Text.Sub(s, 2), 16);
      ELSE
	RETURN Scan.Int(s);
      END;
    EXCEPT
    | Lex.Error =>
      IO.PutError("od: \"" & s & "\" cannot be parsed as a number.");
      RETURN 0;
    END;
  END ParseInt;
  
PROCEDURE Run (<*UNUSED*>c: REFANY; pp : ParseParams.T) : BOOLEAN =
  VAR
    nBytes := 128;
    head := 0;
    file: TEXT;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
    
      LOOP
	IF pp.testNext("-n") THEN
	  nBytes := ParseInt(pp.getNext());
	ELSIF pp.testNext("-h") THEN
	  head := ParseInt(pp.getNext());
	ELSE
	  EXIT;
	END;
      END;
      file := pp.getNext();
      Od(file, head, nBytes);
    EXCEPT
    | ParseParams.Error =>
      IO.PutError("od [-n NBYTES] [-h FROM] file");
    END;
    RETURN TRUE;
  END Run;

BEGIN
  EVAL Commands.Install(Run, "od", " [-n NBYTES] [-h FROM] file");
END Od.

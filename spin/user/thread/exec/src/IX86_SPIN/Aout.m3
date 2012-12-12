(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Coff definitions. The names correspond to C equivalents.
 *)
MODULE Aout; 
IMPORT Word;

PROCEDURE NetToHostLong(n: Word.T) : Word.T = 
  VAR
    r: Word.T;
  BEGIN
    WITH nArray = VIEW(n, ARRAY [1..4] OF CHAR) DO
      WITH rArray = VIEW(r, ARRAY [1..4] OF CHAR) DO
        rArray[1] := nArray[4];
        rArray[2] := nArray[3];
        rArray[3] := nArray[2];
        rArray[4] := nArray[1];
      END;
    END;
    
    RETURN r;
  END NetToHostLong;

PROCEDURE N_BADMAG(READONLY a: AoutHeader) : BOOLEAN =
  VAR
  magic: Word.T;
  magicNet: Word.T;
  BEGIN
    magic := Word.And(a.a_midmag, 16_ffff);
    magicNet := Word.And(NetToHostLong(a.a_midmag), 16_ffff);

    RETURN (magic # OMAGIC AND magic # NMAGIC AND magic # ZMAGIC AND
            magic # QMAGIC AND magicNet # OMAGIC AND magicNet # NMAGIC AND 
            magicNet # ZMAGIC AND magicNet # QMAGIC);
  END N_BADMAG;

PROCEDURE N_TXTOFF(READONLY a: AoutHeader) : INTEGER = 
  VAR
    magic: Word.T;
    magicNet: Word.T;
  BEGIN
    magic := Word.And(a.a_midmag, 16_ffff);
    magicNet := Word.And(NetToHostLong(a.a_midmag), 16_ffff);
    
    IF magic = ZMAGIC THEN
      RETURN 4096;
    ELSIF magic = QMAGIC OR magicNet = ZMAGIC THEN
      RETURN 0;
    ELSE
      RETURN BYTESIZE(AoutHeader);
    END;

  END N_TXTOFF;

PROCEDURE N_TXTADDR(READONLY a: AoutHeader) : INTEGER =
  VAR
    magic: Word.T;
  BEGIN
    magic := Word.And(a.a_midmag, 16_ffff);

    IF magic = OMAGIC OR magic = NMAGIC OR magic = ZMAGIC THEN
      RETURN 0;
    ELSE
      RETURN 4096;
    END;
  END N_TXTADDR;


BEGIN
END Aout.

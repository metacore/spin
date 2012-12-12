(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Replaced heap insane code with saner versions.
 *	Moved the truly unsafe interfaces to UnsafeConvert.
 *      Replaced use of unsafe external atoi with safe reader/writer code.
 *      Got rid of redundant interfaces.
 *
 * 30-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	added BytesToArrayOfChar.
 *	
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Created.  Unsafe conversion facilities go here.
 *
 * This module is unsafe because it has to be.
 *)
UNSAFE MODULE SafeConvert;
IMPORT M3toC, Ctypes, Lex, TextRd, Rd, Word;

PROCEDURE RefAnyToWord(ra: REFANY): Word.T =
  BEGIN
    RETURN LOOPHOLE(ra, Word.T);
  END RefAnyToWord;

PROCEDURE AdrToWord(ra: ADDRESS): Word.T =
  BEGIN
    RETURN LOOPHOLE(ra, Word.T);
  END AdrToWord; 

PROCEDURE TextToString(t: TEXT;
                       proc: PROCEDURE(str:Ctypes.char_star; arg:REFANY):REFANY;
                       arg: REFANY) : REFANY =
  VAR
    string: Ctypes.char_star;
    ret: REFANY;
  BEGIN
    string := M3toC.CopyTtoS(t);
    TRY 
      ret := proc(string, arg);
    FINALLY 
      M3toC.FreeCopiedS(string);
    END;
    RETURN ret;
  END TextToString;

(* Atoi returns 0 upon error *)
PROCEDURE Atoi(READONLY nptr: TEXT) : INTEGER =
  VAR
    trd: TextRd.T;
    val: INTEGER;
  BEGIN
    trd := TextRd.New(nptr);
    TRY
      val := Lex.Int(trd);
      Rd.Close(trd);
    EXCEPT
    ELSE RETURN 0;
    END;
    RETURN val;
  END Atoi;

BEGIN
END SafeConvert.



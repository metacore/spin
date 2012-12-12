(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 21-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Safe conversion routines.
 *)
INTERFACE SafeConvert;
IMPORT Ctypes, Word;

(*
 * It is safe to go from a refany to a word, BUT NOT the other way around!
 *)
PROCEDURE RefAnyToWord(ra: REFANY) : Word.T;

(*
 * It is safe to go from an address to a word, BUT NOT the other way around!
 *)
PROCEDURE AdrToWord(ra: ADDRESS) : Word.T;

(*
 * Pass me a closure, and I'll call it with a C string argument and
 * return its result.
 *)
PROCEDURE TextToString(t: TEXT;
                       proc:PROCEDURE(str:Ctypes.char_star; arg:REFANY):REFANY;
                       arg: REFANY) : REFANY;

PROCEDURE Atoi(READONLY nptr: TEXT) : INTEGER;

END SafeConvert.



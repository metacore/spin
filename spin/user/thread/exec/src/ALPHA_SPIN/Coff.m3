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
MODULE Coff; 
IMPORT Word;

(* Round an integer to a power of two *)
PROCEDURE Round(val: INTEGER; roundto: INTEGER) : INTEGER =
  BEGIN
    RETURN Word.And(val + roundto-1, Word.Not(roundto-1));
  END Round;

PROCEDURE Dynamic (READONLY f: Filehdr): BOOLEAN =
  BEGIN
    IF Word.And(f.f_flags, 16_3000) # 0 THEN
      RETURN TRUE;
    END;
    RETURN FALSE;
  END Dynamic;
  
PROCEDURE N_BADMAG(READONLY a: Aouthdr) : BOOLEAN =
  BEGIN
    RETURN (a.magic # OMAGIC AND a.magic # NMAGIC AND a.magic # ZMAGIC);
  END N_BADMAG;

PROCEDURE N_TXTOFF(READONLY f: Filehdr; READONLY a: Aouthdr) : INTEGER = 
  BEGIN
    IF a.magic = ZMAGIC THEN
      RETURN 0;
    ELSIF Word.And(f.f_flags, F_EXEC) # 0 THEN
      RETURN Round(BYTESIZE(Filehdr) +
                   BYTESIZE(Aouthdr) + 
                   f.f_nscns * BYTESIZE(Scnhdr),
                   SCNROUND);
    ELSE
      RETURN BYTESIZE(Filehdr) + 
             BYTESIZE(Aouthdr) +
             f.f_nscns * BYTESIZE(Scnhdr);
    END;
  END N_TXTOFF;

BEGIN
END Coff.

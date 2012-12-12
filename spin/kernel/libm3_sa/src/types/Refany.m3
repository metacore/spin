(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu May 13 15:54:04 PDT 1993 by swart      *)
(*      modified on Tue Apr 13 11:16:19 PDT 1993 by mcjones    *)

(*
 * HISTORY
 * 11-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Hash returns 0. Someone aware of refheader stuff please devise more
 *	efficient function!
 *
 * 17-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of warnings.
 *
 * 12-Jun-95  Brian Bershad (bershad) at the University of Washington
 *	Added HISTORY
 *
 * 16-Mar-95  Przemek Pardyak (pardy) at the University of Washington
 *	Made it actually usable by casting REFANY-s to Word.T.
 *	Even though we LOOPHOLE traced references to integers,
 *	we are GC-safe because these values will be on stack.  
 *	References that pass this interface should be strongref-ed.
 *) 

UNSAFE MODULE Refany;

IMPORT Word, RT0;

PROCEDURE Equal(r1, r2: T): BOOLEAN = BEGIN RETURN r1 = r2 END Equal;

<* UNUSED *> EXCEPTION Error; (* <*FATAL Error*>*)

(*
PROCEDURE Compare (<*UNUSED*> r1, r2: T): [-1 .. 1] =
  BEGIN
    RAISE Error
  END Compare;
*)

VAR
  hashCnt: INTEGER := 0;

PROCEDURE Hash(r: T): Word.T = 
  VAR
    hash: Word.T;
  BEGIN
    IF RT0.MaxHash = 0 THEN
      RETURN 0;
    END;
    WITH header = LOOPHOLE(LOOPHOLE(r, ADDRESS) - ADRSIZE(RT0.RefHeader),
                           UNTRACED REF RT0.RefHeader) 
     DO
      IF header.hash = 0 THEN
        INC(hashCnt);
        header.hash := Word.And(hashCnt, RT0.MaxHash);
      END;
      hash := Word.Or(Word.Shift(header.typecode, 16), header.hash);
      RETURN hash;
    END;
  END Hash;
  
PROCEDURE Compare (r1, r2: T): [-1 .. 1] =
  VAR
    w1: Word.T := LOOPHOLE(r1, Word.T);
    w2: Word.T := LOOPHOLE(r2, Word.T);
  BEGIN
    IF w1 = w2 THEN
      RETURN 0;
    ELSIF Word.LT(w1,w2) THEN
      RETURN -1;
    ELSE
      RETURN 1;
    END;
  END Compare;

BEGIN
END Refany.


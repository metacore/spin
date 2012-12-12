(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Made safe by eliminating unnecessary loophole.
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 *
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *)

MODULE EID;
IMPORT Word, SafeConvert;

PROCEDURE Equal(k1, k2: T): BOOLEAN =
  BEGIN
    RETURN k1 = k2;
  END Equal;

PROCEDURE Hash(k: T): Word.T =
  BEGIN
    RETURN SafeConvert.AdrToWord(k);
  END Hash;

BEGIN
END EID.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 20-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 06-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Added Brand
 *
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *)
INTERFACE EID;
IMPORT Word;

TYPE T = PROCANY;

CONST Brand = "EID-1.0";

PROCEDURE Equal(k1, k2: T): BOOLEAN;
PROCEDURE Hash(k: T): Word.T;

END EID.

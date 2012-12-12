(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon May 17 22:34:00 PDT 1993 by mjordan    *)

UNSAFE MODULE TypeTable;

IMPORT RefRefTbl, Refany, Word;

REVEAL T = RefRefTbl.Default BRANDED OBJECT
OVERRIDES
    keyEqual := KeyEqual;
    keyHash := KeyHash;
END;

PROCEDURE KeyEqual(<*UNUSED*> t: T; READONLY k1, k2: Refany.T): BOOLEAN=
BEGIN
    RETURN k1 = k2;
END KeyEqual;

PROCEDURE KeyHash(<*UNUSED*> t: T; READONLY k: Refany.T): Word.T=
BEGIN
    RETURN LOOPHOLE(k, Word.T);
END KeyHash;

BEGIN
END TypeTable.


    

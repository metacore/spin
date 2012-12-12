(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransMode;
IMPORT Word;

PROCEDURE WordToT (v: Word.T): Set =
  VAR mode := Set{};
  BEGIN
    IF Word.And(v, 2) # 0 THEN
      mode := mode + Set{T.NoLogging};
    END;
    IF Word.And(v, 4) # 0 THEN
      mode := mode + Set{T.NoLocks};
    END;
    IF Word.And(v, 8) # 0 THEN
      mode := mode + Set{T.PageGrainLogging};
    END;

    RETURN mode;
  END WordToT;
  
BEGIN
END TransMode.

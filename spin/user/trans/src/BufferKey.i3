(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE BufferKey;
IMPORT Word, Storage;

TYPE T = RECORD
END;

CONST Brand = "BufferKey";
PROCEDURE Equal(READONLY k1, k2: T): BOOLEAN;
PROCEDURE Hash(t: T): Word.T;
  
END BufferKey.

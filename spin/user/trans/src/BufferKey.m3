(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added Equal and Hash.
 *)
UNSAFE MODULE BufferKey;
IMPORT Word;

PROCEDURE Equal (READONLY k1, k2: T): BOOLEAN =
BEGIN
  RETURN k1.pos = k2.pos AND k1.st = k2.st;
END Equal;

PROCEDURE Hash (t: T): Word.T =
BEGIN
  RETURN t.st.id*16_100000000 + t.pos;
END Hash;
BEGIN
END BufferKey.

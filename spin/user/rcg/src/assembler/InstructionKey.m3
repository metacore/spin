(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)


UNSAFE MODULE InstructionKey;

IMPORT Word;

(* for hash tables *)

PROCEDURE Hash (i: T) : Word.T =
  BEGIN
    RETURN LOOPHOLE (i, Word.T);
  END Hash;

PROCEDURE Equal (i1, i2: T) : BOOLEAN =
  BEGIN
    RETURN i1 = i2;
  END Equal;

BEGIN
END InstructionKey.

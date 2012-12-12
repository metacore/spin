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


INTERFACE InstructionKey;

IMPORT Instruction, Word;

CONST Brand = "Instruction pointers as values";

TYPE T = Instruction.TPtr;

PROCEDURE Hash (i: T) : Word.T;
PROCEDURE Equal (i1, i2: T) : BOOLEAN;

END InstructionKey.

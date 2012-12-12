(*
 * HISTORY
 * 21-Oct-95  Yasushi Saito (yasushi) at the University of Washington
 *	Implemented some procedures that were originally implemented in
 *	CPP macros.
 *)
MODULE FileDefs;
IMPORT Word;

PROCEDURE btodb(byte_offset: Word.T) : Word.T =
  BEGIN
    RETURN Word.RightShift(byte_offset, 9);
  END btodb;

PROCEDURE dbtob(block_number: Word.T) : Word.T =
  BEGIN
    RETURN Word.LeftShift(block_number, 9);
  END dbtob;

BEGIN
END FileDefs.

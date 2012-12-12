(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE Bitmap;
IMPORT Word;

REVEAL T = BRANDED REF RECORD
  size: CARDINAL;
  map: REF ARRAY OF Word.T;
END;

PROCEDURE Create (size: CARDINAL): T =
  BEGIN
    RETURN NEW(T,
	       size := size,
	       map := NEW(REF ARRAY OF Word.T,
			  (size+BITSIZE(Word.T)) DIV BITSIZE(Word.T)));
  END Create;

PROCEDURE Get (t: T; i: CARDINAL): BOOLEAN =
  VAR
    idx := i DIV BITSIZE(Word.T);
    off := i MOD BITSIZE(Word.T);
  BEGIN
    <*ASSERT i < t.size*>
    RETURN Word.And(t.map[idx], Word.LeftShift(1, off)) # 0;
  END Get;
  
PROCEDURE Set (t: T; i: CARDINAL) =
  VAR
    idx := i DIV BITSIZE(Word.T);
    off := i MOD BITSIZE(Word.T);
  BEGIN
    <*ASSERT i < t.size*>
    t.map[idx] := Word.Or(t.map[idx], Word.LeftShift(1, off));
  END Set;
  
PROCEDURE Reset (t: T; i: CARDINAL) =
  VAR
    idx := i DIV BITSIZE(Word.T);
    off := i MOD BITSIZE(Word.T);
  BEGIN
    <*ASSERT i < t.size*>
    t.map[idx] := Word.And(t.map[idx], Word.Not(Word.LeftShift(1, off)));
  END Reset;

PROCEDURE FindTrue (t: T; VAR i: CARDINAL): BOOLEAN =
  PROCEDURE FindTrueWithinWord(w: Word.T; VAR off: CARDINAL): BOOLEAN =
    BEGIN
      WHILE off < BITSIZE(Word.T) DO
	IF Word.And(w, Word.LeftShift(1, off)) # 0 THEN
	  RETURN TRUE;
	END;
	INC(off);
      END;
      RETURN FALSE;
    END FindTrueWithinWord;
  VAR
    idx : CARDINAL := i DIV BITSIZE(Word.T);
    off : CARDINAL := i MOD BITSIZE(Word.T);
  BEGIN
    INC(off);
    IF FindTrueWithinWord(t.map[idx], off) THEN
      i := idx*BITSIZE(Word.T) + off;
      RETURN i < t.size;
    END;

    INC(idx);
    WHILE idx < LAST(t.map^) DO
      IF t.map[idx] # 0 THEN
	off := 0;
	IF FindTrueWithinWord(t.map[idx], off) THEN
	  i := idx*BITSIZE(Word.T) + off;
	  RETURN i < t.size;
	END;
      ELSE
	(* all bits are 0. *)
      END;
      INC(idx);
    END;
    RETURN FALSE;
  END FindTrue;
    
PROCEDURE FindFalse (t: T; VAR i: CARDINAL): BOOLEAN =
  PROCEDURE FindFalseWithinWord(w: Word.T; VAR off: CARDINAL): BOOLEAN =
    BEGIN
      WHILE off < BITSIZE(Word.T) DO
	IF Word.And(w, Word.LeftShift(1, off)) = 0 THEN
	  RETURN TRUE;
	END;
	INC(off);
      END;
      RETURN FALSE;
    END FindFalseWithinWord;
  VAR
    idx : CARDINAL := i DIV BITSIZE(Word.T);
    off : CARDINAL := i MOD BITSIZE(Word.T);
  BEGIN
    INC(off);
    IF FindFalseWithinWord(t.map[idx], off) THEN
      i := idx*BITSIZE(Word.T) + off;
      RETURN i < t.size;
    END;

    INC(idx);
    WHILE idx < LAST(t.map^) DO
      IF t.map[idx] # -1 THEN
	(* This condition looks weird, but this is the only portable way to
	   check if all bits in the word are set. *)
	off := 0;
	IF FindFalseWithinWord(t.map[idx], off) THEN
	  i := idx*BITSIZE(Word.T) + off;
	  RETURN i < t.size;
	END;
      ELSE
	(* all bits are 1. *)
      END;
      INC(idx);
    END;
    RETURN FALSE;
  END FindFalse;
  
BEGIN
END Bitmap.

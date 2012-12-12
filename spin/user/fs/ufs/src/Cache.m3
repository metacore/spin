(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

MODULE Cache;
IMPORT Word;

CONST
  LineSize = 2;
<*UNUSED*>
CONST
  ConsistencyCheck = FALSE;
  
(* This is set-associative cache with LRU eviction within a line. *)

TYPE Entry =
  RECORD
    mp: REFANY;
    inumber: INTEGER;
    age: INTEGER;
    val: REFANY;
  END;
  
REVEAL T = TPublic BRANDED OBJECT
  table: REF ARRAY OF ARRAY [0 .. LineSize-1] OF Entry;
  mu: MUTEX;
  seq: CARDINAL;
  sizeMask: Word.T; (* sizeMask = NUMBER(table^) - 1. *)
OVERRIDES
  init := Init;
  find := Find;
  insert := Insert;
  purgeEntries := PurgeEntries;
END;

PROCEDURE Init(t: T; size: INTEGER): T =
  BEGIN
    size := size DIV 2; (* it's 2way sa *)
    t.table := NEW(REF ARRAY OF ARRAY [0 .. LineSize-1] OF Entry, size);
    t.mu := NEW(MUTEX);
    t.seq := 0;
    t.sizeMask := size-1;
    RETURN t;
  END Init;
  
PROCEDURE Find (t: T; mp: REFANY; inumber: INTEGER): REFANY =
VAR
  idx := Word.And(inumber, t.sizeMask);
BEGIN
  LOCK t.mu DO
    FOR i := 0 TO LineSize-1 DO
      WITH t = t.table[idx][i] DO
	IF t.val # NIL AND t.mp = mp AND t.inumber = inumber THEN
	  INC(t.age);
	  RETURN t.val;
	END;
      END;
    END;
    RETURN NIL;
  END;
END Find;

PROCEDURE Insert(t: T; val: REFANY; mp: REFANY; inumber: INTEGER) =
VAR
  minAge: CARDINAL := LAST(CARDINAL);
  idx := Word.And(inumber, t.sizeMask);
  lruEntry: [0 .. LineSize-1];
BEGIN
  LOCK t.mu DO
    INC(t.seq);
    FOR i := 0 TO LineSize-1 DO
      WITH e = t.table[idx][i] DO
	IF e.val = NIL THEN
	  t.table[idx][i].val := val;
	  t.table[idx][i].mp := mp;
	  t.table[idx][i].inumber := inumber;
	  t.table[idx][i].age := t.seq;
	  RETURN;
	ELSE
	  IF e.age < minAge THEN
	    minAge := e.age;
	    lruEntry := i;
	  END;
	END;
      END;
    END;
    
    (* All the entries are full. Replace the lru entry *)
    t.table[idx][lruEntry].val := val;
    t.table[idx][lruEntry].mp := mp;
    t.table[idx][lruEntry].inumber := inumber;
    t.table[idx][lruEntry].age := t.seq;
    RETURN;
  END;
END Insert;

PROCEDURE PurgeEntries (t: T; mp: REFANY) =
BEGIN
  FOR idx := 0 TO NUMBER(t.table^)-1 DO 
    FOR i := 0 TO LineSize-1 DO
      WITH e = t.table[idx][i] DO
	IF e.val # NIL AND e.mp = mp THEN
	  t.table[idx][i].val := NIL;
	END;
      END;
    END;
  END;
END PurgeEntries;
  
BEGIN
END Cache.

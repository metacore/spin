(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE FastIntRefTbl;
IMPORT IO;
CONST Size = 32;
  
TYPE
  Entry = RECORD
    key: INTEGER;
    data: REFANY;
    link: REF Entry;
  END;
  
REVEAL
  Default = T BRANDED OBJECT
    bucket: ARRAY [0..Size-1] OF Entry;
  OVERRIDES
    init := Init;
    get := Get;
    put := Put;
    delete := Delete;
    size := SizeMethod;
    iterate := Iterate;
  END;

PROCEDURE Init (t: Default): T =
  BEGIN
    FOR i := FIRST(t.bucket) TO LAST(t.bucket) DO
      t.bucket[i].data := NIL;
      t.bucket[i].link := NIL;
    END;
    RETURN t;
  END Init;

PROCEDURE Get (t: Default; key: INTEGER; VAR v: REFANY): BOOLEAN =
  VAR
    hash := key MOD Size;
    cur: REF Entry;
  BEGIN
    IF t.bucket[hash].data = NIL THEN RETURN FALSE; END;
    
    IF t.bucket[hash].key = key THEN
      v := t.bucket[hash].data;
      RETURN TRUE;
    END;

    cur := t.bucket[hash].link;
    WHILE cur # NIL DO
      IF cur.key = key THEN
	v := cur.data;
	RETURN TRUE;
      END;
      cur := cur.link;
    END;
    RETURN FALSE;
  END Get;
  
PROCEDURE Put (t: Default; key: INTEGER; v: REFANY): BOOLEAN =
  VAR
    hash := key MOD Size;
    e: REF Entry;
  BEGIN
    IF t.bucket[hash].data = NIL THEN
      <*ASSERT t.bucket[hash].link = NIL*>
      t.bucket[hash].data := v;
      t.bucket[hash].key := key;
    ELSE
      e := NEW(REF Entry,
	       key := key,
	       data := v,
	       link := t.bucket[hash].link);
      t.bucket[hash].link := e;
    END;
    RETURN FALSE;
  END Put;

PROCEDURE Delete (t: Default; key: INTEGER; VAR v: REFANY): BOOLEAN =
  VAR
    hash := key MOD Size;
    e: REF Entry;
  BEGIN
    <*ASSERT t.bucket[hash].data # NIL*>
    IF t.bucket[hash].key = key THEN
      v := t.bucket[hash].data;
      e := t.bucket[hash].link;
      IF e = NIL THEN
	t.bucket[hash].data := NIL;
	RETURN TRUE;
      END;
      t.bucket[hash] := e^;

      RETURN TRUE;
    END;

    IO.Put("delete not supported.\n");
    RETURN FALSE;
  END Delete;

PROCEDURE SizeMethod (t: Default): INTEGER =
  VAR
    size := 0;
    e: REF Entry;
  BEGIN
    FOR i := 0 TO Size-1 DO
      IF t.bucket[i].data # NIL THEN
	INC(size);
	e := t.bucket[i].link;
	WHILE e # NIL DO
	  INC(size);
	  e := e.link;
	END;
      END;
    END;
    RETURN size;
  END SizeMethod;
  
PROCEDURE Iterate (t: Default): Iterator =
  VAR itr := NEW(Iterator);
  BEGIN
    itr.idx := 0;
    itr.cur := NIL;
    itr.t := t;
    RETURN itr;
  END Iterate;
  
REVEAL Iterator = IteratorPublic BRANDED OBJECT
  idx: [0 .. Size]; (* when the iterator terminates, "idx = Size". *)
  cur: REF Entry;
  t: Default;
OVERRIDES
  next := Next;
END;

PROCEDURE Next(itr: Iterator; VAR key: INTEGER; VAR v: REFANY): BOOLEAN =
  BEGIN
    IF itr.cur = NIL THEN
      (* look at the bucket "itr.idx" or later and find first nonnull
       buckets *)
      FOR i := itr.idx TO Size-1 DO
	IF itr.t.bucket[i].data # NIL THEN
	  v := itr.t.bucket[i].data;
	  key := itr.t.bucket[i].key;
	  itr.cur := itr.t.bucket[i].link;
	  itr.idx := i+1;
	  RETURN TRUE;
	END;
      END;
      RETURN FALSE;
    ELSE
      key := itr.cur.key;
      v := itr.cur.data;
      itr.cur := itr.cur.link;
      RETURN TRUE;
    END;
  END Next;
  
BEGIN
END FastIntRefTbl.

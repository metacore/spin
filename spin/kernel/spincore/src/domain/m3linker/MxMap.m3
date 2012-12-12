(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxMap.m3                                              *)
(* Last Modified On Tue Mar 15 15:33:19 PST 1994 By kalsow     *)

MODULE MxMap;

REVEAL
  T = BRANDED "MxMap.T" REF RECORD
          n_used : INTEGER := 0;
          data   : Contents;
          linked : MapList := NIL;
        END;

TYPE
  MapList = REF RECORD
    map:  T       := NIL;
    next: MapList := NIL;
  END;

PROCEDURE New (initialSize: CARDINAL): T =
  VAR t := NEW (T);
  BEGIN
    t.data := NEW (Contents, MAX (16, initialSize));
    RETURN t;
  END New;

PROCEDURE AddLinked (t, linked: T) =
  VAR
    ptr, prev, elem  : MapList;
  BEGIN
    ptr := t.linked;
    prev := NIL;
    WHILE ptr # NIL DO
      IF ptr.map = linked THEN
        RETURN;
      END;
      prev := ptr;
      ptr := ptr.next;
    END;
    elem := NEW(MapList, map := linked);
    IF prev = NIL THEN
      t.linked := elem;
    ELSE
      prev.next := elem;
    END;
  END AddLinked;

PROCEDURE Get (t: T;  k: Key): Value =
  VAR ptr: MapList; res: Value;
  BEGIN
    IF t = NIL THEN RETURN NIL; END;

    res := Get1(t, k);
    IF res # NIL THEN RETURN res; END;

    ptr := t.linked;
    WHILE ptr # NIL DO
      res := Get1(ptr.map, k);
      IF res # NIL THEN RETURN res; END;
      ptr := ptr.next;
    END;
    RETURN NIL;
  END Get;

PROCEDURE Get1 (t: T;  k: Key): Value =
  VAR x0, x : INTEGER;
  BEGIN
    IF t = NIL THEN RETURN NIL; END;
    x0 := k MOD NUMBER (t.data^);
    x := x0;
    LOOP
      WITH z = t.data[x] DO
        IF (z.key = k) THEN RETURN z.value END;
        IF (z.value = NIL) THEN EXIT END;
      END;
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0 END;
      IF (x = x0) THEN EXIT END;
    END;
    RETURN NIL;
  END Get1;

PROCEDURE Insert (t: T;  k: Key;  v: Value) =
  VAR x0 := k MOD NUMBER (t.data^);
  VAR x := x0;
  BEGIN
    IF v = NIL THEN RETURN; END;
    LOOP
      WITH z = t.data [x] DO
        IF (z.key = k) THEN (* a new value for an existing key *)
          z.value := v;
          RETURN;
        ELSIF (z.value = NIL) THEN (* an empty hole => insert it here *)
          z.key   := k;
          z.value := v;
          INC (t.n_used);
          IF (2 * t.n_used > NUMBER (t.data^)) THEN Expand (t) END;
          RETURN;
        END;
      END;
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0 END;
      IF (x = x0) THEN (* no free slots *) Expand (t) END;
    END;
  END Insert;

PROCEDURE Expand (t: T; size: INTEGER := 0) =
  VAR old := t.data;
  VAR n   := NUMBER (old^);
  BEGIN
    t.n_used := 0;
    IF size = 0 THEN size := 2 * n; END;
    IF size <= 0 THEN size := 1; END;
    t.data   := NEW (Contents, size);
    FOR i := 0 TO n-1 DO
      WITH z = old[i] DO
        IF (z.value # NIL) THEN Insert (t, z.key, z.value) END;
      END;
    END;
  END Expand;

PROCEDURE FixSize (t: T) =
  BEGIN
    IF t.n_used < NUMBER(t.data^) DIV 2 THEN
      Expand(t, t.n_used);
    END;
  END FixSize;

(* BUG ALERT: LEAVE n_used < 0 *)

PROCEDURE Delete (t: T;  k: Key) =
  VAR x0 := k MOD NUMBER (t.data^);
  VAR x := x0;  v: Value;
VAR i := 0;
  BEGIN
    LOOP
      WITH z = t.data [x] DO
        IF (z.key = k) THEN (* a new value for an existing key *)
          (* this is the value to delete *)
          IF (z.value # NIL) THEN
            z.value := NIL;  
            DEC (t.n_used);
          END;
          EXIT;
        ELSIF (z.value = NIL) THEN
          (* an empty hole => nobody to delete *)
          RETURN;
        END;
      END;
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0 END;
      IF (x = x0) THEN (* no match *) RETURN; END;
    END;

    (* reinsert the elements that may be rehashes of the target *)
    LOOP
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0; END;
      WITH z = t.data[x] DO
        v := z.value;
        IF (v = NIL) THEN (* the end of the rehash chain *) RETURN; END;
        z.value := NIL;  
        DEC (t.n_used);
        i := t.n_used;
        Insert (t, z.key, v);
        IF i # t.n_used -1 THEN 
          INC(t.n_used);
        END;
      END;
    END;

  END Delete;

PROCEDURE GetData (t: T): Contents =
  BEGIN
    IF (t = NIL)
      THEN RETURN NIL
      ELSE RETURN t.data;
    END;
  END GetData;

BEGIN
END MxMap.

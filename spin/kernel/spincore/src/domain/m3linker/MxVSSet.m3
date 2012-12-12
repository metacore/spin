(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxVSSet.m3                                            *)
(* Last Modified On Mon Nov 21 16:51:35 PST 1994 By kalsow     *)

MODULE MxVSSet;

IMPORT Word, MxVS;
(* IMPORT RTIO;*)

REVEAL
  T = BRANDED "MxVSMap.T" REF RECORD
          n_used : INTEGER := 0;
          data   : Contents;
          linked : VSSetList := NIL;
        END;
TYPE
  VSSetList = REF RECORD
    set:  T         := NIL;
    next: VSSetList := NIL;
  END;

PROCEDURE New (initialSize: CARDINAL): T =
  VAR t := NEW (T);
  BEGIN
    t.data := NEW (Contents, MAX (16, initialSize));
    RETURN t;
  END New;

PROCEDURE AddLinked (t, linked: T) =
  VAR
    ptr, prev, elem  : VSSetList;
  BEGIN
    ptr := t.linked;
    prev := NIL;
    WHILE ptr # NIL DO
      IF ptr.set = linked THEN
        RETURN;
      END;
      prev := ptr;
      ptr := ptr.next;
    END;
    elem := NEW(VSSetList, set := linked);
    IF prev = NIL THEN
      t.linked := elem;
    ELSE
      prev.next := elem;
    END;
  END AddLinked;

PROCEDURE Get (t: T;  key: Elt): Elt =
  VAR ptr: VSSetList; res: Elt;
  BEGIN
    IF t = NIL THEN RETURN MxVS.NoVS; END;

    res := Get1(t, key);
    IF res # MxVS.NoVS THEN RETURN res; END;

    ptr := t.linked;
    WHILE ptr # NIL DO
      res := Get1(ptr.set, key);
      IF res # MxVS.NoVS THEN RETURN res; END;
      ptr := ptr.next;
    END;
    RETURN MxVS.NoVS;
  END Get;

PROCEDURE Get1 (t: T;  key: Elt): Elt =
  VAR e: Elt;  k_info, e_info: MxVS.Info;  x, x0: INTEGER;
  BEGIN
    IF t = NIL THEN RETURN MxVS.NoVS; END;
    MxVS.Get (key, k_info);
    x0 := Word.Xor (17 * k_info.symbol, k_info.source) MOD NUMBER (t.data^);
    x := x0;
    LOOP
      e := t.data[x];
      IF (e = MxVS.NoVS) THEN EXIT; END;
      MxVS.Get (e, e_info);
      IF (e_info.symbol = k_info.symbol)
        AND (e_info.source = k_info.source) THEN RETURN e END;
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0 END;
      IF (x = x0) THEN EXIT; END;
    END;
    RETURN MxVS.NoVS;
  END Get1;

PROCEDURE Insert (t: T;  key: Elt) =
  VAR e: Elt;  k_info, e_info: MxVS.Info;  x, x0: INTEGER;
  BEGIN
    MxVS.Get (key, k_info);
    x0 := Word.Xor (17 * k_info.symbol, k_info.source) MOD NUMBER (t.data^);
    x := x0;
    LOOP
      e := t.data[x];
      IF (e = MxVS.NoVS) THEN
        (* an empty hole => insert it here *)
        t.data[x] := key;
        INC (t.n_used);
        IF (2 * t.n_used > NUMBER (t.data^)) THEN Expand (t) END;
        RETURN;
      END;
      MxVS.Get (e, e_info);
      IF (e_info.symbol = k_info.symbol)
        AND (e_info.source = k_info.source) THEN
        (* a new value for an existing key *)
        t.data[x] := key;
        RETURN;
      END;
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0 END;
      IF (x = x0) THEN (* no free slots *) Expand (t) END;
    END;
  END Insert;

PROCEDURE Expand (t: T; size: INTEGER := 0) =
  VAR old := t.data;
  VAR n   := NUMBER (old^);
  VAR e : Elt;
  BEGIN
    t.n_used := 0;
    IF size = 0 THEN size := 2 * n; END;
    IF size <= 0 THEN size := 1; END;
    t.data   := NEW (Contents, size);
    FOR i := 0 TO n-1 DO
      e := old[i];
      IF (e # MxVS.NoVS) THEN Insert (t, e) END;
    END;
  END Expand;

PROCEDURE FixSize (t: T) =
  BEGIN
    IF t.n_used < NUMBER(t.data^) DIV 2 THEN
      Expand(t, t.n_used);
    END;
  END FixSize;

PROCEDURE GetData (t: T): Contents =
  BEGIN
    IF (t = NIL)
      THEN RETURN NIL
      ELSE RETURN t.data;
    END;
  END GetData;

BEGIN
  <*ASSERT MxVS.NoVS = 0 *> (* => new hash tables are initialized *)
END MxVSSet.

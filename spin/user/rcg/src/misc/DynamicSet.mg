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

GENERIC MODULE DynamicSet (Elem);

IMPORT Word, Fmt, Stdio, Wr;
IMPORT Thread;

<* FATAL Thread.Alerted, Wr.Failure *>

(* Sets are padded to the nearest quadword boundary. The use of
   InitT should force them to be initialized to the empty set. *)

PROCEDURE New (n: CARDINAL): T =
  VAR
    size := n DIV BITSIZE (InitT);
  BEGIN
    IF n MOD BITSIZE (InitT) # 0 THEN
      INC(size);
    END;
    RETURN NEW (T, size);
  END New;

PROCEDURE NewComplete (n: CARDINAL): T =
  VAR
    size := n DIV BITSIZE(InitT);
    ret : T;
  BEGIN
    IF n MOD BITSIZE(InitT) # 0 THEN
      INC(size);
    END;

    ret := NEW(T, size);
    FOR i := 0 TO size - 1 DO
      ret[i] := InitT{field := -1};
    END;

    RETURN ret;
  END NewComplete; 

(* set operations *)

PROCEDURE Union(a, b: T): T =
  VAR
    result := NEW(T, NUMBER(a^));
  BEGIN
    <* ASSERT NUMBER(a^) = NUMBER(b^) *>
    FOR i := 0 TO LAST(a^) DO
      result[i].field := Word.Or(a[i].field, b[i].field);
    END;
    RETURN result;
  END Union;

PROCEDURE Intersection(a, b: T): T =
  VAR
    result := NEW(T, NUMBER(a^));
  BEGIN
    <* ASSERT NUMBER(a^) = NUMBER(b^) *>
    FOR i := 0 TO LAST(a^) DO
      result[i].field := Word.And(a[i].field, b[i].field);
    END;
    RETURN result;
  END Intersection;
 
PROCEDURE Diff(a, b: T): T =
  VAR
    result := NEW(T, NUMBER(a^));
  BEGIN
    <* ASSERT NUMBER(a^) = NUMBER(b^) *>
    FOR i := 0 TO LAST(a^) DO
      result[i].field := Word.And(a[i].field, Word.Not(b[i].field));
    END;
    RETURN result;
  END Diff;
 
PROCEDURE SymDiff(a, b: T): T =
  VAR
    result := NEW(T, NUMBER(a^));
  BEGIN
    <* ASSERT NUMBER(a^) = NUMBER(b^) *>
    FOR i := 0 TO LAST(a^) DO
      result[i].field := Word.Xor(a[i].field, b[i].field);
    END;
    RETURN result;
  END SymDiff; 

(* tests *)

PROCEDURE IsEqual(a, b: T): BOOLEAN =
  BEGIN
    <* ASSERT NUMBER(a^) = NUMBER(b^) *>
    FOR i := 0 TO LAST(a^) DO
      IF a[i].field # b[i].field THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END IsEqual;
  
PROCEDURE IsSubset(a, b: T): BOOLEAN =
  BEGIN
    <* ASSERT NUMBER(a^) = NUMBER(b^) *>
    FOR i := 0 TO LAST(a^) DO
      IF Word.And(a[i].field, Word.Not(b[i].field)) # 0 THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END IsSubset;

PROCEDURE IsProperSubset(a, b: T): BOOLEAN =
  VAR
    proper := FALSE;
  BEGIN
    <* ASSERT NUMBER(a^) = NUMBER(b^) *>
    FOR i := 0 TO LAST(a^) DO
      IF Word.And(a[i].field, Word.Not(b[i].field)) # 0 THEN
        RETURN FALSE;
      ELSIF Word.And(Word.Not(a[i].field), b[i].field) # 0 THEN
        proper := TRUE;
      END;
    END;
    RETURN proper;
  END IsProperSubset;

PROCEDURE IsMember (e: Elem.T; s: T): BOOLEAN =
  BEGIN
    <* ASSERT ORD (e) < NUMBER (s^) * BITSIZE (InitT) *>
    WITH quot = ORD (e) DIV BITSIZE (InitT),
         rem = ORD (e) MOD BITSIZE (InitT) DO
      RETURN Word.And(s[quot].field, Word.Shift(1, rem)) # 0;
    END;
  END IsMember;

PROCEDURE IsEmpty (a: T) : BOOLEAN =
  BEGIN
    FOR i := FIRST (a^) TO LAST (a^) DO
      IF a[i].field # 0 THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END IsEmpty;

(* mutators *)

PROCEDURE Add(e: Elem.T; s: T) =
  BEGIN
    <* ASSERT ORD(e) < NUMBER(s^) * BITSIZE(InitT) *>
    WITH quot = ORD(e) DIV BITSIZE(InitT),
         rem = ORD(e) MOD BITSIZE(InitT) DO
      s[quot].field := Word.Or(s[quot].field, Word.Shift(1, rem));
    END;
  END Add;

PROCEDURE Remove(e: Elem.T; s: T) =
  BEGIN
    <* ASSERT ORD(e) < NUMBER(s^) * BITSIZE(InitT) *>
    WITH quot = ORD(e) DIV BITSIZE(InitT),
         rem = ORD(e) MOD BITSIZE(InitT) DO
      s[quot].field := Word.And(s[quot].field, Word.Not(Word.Shift(1, rem)));
    END;
  END Remove; 

PROCEDURE Clear(s: T) =
  BEGIN
    FOR i := 0 TO LAST (s^) DO
      s[i] := InitT{field := 0};
    END;
  END Clear;

PROCEDURE UnionIn (a, b: T) =
  BEGIN
    <* ASSERT NUMBER (a^) = NUMBER (b^) *>
    FOR i := FIRST (a^) TO LAST (a^) DO
      a[i].field := Word.Or (a[i].field, b[i].field);
    END;
  END UnionIn;

PROCEDURE Copy (s: T) : T =
  VAR
    val : T;
  BEGIN
    val := NEW (T, NUMBER (s^));
    val^ := s^;
    RETURN val;
  END Copy;

PROCEDURE Print(s: T; ss: Wr.T := NIL) =
  VAR
    index : CARDINAL;
  BEGIN
    IF s # NIL THEN
      IF ss = NIL THEN ss := Stdio.stderr; END;
      FOR i := FIRST(s^) TO LAST(s^) DO
        FOR j := 0 TO BITSIZE(InitT) - 1 DO
          IF Word.And(s[i].field, Word.Shift(1, j)) # 0 THEN
            Wr.PutText(ss, Fmt.Int(index + j) & " ");
          END;
        END;
        INC(index, BITSIZE(InitT));
      END;
    END;
  END Print;

BEGIN
END DynamicSet.

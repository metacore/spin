(* Copyright (C) 1989, 1994, Digital Equipment Corporation     *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last Modified On Fri Nov 18 11:18:05 PST 1994 By detlefs    *)
(*      Modified On Fri Mar 25 12:03:21 PST 1994 By kalsow     *)
(*      Modified On Tue Feb 15 10:19:04 PST 1994 By perl       *)
(*      Modified On Tue Feb 16 10:20:34 PST 1993 By mjordan    *)
(*      Modified On Tue Nov  3 16:19:49 PST 1992 By meehan     *)
(*      Modified On Thu Oct  8 08:53:53 PDT 1992 By mcjones    *)
(*      Modified On Wed Oct  7 11:51:?? PDT 1991 By muller     *)

(*
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make Text.Equal FUNCTIONAL
 *
 *)

MODULE Text EXPORTS Text, TextF;

<*PRAGMA SPEC, LOOPINV*>

TYPE CharMap = ARRAY CHAR OF T;
VAR fromCharCache := CharMap {NIL, ..}; (* 1-char texts *)

PROCEDURE New (n: CARDINAL): T RAISES {} =
  BEGIN
    WITH t = NEW (T, n + 1) DO
      t[n] := '\000';
      RETURN t;
    END;
  END New;

PROCEDURE Cat (t, u: T): T =
  BEGIN
    WITH nt = NUMBER (t^),  nu = NUMBER (u^) DO
      IF nt <= 1 THEN RETURN u END;
      IF nu <= 1 THEN RETURN t END;
      WITH res = NEW (T, nt + nu - 1) DO
        SUBARRAY(res^, 0, nt) := t^;
        SUBARRAY(res^, nt-1, nu) := u^;
        RETURN res;
      END;
    END;
  END Cat;

FUNCTIONAL PROCEDURE Equal (t, u: T): BOOLEAN =
  BEGIN
    IF NUMBER (t^) <= 1 THEN
      RETURN NUMBER (u^) <= 1;
    ELSIF NUMBER (u^) <= 1 THEN
      RETURN (FALSE);
    ELSE
      RETURN (t^ = u^);
    END;
  END Equal;

PROCEDURE GetChar (t: T; i: CARDINAL): CHAR =
  BEGIN
    IF i = LAST (t^) THEN (* force a subscript fault *) INC (i) END;
    RETURN t[i];
  END GetChar;

PROCEDURE Length (t: T): CARDINAL =
  BEGIN
    RETURN MAX (0, NUMBER (t^) - 1);
  END Length;

PROCEDURE Empty (t: T): BOOLEAN =
  BEGIN
    RETURN (NUMBER (t^) <= 1);
  END Empty;

PROCEDURE Sub (t: T; start, length: CARDINAL): T =
  BEGIN
    WITH n   = NUMBER (t^) - 1,
         len = MIN (n - start, length) DO
      IF (len <= 0) THEN RETURN "" END;
      IF (len = n) THEN RETURN t END;
      IF len = 1 THEN RETURN FromChar (t [start]) END;
      WITH res = NEW (T, len + 1) DO
        SUBARRAY(res^, 0, len) := SUBARRAY(t^, start, len);
        res [len] := '\000';
        RETURN res;
      END;
    END;
  END Sub;

PROCEDURE SetChars (VAR a: ARRAY OF CHAR; t: T) =
  BEGIN
    WITH n = MIN (NUMBER (a), NUMBER (t^)-1) DO
      SUBARRAY(a, 0, n) := SUBARRAY(t^, 0, n)
    END;
  END SetChars;

PROCEDURE FromChar (c: CHAR): T =
  BEGIN
    IF fromCharCache [c] = NIL THEN
      WITH new = NEW (T, 2) DO
        new [0] := c;
        new [1] := '\000';
        fromCharCache [c] := new;
        RETURN new
      END
    END;
    RETURN fromCharCache [c]
  END FromChar;

PROCEDURE FromChars (READONLY a: ARRAY OF CHAR): T =
  BEGIN
    WITH n = NUMBER (a) DO
      IF (n = 0) THEN RETURN "" END;
      IF n = 1 THEN RETURN FromChar (a [0]) END;
      WITH res = NEW (T, n + 1) DO
        SUBARRAY(res^, 0, n) := SUBARRAY(a, 0, n);
        res [n] := '\000';
        RETURN res;
      END;
    END;
  END FromChars;

PROCEDURE FindChar (t: T;  c: CHAR;  start := 0): INTEGER =
  <*SPEC LET startPre := start *>
  BEGIN
    IF (start < 0) THEN RETURN -1 END;
    WITH len = NUMBER (t^) - 1 DO
      LOOP
        <*LOOPINV start >= startPre *>
        IF (start >= len) THEN RETURN -1 END;
        IF (t[start] = c) THEN RETURN start END;
        INC (start);
      END;
    END;
  END FindChar;

PROCEDURE FindCharR (t: T;  c: CHAR;  start := LAST (INTEGER)): INTEGER =
  VAR
    n := NUMBER (t^) - 2;
    i := MIN (n, start);
    <*SPEC LET iPre := i *>
  BEGIN
    LOOP
      <*LOOPINV i <= iPre *>
      IF (i < 0) THEN RETURN -1 END;
      IF (t[i] = c) THEN RETURN i END;
      DEC (i);
    END;
  END FindCharR;

PROCEDURE Compare (t, u: T): [-1..1] =
  BEGIN
    WITH tEmpty = NUMBER (t^) <= 1,
         uEmpty = NUMBER (u^) <= 1 DO
      IF (tEmpty) THEN
        IF (uEmpty) THEN RETURN 0 ELSE RETURN -1 END;
      ELSIF (uEmpty) THEN
        RETURN 1;
      ELSE
        WITH tn = NUMBER (t^) - 1, tu = NUMBER (u^) - 1 DO
          FOR i := 0 TO MIN (tn, tu) DO
            IF    ORD (t[i]) < ORD (u[i]) THEN RETURN -1;
            ELSIF ORD (t[i]) > ORD (u[i]) THEN RETURN +1;
            END;
          END;
          IF    (tn = tu) THEN RETURN 0;
          ELSIF (tn < tu) THEN RETURN -1;
          ELSE                 RETURN +1;
          END;
        END;
      END;
    END;
  END Compare;

PROCEDURE CatMultiple(READONLY t: ARRAY OF T): T =
  VAR
    size: INTEGER := 0;
    idx : INTEGER := 0;
  BEGIN
    FOR i := FIRST(t) TO LAST(t) DO
      IF t[i] # NIL THEN
        WITH nt = NUMBER(t[i]^) DO
          IF nt > 1 THEN INC(size, nt-1); END;
        END;
      END;
    END;
    WITH res = NEW (T, size + 1) DO
      FOR i := FIRST(t) TO LAST(t) DO
        IF t[i] # NIL THEN
          WITH nt = NUMBER(t[i]^) DO
            IF nt > 1 THEN
              SUBARRAY(res^, idx, nt) := t[i]^;
              INC(idx, nt-1);
            END;
          END;
        END;
      END;
      RETURN res;
    END;
  END CatMultiple;

(* ESC specs *)
<*SPEC INVARIANT
         (ALL [c: CHAR]
           fromCharCache[c] = NIL OR
           (NUMBER(TEXT[fromCharCache[c]]) = 2 AND
            TEXT[fromCharCache[c]][0] = c AND
             TEXT[fromCharCache[c]][1] = VAL(0, CHAR)))
*>
<*SPEC DEPEND Value: fromCharCache *>

<*SPEC FromChar(c)
       MODIFIES Value[RES], TEXT[RES]
       ENSURES RES # NIL
           AND NUMBER(Value'[RES]) = 1
           AND Value'[RES][0] = c
           AND (TEXT = TEXT' OR FRESH(RES))
*>

<*SPEC FromChars(a)
       MODIFIES Value[RES], TEXT[RES], fromCharCache
       ENSURES RES # NIL
           AND (TEXT' = TEXT OR FRESH(RES))
           AND NUMBER(Value'[RES]) = NUMBER(a)
*>

BEGIN
END Text.

(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Fri Nov 18 11:18:32 PST 1994 by detlefs   *)
(*      modified on Fri Sep 23 09:31:45 PDT 1994 by heydon    *)
(*      modified on Fri Mar 25 12:03:15 PST 1994 by kalsow    *)
(*      modified on Wed Nov  3 14:09:28 PST 1993 by mcjones   *)
(*      modified on Wed Oct  7 11:49:?? PST 1992 by muller    *)

(*
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make Text.Equal FUNCTIONAL
 *
 *)

(* A non-nil "TEXT" represents an immutable, zero-based sequence of
   characters.  "NIL" does not represent any sequence of characters,
   it will not be returned from any procedure in this interface, and
   it is a checked runtime error to pass "NIL" to any procedure in
   this interface. *)

INTERFACE Text;

<*PRAGMA SPEC*>

IMPORT Word;

TYPE T = TEXT;

CONST Brand = "Text-1.0";

PROCEDURE Init(); (* SPIN - main body of UnsafeHash moved here and is
			run directly by RTLinker *)

PROCEDURE Cat(t, u: T): T;
(* Return the concatenation of "t" and "u". *)

FUNCTIONAL PROCEDURE Equal(t, u: T): BOOLEAN;
(* Return "TRUE" if "t" and "u" have the same length and
   (case-sensitive) contents. *)

PROCEDURE GetChar(t: T; i: CARDINAL): CHAR;
(* Return character "i" of "t".  It is a checked runtime error if "i
   >= Length(t)". *)

PROCEDURE Length(t: T): CARDINAL;
(* Return the number of characters in "t". *)

PROCEDURE Empty(t: T): BOOLEAN;
(* Equivalent to "Length(t) = 0". *)

PROCEDURE Sub(t: T; start: CARDINAL;
  length: CARDINAL := LAST(CARDINAL)): T;
(* Return a sub-sequence of "t": empty if "start >= Length(t)" or
   "length = 0"; otherwise the subsequence ranging from "start" to the
   minimum of "start+length-1" and "Length(t)-1". *)

PROCEDURE SetChars(VAR a: ARRAY OF CHAR; t: T);
(* For each "i" from 0 to "MIN(LAST(a), Length(t)-1)", set "a[i]" to
   "GetChar(t, i)".  *)

PROCEDURE FromChar(ch: CHAR): T;
(* Return a text containing the single character "ch". *)

PROCEDURE FromChars(READONLY a: ARRAY OF CHAR): T;
(* Return a text containing the characters of "a". *)

PROCEDURE Hash(t: T): Word.T;
(* Return a hash function of the contents of "t". *)
      
PROCEDURE Compare(t1, t2: T): [-1..1];
(* Return -1 if "t1" occurs before "t2", 0 if "Equal(t1, t2)", +1 if
   "t1" occurs after "t2" in lexicographic order. *)

PROCEDURE FindChar(t: T; c: CHAR; start := 0): INTEGER;
(* If "c = t[i]" for some "i" in "[start~..~Length(t)-1]", return the
   smallest such "i"; otherwise, return -1. *)

PROCEDURE FindCharR(t: T; c: CHAR;  start := LAST(INTEGER)): INTEGER;
(* If "c = t[i]" for some "i" in "[0~..~MIN(start, Length(t)-1)]",
   return the largest such "i"; otherwise, return -1. *)

PROCEDURE CatMultiple(READONLY t: ARRAY OF T): T;
(* Concatenate multiple texts *)

(* ESC Specs *)
<*SPEC VAR Value: MAP TEXT TO SEQ[CHAR] *>

<*SPEC Cat(t, u)
       MODIFIES Value[RES]
       REQUIRES t # NIL AND u # NIL
       ENSURES RES # NIL
           AND NUMBER(Value'[RES]) = NUMBER(Value[t]) + NUMBER(Value[u])
           AND (ALL [i: INTEGER]
                  0 <= i AND i < NUMBER(Value'[RES]) IMPLIES
                      Value'[RES][i] = CONCAT(Value[t], Value[u])[i]) *>

<*SPEC Equal(t, u)
       REQUIRES t # NIL AND u # NIL
       ENSURES RES IMPLIES
               (NUMBER(Value[t]) = NUMBER(Value[u])
                AND (ALL [i: INTEGER]
                     0 <= i AND i < NUMBER(Value[t]) IMPLIES
                        Value[t][i] = Value[u][i])) *>

<*SPEC GetChar(t, i)
       REQUIRES t # NIL AND i < NUMBER(Value[t])
       ENSURES RES = Value[t][i] *>

<*SPEC Length(t)
       REQUIRES t # NIL
       ENSURES RES = NUMBER(Value[t]) *>

<*SPEC Empty(t)
       REQUIRES t # NIL
       ENSURES RES IFF NUMBER(Value[t]) = 0 *>

<*SPEC Sub(t, start, length)
       MODIFIES Value[RES]
       REQUIRES t # NIL
       ENSURES RES # NIL 
           AND NUMBER(Value'[RES]) = MIN(length, MAX(NUMBER(Value[t])-start, 0))
*>

<*SPEC SetChars(a, t)
       MODIFIES a
       REQUIRES t # NIL
       ENSURES (ALL [i: INTEGER]
                    (0 <= i AND i < MIN(NUMBER(Value[t]), NUMBER(a))) IMPLIES
                    a'[i] = Value[t][i]) *>

<*SPEC FromChar(ch)
       MODIFIES Value[RES]
       ENSURES RES # NIL
           AND NUMBER(Value'[RES]) = 1
           AND Value'[RES][0] = ch *>

<*SPEC FromChars(a)
       MODIFIES Value[RES]
       ENSURES RES # NIL
           AND NUMBER(Value'[RES]) = NUMBER(a) *>

<*SPEC Hash(t)
       REQUIRES t # NIL
       ENSURES TRUE *>

<*SPEC Compare(t1, t2)
       REQUIRES t1 # NIL AND t2 # NIL
       ENSURES RES = 0 IMPLIES NUMBER(Value[t1]) = NUMBER(Value[t2]) *>

<*SPEC FindChar(t, c, start)
       REQUIRES t # NIL
       ENSURES RES = 0-1
            OR (    RES >= start
                AND RES < NUMBER(Value[t])
                AND Value[t][RES] = c)
*>

<*SPEC FindCharR(t, c, start)
       REQUIRES t # NIL
       ENSURES RES = 0-1
            OR (    RES <= start
                AND RES < NUMBER(Value[t])
                AND Value[t][RES] = c)
*>

END Text.

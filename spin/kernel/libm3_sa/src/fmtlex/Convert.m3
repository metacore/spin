(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri May  7 14:42:51 PDT 1993 by mjordan    *)
(*      modified on Fri Apr  9 15:25:53 PDT 1993 by muller     *)
(*      modified on Mon Dec 23 11:03:05 PST 1991 by kalsow     *)

(*
 * HISTORY
 *
 * 17-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of warnings.
 *
 *)

UNSAFE MODULE Convert;

IMPORT Word;

VAR
  Digits := ARRAY [0..15] OF CHAR {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };

PROCEDURE FromInt (VAR buf    : Buffer;
                       value  : INTEGER;
                       base   : Base := 10;
                       prefix : BOOLEAN := FALSE): INTEGER   RAISES {Failed} =
  VAR
    nDigits : INTEGER := 0;
    minus   : BOOLEAN := FALSE;
    bump    : BOOLEAN := FALSE;
    i, j    : INTEGER;
    c       : CHAR;
    result  : ARRAY [0..BITSIZE (INTEGER)] OF CHAR;

  BEGIN
    IF (value = 0) THEN
      result[0] := '0';
      nDigits := 1;
    ELSE (* handle a non-zero number *)
      (* get rid of negative numbers *)
      IF (value < 0) THEN
        IF (value = FIRST (INTEGER)) THEN
          (* 2's complement makes FIRST(INTEGER) a special case *)
          bump := TRUE;
	  INC (value);
        END;
        minus := TRUE;
        value := -value;
        <* ASSERT value > 0 *>
      END;

      (* convert the bulk of the digits *)
      WHILE (value > 0) DO
        result [nDigits] := Digits [value MOD base];
        value := value DIV base;
        INC (nDigits);
      END;

      (* fixup FIRST (INTEGER) *)
      IF (bump) THEN
        result [nDigits] := '0';
        j := 0;
        LOOP
          c := result [j];
          IF (c <= '9')
            THEN i := ORD (c) - ORD ('0');
            ELSE i := ORD (c) - ORD ('a') + 10;
          END;
          INC (i);
	  IF (i < base) THEN  result [j] := Digits [i];  EXIT END;
	  result [j] := '0';
	  INC (j);
        END;
        nDigits := MAX (nDigits, j+1);
      END;
    END;

    (* make sure we've got room for the result *)
    j := nDigits;
    IF minus THEN INC (j) END;
    IF prefix THEN IF base > 9 THEN INC (j, 3) ELSE INC (j, 2) END END;
    IF (j > NUMBER (buf)) THEN RAISE Failed END;

    (* build the result buffer *)
    j := 0;
    IF (minus)  THEN buf [0] := '-';  j := 1; END;
    IF (prefix) THEN
      IF (base > 9) THEN buf[j] := '1'; INC (j); END;

(*SPIN*)
      (* 
      The problem was that taking the MOD of a particular subrange (base) was
      causing the backend to crash. Assigning to an intermediate INTEGER avoids
      this problem. I have filed a PRMS on this so it can hopefully be
      fixed.

      Gun.
      *)
      VAR
        BackEndCrashWorkAround: INTEGER := base;
      BEGIN
        buf[j] := Digits [BackEndCrashWorkAround MOD 10];  INC (j);
      END;
      (* Orginal line
      buf[j] := Digits [base MOD 10];  INC (j);
      *)
(*END SPIN*)
      buf[j] := '_';  INC (j);
    END;
    FOR k := nDigits-1 TO 0 BY -1 DO
      buf [j] := result [k];  INC (j);
    END;

    RETURN j;
  END FromInt;


PROCEDURE FromUnsigned (VAR buf    : Buffer;
                            value  : INTEGER;
                            base   : Base := 10;
                            prefix : BOOLEAN := FALSE): INTEGER RAISES{Failed}=
  VAR
    nDigits : INTEGER := 0;
    j       : INTEGER;
    result  : ARRAY [0..BITSIZE (INTEGER)] OF CHAR;

  BEGIN
    IF (value = 0) THEN
      result[0] := '0';
      nDigits := 1;
    ELSE
      (* convert the bulk of the digits *)
      WHILE (value # 0) DO
        result [nDigits] := Digits [Word.Mod (value, base)];
        value := Word.Divide (value, base);
        INC (nDigits);
      END;
    END;

    (* make sure we've got room for the result *)
    j := nDigits;
    IF (prefix) THEN IF base > 9 THEN INC (j, 3) ELSE INC (j, 2) END END;
    IF (j > NUMBER (buf)) THEN RAISE Failed END;

    (* build the result buffer *)
    j := 0;
    IF (prefix) THEN
      IF (base > 9) THEN buf[j] := '1'; INC (j); END;
      buf[j] := Digits [base MOD 10];  INC (j);
      buf[j] := '_';  INC (j);
    END;
    FOR k := nDigits-1 TO 0 BY -1 DO
      buf [j] := result [k];  INC (j);
    END;

    RETURN j;
  END FromUnsigned;



                  

PROCEDURE ToInt (READONLY buf  : Buffer;
                      VAR used : INTEGER;
                          default_base : Base := 10): INTEGER   RAISES {} =
  VAR
    value: Word.T;
    skipped := 0;
  BEGIN
    IF NUMBER (buf) = 0 THEN
      used := 0;
      RETURN 0; END;

    IF buf [0] = '-' THEN
      skipped := 1;
      value := InternalToInt (SUBARRAY (buf, 1, NUMBER (buf) - 1),
                              used, default_base, 
                              Word.LeftShift (1, BITSIZE (INTEGER) - 1));
      value := Word.Plus (1, Word.Not (value));
    ELSE
      IF buf [0] = '+' THEN
        skipped := 1; END;
      value := InternalToInt (SUBARRAY (buf, skipped, NUMBER (buf) - skipped),
                              used, default_base, 
                              Word.RightShift (Word.LeftShift (Word.Not (0),
                                                               1), 1)); END;
    IF used # 0 THEN
      INC (used, skipped); END;
    RETURN value;
  END ToInt;

PROCEDURE ToUnsigned (READONLY buf  : Buffer;
                           VAR used : INTEGER;
                               default_base : Base := 10): INTEGER  RAISES {} =
  VAR
    value: Word.T;
    skipped := 0;
  BEGIN
    IF NUMBER (buf) = 0 THEN
      used := 0;
      RETURN 0; END;

    IF buf [0] = '+' THEN
      skipped := 1; END;
    value := InternalToInt (SUBARRAY (buf, skipped, NUMBER (buf) - skipped),
                            used, default_base,
                            Word.Not (0));
    IF used # 0 THEN
      INC (used, skipped); END;
    RETURN value;
  END ToUnsigned;

PROCEDURE InternalToInt (READONLY buf  : Buffer;
                         VAR used : INTEGER;
                         default_base : Base := 10;
                         limit: Word.T): Word.T  RAISES {} =

  VAR
    value : Word.T := 0;  (* accumulated value *)
    val2  : Word.T;
    n     : INTEGER  := 0;  (* index of current digit *)
    z     : INTEGER  := NUMBER (buf);
    ibase : INTEGER;
    based : BOOLEAN;
    i, j  : INTEGER;
    c     : CHAR;
  BEGIN
    IF z = 0 THEN
      used := 0;
      RETURN 0; END;
    c := buf [0];

    (* peel off any leading zeros *)
    WHILE (c = '0') DO
      INC (n);
      IF (n >= z) THEN used := n;  RETURN 0 END;
      c := buf[n];
    END;

    (* check for an explicit base *)
    IF (c = '1') AND (n+3 < z) AND (buf[n+2] = '_')
      AND ('0' <= buf[n+1]) AND (buf[n+1] <= '6') THEN
      (* an explicit base between 10 and 16 *)
      based := TRUE;
      ibase := 10 + ORD (buf[n+1]) - ORD ('0');
      INC (n, 3);
      c := buf[n];
    ELSIF ('2' <= c) AND (c <= '9') AND (n+2 < z) AND (buf[n+1] = '_') THEN
      (* an explicit base between 2 and 9 *)
      based := TRUE;
      ibase := ORD (c) - ORD ('0');
      INC (n, 2);
      c := buf[n];
    ELSE
      (* no explicit base *)
      based := FALSE;
      ibase := default_base; END;

    (* scan the digits *)
    j := n;  (* remember the first digit *)
    LOOP
      IF    ('0' <= c) AND (c <= '9') THEN  i := ORD (c) - ORD ('0');
      ELSIF ('A' <= c) AND (c <= 'F') THEN  i := ORD (c) - ORD ('A') + 10;
      ELSIF ('a' <= c) AND (c <= 'f') THEN  i := ORD (c) - ORD ('a') + 10;
      ELSE  EXIT;
      END;
      IF (i >= ibase) THEN EXIT END;
      IF Word.LT (Word.Divide (limit, ibase), value) THEN
        EXIT; END;
      val2 := Word.Times (value, ibase);  (* no overflow *)
      IF Word.LT (Word.Minus (limit, i), value) THEN
        EXIT; END;
      value := Word.Plus (val2, i);	 (* no overflow *)
      INC (n);
      IF (n >= z) THEN EXIT END;
      c := buf [n]; END;

    IF (j = n) AND (based) THEN  (* no digits were consumed *)
      (* back up and "rescan" the explicit base *)
      IF (ibase < 10) THEN (* single digit base was specified *)
        DEC (n); (* return the "_" *)
        IF (ibase < default_base) THEN (* digit is legal *)
          value := ibase;
        ELSE (* an illegal digit was specified *)
          DEC (n);  (* return the digit *)
          (* value remains 0 *) END;
      ELSE (* 2-digit base was specified *)
	(* first digit was '1' and is always legal *)
        DEC (n); (* return the "_" *)
        IF (ibase-10 < default_base) THEN (* the second digit was legal *)
          value := default_base + ibase - 10;
	ELSE (* the second digit was illegal *)
	  DEC (n);  (* return the second digit *)
	  value := 1; END; END; END;

    used := n;
    RETURN value;
  END InternalToInt;

BEGIN
END Convert.

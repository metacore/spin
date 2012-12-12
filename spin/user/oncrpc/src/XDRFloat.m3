(*
   IEEE.m3
   Convert between REAL, LONGREAL and IEEE (XDR) representation.
   David Goldberg, Xerox PARC
   March, 1992

   $Id: XDRFloat.m3,v 1.1 1996/02/09 18:08:19 mef Exp $
*)

(* Copyright (c) 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

MODULE XDRFloat;

IMPORT Word, RealFloat, LongFloat, FloatMode;
FROM RealFloat IMPORT IEEEClass;

CONST
  Bias       = 127;
  LongBias   = 1023;
  twoMinus21 = 4.7683715820312500D-7; (* 2^-21 *)

PROCEDURE WordToReal (word: Word.T): REAL RAISES {FloatMode.Trap} =
  VAR
    rawExp     : CARDINAL;      (* value of exponent field *)
    exp        : INTEGER;       (* unbiased (actual) exponent *)
    frac       : INTEGER;       (* value of fraction field *)
    significand: REAL;          (* real number is significand * 2^exp *)
    sign       : REAL;          (* 1.0 or -1.0 *)
  BEGIN
    rawExp := Word.Extract(word, i := 23, n := 8);
    exp := rawExp - Bias;
    frac := Word.Extract(word, i := 0, n := 23);
    IF Word.Extract(word, i := 31, n := 1) # 0 THEN
      sign := -1.0;
    ELSE
      sign := 1.0;
    END;

    (* Check for special values *)
    IF rawExp = 0 THEN
      IF frac = 0 THEN
        RETURN (sign * 0.0);
      ELSE
        (* denormal *)
        significand := RealFloat.Scalb(FLOAT(frac), -23);
        RETURN (sign * RealFloat.Scalb(significand, exp + 1));
      END;
    ELSIF rawExp = 255 THEN
      IF frac = 0 THEN
        RETURN (sign / 0.0);    (* infinity *)
      ELSE
        RETURN ((sign * 0.0) / 0.0); (* NaN *)
      END;
    ELSE
      significand := 1.0 + RealFloat.Scalb(FLOAT(frac), -23);
      RETURN (sign * RealFloat.Scalb(significand, exp));
    END;
  END WordToReal;

PROCEDURE RealToWord (x: REAL): Word.T =
  VAR
    exp        : INTEGER;       (* unbiased (actual) exponent *)
    rawExp     : CARDINAL;      (* value of exponent field *)
    frac       : INTEGER;       (* value of fraction field *)
    significand: REAL;          (* significand scaled to be an integer *)
    sign       : CARDINAL;      (* 1 for minus, 0 for plus *)
    ans        : Word.T;
  BEGIN
    sign := RealFloat.Sign(x);
    CASE RealFloat.Class(x) OF
    | IEEEClass.SignalingNaN, IEEEClass.QuietNaN =>
        rawExp := 255;
        frac := 1;              (* any nonzero value will do *)
    | IEEEClass.Infinity => rawExp := 255; frac := 0;
    | IEEEClass.Zero => rawExp := 0; frac := 0;
    | IEEEClass.Normal =>
        x := ABS(x);
        exp := RealFloat.ILogb(x);
        rawExp := exp + Bias;
        significand := RealFloat.Scalb(x, -exp + 23);
        (* frac has extra bit, but this is ignored on Word.Insert *)
        frac := TRUNC(significand);
    | IEEEClass.Denormal =>
        x := ABS(x);
        rawExp := 0;
        significand := RealFloat.Scalb(x, Bias + 23 - 1);
        frac := TRUNC(significand);
    END;
    ans := Word.Insert(ans, rawExp, 23, 8);
    ans := Word.Insert(ans, sign, 31, 1);
    ans := Word.Insert(ans, frac, 0, 23);
    RETURN (ans);
  END RealToWord;

PROCEDURE WordToLongReal (wordHigh, wordLow: Word.T): LONGREAL
  RAISES {FloatMode.Trap} =
  VAR
    rawExp: CARDINAL;           (* value of exponent field *)
    exp   : INTEGER;            (* unbiased (actual) exponent *)
    signif: LONGREAL;           (* real number is signif * 2^exp *)
    tmp   : Word.T;
    sign  : LONGREAL;           (* 1.0 or -1.0 *)
  BEGIN
    rawExp := Word.Extract(wordHigh, i := 20, n := 11);
    exp := rawExp - LongBias;
    signif :=
      LongFloat.Scalb(
        FLOAT(Word.Extract(wordHigh, i := 0, n := 20), LONGREAL), -20);

    (* need to treat low word as an unsigned integer *)
    IF Word.Extract(wordLow, i := 31, n := 1) = 1 THEN
      tmp := Word.Insert(wordLow, i := 31, n := 1, y := 0);
      signif := signif + LongFloat.Scalb(FLOAT(tmp, LONGREAL), -52);
      signif := signif + twoMinus21;
    ELSE
      signif := signif + LongFloat.Scalb(FLOAT(wordLow, LONGREAL), -52);
    END;
    IF Word.Extract(wordHigh, i := 31, n := 1) # 0 THEN
      sign := -1.0d0;
    ELSE
      sign := 1.0d0;
    END;

    (* Check for special values *)
    IF rawExp = 0 THEN
      IF signif = 0.0d0 THEN
        RETURN (sign * 0.0D0);
      ELSE
        (* denormal *)
        RETURN (sign * LongFloat.Scalb(signif, exp + 1));
      END;
    ELSIF rawExp = 2047 THEN
      IF signif = 0.0D0 THEN
        RETURN (sign / 0.0D0);  (* infinity *)
      ELSE
        RETURN ((sign * 0.0D0) / 0.0D0); (* NaN *)
      END;
    ELSE
      signif := 1.0D0 + signif;
      RETURN (sign * LongFloat.Scalb(signif, exp));
    END;
  END WordToLongReal;

PROCEDURE LongRealToWord (            x                : LONGREAL;
                          VAR (*out*) wordHigh, wordLow: Word.T    ) =
  VAR
    exp        : INTEGER;       (* unbiased (actual) exponent *)
    rawExp     : CARDINAL;      (* value of exponent field *)
    significand: LONGREAL;
    sign       : CARDINAL;      (* 1 for minus, 0 for plus *)
    tmp        : LONGREAL;
    top        : Word.T;        (* top 20 bits of fraction *)
    bottom     : Word.T;        (* bottom 32 bits of fraction *)
  BEGIN
    sign := LongFloat.Sign(x);
    CASE LongFloat.Class(x) OF
    | IEEEClass.SignalingNaN, IEEEClass.QuietNaN =>
        rawExp := 2047;
        (* any values with (top OR bottom) # 0 will do *)
        top := 1;
        bottom := 0;
    | IEEEClass.Infinity => rawExp := 2047; top := 0; bottom := 0;
    | IEEEClass.Zero => rawExp := 0; top := 0; bottom := 0;
    | IEEEClass.Normal =>
        x := ABS(x);
        exp := LongFloat.ILogb(x);
        rawExp := exp + LongBias;
        significand := LongFloat.Scalb(x, -exp) - 1.0d0;
        tmp := LongFloat.Scalb(significand, 20);
        top := TRUNC(tmp);
        tmp := significand - LongFloat.Scalb(FLOAT(top, LONGREAL), -20);
        bottom := TRUNC(LongFloat.Scalb(tmp, 52));
    | IEEEClass.Denormal =>
        x := ABS(x);
        rawExp := 0;
        significand := LongFloat.Scalb(x, LongBias - 1);
        tmp := LongFloat.Scalb(significand, 20);
        top := TRUNC(tmp);
        tmp := significand - LongFloat.Scalb(FLOAT(top, LONGREAL), -20);
        bottom := TRUNC(LongFloat.Scalb(tmp, 52));
    END;
    wordHigh := Word.Insert(wordHigh, rawExp, i := 20, n := 11);
    wordHigh := Word.Insert(wordHigh, sign, i := 31, n := 1);
    wordHigh := Word.Insert(wordHigh, top, i := 0, n := 20);
    wordLow := bottom;
  END LongRealToWord;

BEGIN
  (* start code *)
END XDRFloat.

(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordShift.m3                                          *)
(* Last Modified On Mon Dec  5 15:30:43 PST 1994 By kalsow     *)
(*      Modified On Fri May 18 08:15:52 1990 By muller         *)

(*
 * HISTORY
 * 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	functional
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	make word operations non-bounded
 *
 *)

MODULE WordShift;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, SubrangeType, Formal;
IMPORT Int, IntegerExpr, Value, ProcType, CheckExpr, Target, TInt, TWord;

VAR Z, ZL, ZR: CallExpr.MethodList;
VAR formals, formalsL, formalsR: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Int.T;
  END Check;

PROCEDURE CheckL (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formalsL, ce.proc);
    ce.type := Int.T;
  END CheckL;

PROCEDURE CheckR (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formalsR, ce.proc);
    ce.type := Int.T;
  END CheckR;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.Shift ();
  END Compile;

PROCEDURE CompileL (ce: CallExpr.T) =
  VAR max: Target.Int;  b := TInt.FromInt (Target.Integer.size -1, max);
  BEGIN
    <* ASSERT b *>
    Expr.Compile (ce.args[0]);
    CheckExpr.Emit (ce.args[1], TInt.Zero, max);
    CG.Shift_left ();
  END CompileL;

PROCEDURE CompileR (ce: CallExpr.T) =
  VAR max: Target.Int;  b := TInt.FromInt (Target.Integer.size -1, max);
  BEGIN
    <* ASSERT b *>
    Expr.Compile (ce.args[0]);
    CheckExpr.Emit (ce.args[1], TInt.Zero, max);
    CG.Shift_right ();
  END CompileR;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, i1, result: Target.Int;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF (e0 # NIL) AND IntegerExpr.Split (e0, w0)
      AND (e1 # NIL) AND IntegerExpr.Split (e1, i1)
    THEN
      TWord.Shift (w0, i1, result);
      RETURN IntegerExpr.New (result);
    ELSE
      RETURN NIL;
    END;
  END Fold;

PROCEDURE FoldL (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, i1, max, result: Target.Int;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF (e0 # NIL) AND IntegerExpr.Split (e0, w0)
      AND (e1 # NIL) AND IntegerExpr.Split (e1, i1)
      AND TInt.LE (TInt.Zero, i1)
      AND TInt.FromInt (Target.Integer.size, max)
      AND TInt.LT (i1, max)
    THEN
      TWord.Shift (w0, i1, result);
      RETURN IntegerExpr.New (result);
    ELSE
      RETURN NIL;
    END;
  END FoldL;

PROCEDURE FoldR (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, i1, max, neg_i1, result: Target.Int;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF (e0 # NIL) AND IntegerExpr.Split (e0, w0)
      AND (e1 # NIL) AND IntegerExpr.Split (e1, i1)
      AND TInt.LE (TInt.Zero, i1)
      AND TInt.FromInt (Target.Integer.size, max)
      AND TInt.LT (i1, max)
      AND TInt.Subtract (TInt.Zero, i1, neg_i1)
    THEN
      TWord.Shift (w0, neg_i1, result);
      RETURN IntegerExpr.New (result);
    ELSE
      RETURN NIL;
    END;
  END FoldR;

PROCEDURE GetBounds (ce: CallExpr.T; VAR min, max: Target.Int) =
  VAR e1: Expr.T;
      const, minE, maxE: Target.Int;
      bitsize, mask, mask_shift, check_min, check_max: Target.Int;
  BEGIN
    e1 := Expr.ConstValue (ce.args[1]);

    (* We attempt to do something smart if the shift value is a non-negative 
       constant *)
    IF e1 # NIL AND IntegerExpr.Split(e1, const) AND 
       TInt.LE(TInt.Zero, const) THEN
      Expr.GetBounds(ce.args[0], minE, maxE);
    ELSE
      (* If neither argument is a constant. *)
      ExprRep.NoBounds(ce, min, max);
      RETURN;
    END;

    (* We are interested in the case where both bounds have no bits on
       that could be shifted into the sign bit, then the min and max are a
       function of the expression's min and max. *)
    EVAL TInt.FromInt(Target.Integer.size, bitsize);
    EVAL TInt.Subtract(bitsize, const, mask_shift);
    EVAL TInt.Subtract(mask_shift, TInt.One, mask_shift);
    EVAL TInt.Shift(TInt.MOne, mask_shift, mask);
    EVAL TInt.BitwiseAnd(mask, minE, check_min);
    EVAL TInt.BitwiseAnd(mask, maxE, check_max);
    IF TInt.EQ(TInt.Zero, check_min) AND TInt.EQ(TInt.Zero, check_max) THEN
      EVAL TInt.Shift(minE, const, min);
      EVAL TInt.Shift(maxE, const, max);
    ELSE
      min := Target.Integer.min;
      max := Target.Integer.max;
    END;
  END GetBounds;

PROCEDURE GetBoundsL (ce: CallExpr.T; VAR min, max: Target.Int) =
  VAR e1: Expr.T;
      const, minE, maxE: Target.Int;
      bitsize, mask, mask_shift, check_min, check_max: Target.Int;
  BEGIN
    e1 := Expr.ConstValue (ce.args[1]);

    (* We attempt to do something smart if the shift value is a constant *)
    IF e1 # NIL AND IntegerExpr.Split(e1, const) THEN
      Expr.GetBounds(ce.args[0], minE, maxE);
    ELSE
      (* If neither argument is a constant. *)
      ExprRep.NoBounds(ce, min, max);
      RETURN;
    END;

    (* We are interested in the case where both bounds have no bits on
       that could be shifted into the sign bit, then the min and max are a
       function of the expression's min and max. *)
    EVAL TInt.FromInt(Target.Integer.size, bitsize);
    EVAL TInt.Subtract(bitsize, const, mask_shift);
    EVAL TInt.Subtract(mask_shift, TInt.One, mask_shift);
    EVAL TInt.Shift(TInt.MOne, mask_shift, mask);
    EVAL TInt.BitwiseAnd(mask, minE, check_min);
    EVAL TInt.BitwiseAnd(mask, maxE, check_max);
    IF TInt.EQ(TInt.Zero, check_min) AND TInt.EQ(TInt.Zero, check_max) THEN
      EVAL TInt.Shift(minE, const, min);
      EVAL TInt.Shift(maxE, const, max);
    ELSE
      min := Target.Integer.min;
      max := Target.Integer.max;
    END;
  END GetBoundsL;

PROCEDURE GetBoundsR (ce: CallExpr.T; VAR min, max: Target.Int) =
  VAR e1: Expr.T;
      const, minE, maxE: Target.Int;
      bitsize, mask, mask_shift, check_min, check_max: Target.Int;
  BEGIN
    e1 := Expr.ConstValue (ce.args[1]);

    (* We attempt to do something smart if the shift value is a constant *)
    IF e1 # NIL AND IntegerExpr.Split(e1, const) THEN
      Expr.GetBounds(ce.args[0], minE, maxE);
    ELSE
      (* If neither argument is a constant. *)
      ExprRep.NoBounds(ce, min, max);
      RETURN;
    END;

    (* If the shift constant is bigger than 0 and the value to be shifted
       is non-negative then use a bound of [0 .. (maxE >> const)] *)
    IF TInt.LT(TInt.Zero, const) AND TInt.LE(TInt.Zero, maxE) THEN
      min := TInt.Zero;
      EVAL TInt.Subtract(TInt.Zero, const, const);
      EVAL TInt.Shift(maxE, const, max);
    ELSE
      min := Target.Integer.min;
      max := Target.Integer.max;
    END;
  END GetBoundsR;

PROCEDURE Initialize () =
  VAR
    max : Target.Int;
    b   := TInt.FromInt (Target.Integer.size-1, max);
    sub := SubrangeType.New (TInt.Zero, max, Int.T, FALSE);

    f0  := Formal.NewBuiltin ("x", 0, Int.T);
    f1  := Formal.NewBuiltin ("n", 1, Int.T);
    t   := ProcType.New (Int.T, f0, f1,
                         isBounded := TRUE, isFunctional := TRUE);

    Lf0 := Formal.NewBuiltin ("x", 0, Int.T);
    Lf1 := Formal.NewBuiltin ("n", 1, sub);
    Lt  := ProcType.New (Int.T, Lf0, Lf1,
                         isBounded := TRUE, isFunctional := TRUE);

    Rf0 := Formal.NewBuiltin ("x", 0, Int.T);
    Rf1 := Formal.NewBuiltin ("n", 1, sub);
    Rt  := ProcType.New (Int.T, Rf0, Rf1,
                         isBounded := TRUE, isFunctional := TRUE);
  BEGIN
    <*ASSERT b*>
    Z := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, Int.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 Fold,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable, (* noteWriter *)
                                 GetBounds);
    Procedure.Define ("Shift", Z, FALSE, t);
    formals := ProcType.Formals (t);


    ZL := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, Int.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 CheckL,
                                 CallExpr.PrepArgs,
                                 CompileL,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 FoldL,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable, (* noteWriter *)
                                 GetBoundsL);
    Procedure.Define ("LeftShift", ZL, FALSE, Lt);
    formalsL := ProcType.Formals (Lt);


    ZR := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, Int.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 CheckR,
                                 CallExpr.PrepArgs,
                                 CompileR,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 FoldR,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable, (* noteWriter *)
                                 GetBoundsR);
    Procedure.Define ("RightShift", ZR, FALSE, Rt);
    formalsR := ProcType.Formals (Rt);

  END Initialize;

BEGIN
END WordShift.

(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordAnd.m3                                            *)
(* Last Modified On Mon Dec  5 15:19:35 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:06:36 1990 By muller         *)

(*
 * HISTORY
 * 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	functional
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	make word operations non-bounded
 *
 *)

MODULE WordAnd;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, ProcType;
IMPORT Int, IntegerExpr, Formal, Value, WordPlus, Target, TWord, TInt;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Int.T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.And ();
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR w0, w1, result: Target.Int;
  BEGIN
    IF WordPlus.GetArgs (ce.args, w0, w1)
      THEN TWord.And (w0, w1, result); RETURN IntegerExpr.New (result);
      ELSE RETURN NIL;
    END;
  END Fold;

PROCEDURE GetBounds (ce: CallExpr.T; VAR min, max: Target.Int) =
  VAR e0, e1: Expr.T;
      const, minE, maxE: Target.Int;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);

    (* Presumably both arguments are not constants, since we are already
       generating the operation by the time be ask for its bounds. *)
    IF e0 # NIL AND IntegerExpr.Split(e0, const) THEN
      Expr.GetBounds(ce.args[1], minE, maxE);
    ELSIF e1 # NIL AND IntegerExpr.Split(e1, const) THEN
      Expr.GetBounds(ce.args[0], minE, maxE);
    ELSE
      (* If neither argument is a constant. *)
      ExprRep.NoBounds(ce, min, max);
      RETURN;
    END;

    (* We are interested in the case where both bounds are non-negative,
       yielding a sub-range from 0 to the minimum of the upper bound or 
       the mask, or the case where the constant mask is non-negative,
       giving a sub-range bound from 0 to the mask. *)
    min := TInt.Zero;
    IF TInt.LE(TInt.Zero, minE) THEN
      IF TInt.LT(maxE, const) THEN
        max := maxE;
      ELSE
        max := const;
      END;
    ELSIF TInt.LE(TInt.Zero, const) THEN
      max := const;
    ELSE
      min := Target.Integer.min;
      max := Target.Integer.max;
    END;
  END GetBounds;

PROCEDURE Initialize () =
  VAR
    x0 := Formal.NewBuiltin ("x", 0, Int.T);
    y0 := Formal.NewBuiltin ("y", 1, Int.T);
    t0 := ProcType.New (Int.T, x0, y0,
                        isBounded := TRUE, isFunctional := TRUE);
  BEGIN
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
    Procedure.Define ("And", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END WordAnd.

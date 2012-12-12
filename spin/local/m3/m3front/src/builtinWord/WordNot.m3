(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordNot.m3                                            *)
(* Last Modified On Mon Dec  5 15:30:45 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:15:33 1990 By muller         *)

(*
 * HISTORY
 * 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	functional
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	make word operations non-bounded
 *
 *)

MODULE WordNot;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TWord;
IMPORT Int, IntegerExpr, Value, Formal, ProcType;
IMPORT TInt;

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
    CG.Not ();
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e: Expr.T;  w, result: Target.Int;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e # NIL) AND IntegerExpr.Split (e, w)
      THEN TWord.Not (w, result);  RETURN IntegerExpr.New (result);
      ELSE RETURN NIL;
    END;
  END Fold;

PROCEDURE GetBounds (ce: CallExpr.T; VAR min, max: Target.Int) =
  VAR
    minE, maxE: Target.Int;
  BEGIN
    Expr.GetBounds(ce.args[0], minE, maxE);

    (* Taking the logical negation is like subtracting the value from -1.
       In particular, the min and max bounds switch places. *)
    EVAL TInt.Subtract(TInt.MOne, minE, max);    
    EVAL TInt.Subtract(TInt.MOne, maxE, min);
  END GetBounds;

PROCEDURE Initialize () =
  VAR
    f0 := Formal.NewBuiltin ("x", 0, Int.T);
    t  := ProcType.New (Int.T, f0,
                        isBounded := TRUE, isFunctional := TRUE);
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, TRUE, TRUE, Int.T,
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
    Procedure.Define ("Not", Z, FALSE, t);
    formals := ProcType.Formals (t);
  END Initialize;

BEGIN
END WordNot.

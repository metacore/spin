(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Floatt.m3                                             *)
(* Last Modified On Tue May  3 16:30:33 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:23 1990 By muller         *)

(*
 * HISTORY
 * 05-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	errors for using floats
 *
 * 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	functional
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	make built-in operations non-bounded
 *
 *)

MODULE Floatt;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Reel, LReel, EReel, Int;
IMPORT Error, ReelExpr, TypeExpr;

IMPORT Host;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR u: Type.T;
  BEGIN
    u := Reel.T;
    IF (NUMBER (ce.args^) > 1) THEN
      EVAL TypeExpr.Split (ce.args[1], u);
    END;
    RETURN Type.Base (u);
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t, u: Type.T;  e: Expr.T;
  BEGIN
    IF Host.no_float THEN
      Error.Msg ("floating-point operation used");
    END;

    INC (cs.fp_ops);
    e := ce.args[0];

    u := Reel.T;
    IF (NUMBER (ce.args^) > 1) THEN
      IF NOT TypeExpr.Split (ce.args[1], u) THEN
        Error.Msg ("FLOAT: second argument must be a floating point type");
      END;
      u := Type.Base (u);
    END;

    t := Type.Base (Expr.TypeOf (ce.args[0]));
    IF (t # Int.T) AND (t # Reel.T) AND (t # LReel.T) AND (t # EReel.T) THEN
      Error.Msg ("FLOAT: wrong first argument type");
    END;

    IF (u # Reel.T) AND (u # LReel.T) AND (u # EReel.T) THEN
      Error.Msg ("FLOAT: wrong second argument type");
    END;

    ce.type := u;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.Prep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR
    e := ce.args[0];
    t := Expr.TypeOf (e);
    u := Reel.T;
  BEGIN
    IF (NUMBER (ce.args^) > 1) THEN
      EVAL TypeExpr.Split (ce.args[1], u);
      u := Type.Base (u);
    END;
    Expr.Compile (e);
    CG.Cvt_float (Type.CGType (t), Type.CGType (u));
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e, x: Expr.T;  t: Type.T;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e = NIL) THEN RETURN NIL END;

    t := Reel.T;
    IF (NUMBER (ce.args^) > 1) THEN
      IF NOT TypeExpr.Split (ce.args[1], t) THEN RETURN NIL END;
    END;

    IF ReelExpr.Float (e, t, x)
      THEN  RETURN x;
      ELSE  RETURN NIL;
    END;
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 2, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 Fold,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("FLOAT", Z, TRUE,
                      isBounded := TRUE, isFunctional := TRUE);
  END Initialize;

BEGIN
END Floatt.

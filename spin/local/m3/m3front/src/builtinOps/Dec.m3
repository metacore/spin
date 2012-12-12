(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Dec.m3                                                *)
(* Last Modified On Fri Jul  8 09:15:07 PDT 1994 By kalsow     *)
(*      Modified On Tue Apr  2 03:46:13 1991 By muller         *)

(*
 * HISTORY
 * 28-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	bounded and functional
 *
 *)

MODULE Dec;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Error, Int, Module;
IMPORT M3ID, Addr, Target, TInt, IntegerExpr, Host, NamedExpr;
IMPORT Fmt;

IMPORT Functional;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    DoCheck ("DEC", ce, cs);
  END Check;

PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t: Type.T; e: Expr.T;  nm: M3ID.T;
  BEGIN
    e := ce.args[0];
    t := Type.Base (Expr.TypeOf (e));
    IF NOT Type.IsOrdinal (t) THEN
      IF Type.IsSubtype (t, Addr.T) THEN
        IF Module.IsSafe () THEN Error.Txt (name, "unsafe operation") END;
      ELSE
        Error.Txt (name, "first argument must be of an ordinal type");
      END;
    ELSIF (NOT Expr.IsDesignator (e)) THEN
      Error.Txt (name, "first argument must be a variable");
    ELSIF (NOT Expr.IsWritable (e)) THEN
      Error.Txt (name, "first argument must be writable");
    ELSIF NamedExpr.SplitName (e, nm) THEN
      (* simple scalar => we don't need an explicit address
            -- demanded by Eric Veach 9/17/93 *)
    ELSE
      Expr.NeedsAddress (e);
    END;
    IF (NUMBER (ce.args^) > 1) THEN
      t := Type.Base (Expr.TypeOf (ce.args[1]));
      IF (t # Int.T) THEN
        Error.Txt (name, "second argument must be an integer");
      END;
    END;
    ce.type := NIL;
    INC (cs.int_ops);

    (* SPIN *)
    Functional.CheckExpr (e, "passed to " & name);
  END DoCheck;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.PrepLValue (ce.args[0]);
    IF (NUMBER (ce.args^) > 1) THEN Expr.Prep (ce.args[1]); END;
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR
    lhs    := ce.args[0];
    tlhs   := Expr.TypeOf (lhs);
    info   : Type.Info;
    dec    : Expr.T;
    check  : [0..3] := 0;
    lvalue : CG.Val;
    bmin, bmax, imin, imax: Target.Int;
  BEGIN
    EVAL Type.CheckInfo (tlhs, info);
    IF (NUMBER (ce.args^) > 1)
      THEN dec := ce.args[1];
      ELSE dec := IntegerExpr.New (TInt.One);  Expr.Prep (dec);
    END;
    Expr.GetBounds (lhs, bmin, bmax);
    Expr.GetBounds (dec, imin, imax);

    IF Host.doRangeChk THEN
      IF NOT TInt.EQ (bmin, Target.Integer.min)
         AND TInt.LT (TInt.Zero, imax) THEN INC (check) END;
      IF NOT TInt.EQ (bmax, Target.Integer.max)
         AND TInt.LT (imin, TInt.Zero) THEN INC (check, 2) END;
    END;

    Expr.CompileLValue (lhs);
    lvalue := CG.Pop ();
    CG.Push (lvalue);

    CG.Push (lvalue);
    CG.Load_indirect (info.cg_type, 0, info.size);
    Expr.Compile (dec);

    IF (info.cg_type = CG.Type.Addr)
      THEN CG.Index_bytes (-Target.Byte);  check := 0;
      ELSE CG.Subtract (CG.Type.Int);
    END;

    VAR
      pmin, pmax: INTEGER;
    BEGIN
      EVAL TInt.ToInt(bmin, pmin);
      EVAL TInt.ToInt(bmax, pmax);

      CASE check OF
      | 0 => (* no range checking *)
      | 1 => 
        IF Host.verbose_checks THEN
          Error.Warn (1, "Emitting DEC check low against " & 
            Fmt.Int(pmin));
        END;
        CG.Check_lo (bmin);
      | 2 => 
        IF Host.verbose_checks THEN
          Error.Warn (1, "Emitting DEC check high against " & 
            Fmt.Int(pmax));
        END;
        CG.Check_hi (bmax);
      | 3 => 
        IF Host.verbose_checks THEN
          Error.Warn (1, "Emitting DEC check range in [" & 
            Fmt.Int(pmin) & ", " & Fmt.Int(pmax));
        END;
        CG.Check_range (bmin, bmax);
      END;
    END;

    CG.Store_indirect (info.cg_type, 0, info.size);
    CG.Free (lvalue);
    Expr.NoteWrite (lhs);
  END Compile;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 2, FALSE, FALSE, TRUE, NIL,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue, 
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    (* SPIN: INC and DEC are statements, so making them FUNCTIONAL
       is a workaround
     *)
    Procedure.Define ("DEC", Z, TRUE,
                      isBounded := TRUE, isFunctional := TRUE);
  END Initialize;

BEGIN
END Dec.

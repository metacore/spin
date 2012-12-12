(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AssignStmt.m3                                         *)
(* Last modified on Wed Jun 29 17:16:58 PDT 1994 by kalsow     *)
(*      modified on Fri Dec 21 01:24:28 1990 by muller         *)

(*
 * HISTORY
 * 18-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	read and write barrier
 *
 * 30-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	code for ref counting
 *
 * 25-Jul-96  Wilson Hsieh (whsieh) at the University of Washington
 *	allow method restrictions to be turned off
 *
 * 06-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	changes to ensure that nested procedure checks happen when
 *	 assigning either a PROCEDURE or a PROCANY
 *
 * 17-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	better error messages for bad assignments
 *
 * 05-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for restricting TYPE.METHOD, which cannot be stored
 *
 * 23-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	Modified to permit a SET object to be assigned to a packed SET of the
 *	 same type.
 *
 *)

MODULE AssignStmt;

IMPORT CG, Stmt, StmtRep, Expr, Type, Error, Module, Target, TInt;
IMPORT Token, Scanner, CallStmt, Addr, CheckExpr;
IMPORT M3, M3ID, Value, NamedExpr, ArrayType;
IMPORT QualifyExpr, Variable, Procedure, OpenArrayType;
IMPORT ProcExpr, ObjectType, CallExpr, Host, Narrow;

IMPORT Functional;           (* for checking FUNCTIONAL *)
IMPORT Runtime, M3RT, ProcType;        (* for tracing *)

TYPE
  P = Stmt.T OBJECT
	lhs     : Expr.T;
	rhs     : Expr.T;
      OVERRIDES
        check       := CheckMethod;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  VAR e: Expr.T;  p: P;  s: Stmt.T;  here := Scanner.offset;
  BEGIN
    e := Expr.Parse ();
    IF (Scanner.cur.token # Token.T.tASSIGN) THEN
      IF NOT CallExpr.Is (e) THEN
	Error.Msg ("Expression is not a statement");
      END;
      s := CallStmt.New (e);
      s.origin := here;
      RETURN s;
    END;	

    p := NEW (P);
    StmtRep.Init (p);
    p.origin := here;
    Scanner.GetToken (); (* := *)
    p.lhs := e;
    p.rhs := Expr.Parse ();
    RETURN p;
  END Parse;

PROCEDURE CheckMethod (p: P;  VAR cs: Stmt.CheckState) =
  VAR tlhs: Type.T;
  BEGIN
    Expr.TypeCheck (p.lhs, cs);
    Expr.TypeCheck (p.rhs, cs);

    tlhs := Expr.TypeOf (p.lhs);
    IF  NOT Expr.IsDesignator (p.lhs) THEN
      Error.Msg ("left-hand side is not a designator");
    ELSIF NOT Expr.IsWritable (p.lhs) THEN
      Error.Msg ("left-hand side is read-only");
    END;

    (* SPIN; still have to check args to INC/DEC *)
    Functional.CheckExpr (p.lhs, "assigned");

    Check (tlhs, p.rhs, cs);
  END CheckMethod;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR
    lhs_info: Type.Info;
    global: M3.Global;
  BEGIN
    EVAL Type.CheckInfo (Expr.TypeOf (p.lhs), lhs_info);
    global := Expr.IsGlobal (p.lhs);
    Expr.PrepLValue (p.lhs);
    Expr.Prep (p.rhs);

    Expr.CompileLValue (p.lhs);

    Emit (Expr.TypeOf (p.lhs), p.rhs, global);
    Expr.NoteWrite (p.lhs);
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END GetOutcome;


(*--------------------------------------------------------- type checking ---*)

PROCEDURE Check (tlhs: Type.T;  rhs: Expr.T;  VAR cs: Stmt.CheckState) =
  VAR
    t := Type.Base (tlhs); (* strip renaming and packing *)
    trhs := Expr.TypeOf (rhs);
    trhs_base := Type.Base (trhs);
    lhs_info, t_info, rhs_info: Type.Info;
    c: Type.Class;
  BEGIN
    tlhs := Type.CheckInfo (tlhs, lhs_info);
    t := Type.CheckInfo (t, t_info);
    trhs_base := Type.CheckInfo (trhs_base, rhs_info);
    c := t_info.class;
    Expr.TypeCheck (rhs, cs);

    IF tlhs = NIL OR trhs = NIL THEN
      Error.Msg ("rhs is not assignable to lhs");

    ELSIF NOT Type.IsAssignable (tlhs, trhs) THEN
      Error.Msg (Type.ToText(trhs) & " rhs is not assignable to " & Type.ToText(tlhs) & " lhs");

    ELSIF (Type.IsOrdinal (t)) THEN
      CheckOrdinal (tlhs, rhs);

    (* must check if assignment involves PROCEDUREs first
     * because PROCEDURE is a subtype of ADDRESS in SPIN
     *)
    ELSIF (c = Type.Class.Procedure)
       OR (rhs_info.class = Type.Class.Procedure) THEN
      CheckProcedure (rhs);

    ELSIF (c = Type.Class.Ref) OR (c = Type.Class.Object)
       OR (c = Type.Class.Opaque) THEN
      CheckReference (tlhs, trhs, lhs_info);

    ELSE
      (* ok *)

    END;
  END Check;

PROCEDURE CheckOrdinal (tlhs: Type.T;  rhs: Expr.T) =
  VAR lmin, lmax, rmin, rmax: Target.Int;  constant: Expr.T;
  BEGIN
    (* ok, but must generate a check *)
    constant := Expr.ConstValue (rhs);
    IF (constant # NIL) THEN rhs := constant END;
    Expr.GetBounds (rhs, rmin, rmax);
    EVAL Type.GetBounds (tlhs, lmin, lmax);
    IF TInt.LE (lmin, lmax) AND TInt.LE (rmin, rmax)
      AND (TInt.LT (lmax, rmin) OR TInt.LT (rmax, lmin)) THEN
      (* non-overlappling, non-empty ranges *)
      Error.Warn (2, "value not assignable (range fault)");
    END;
  END CheckOrdinal;

PROCEDURE CheckReference (tlhs, trhs: Type.T;  READONLY lhs_info: Type.Info) =
  BEGIN
    IF Type.IsSubtype (trhs, tlhs) THEN
      (* ok *)
    ELSIF NOT Type.IsSubtype (tlhs, trhs) THEN
      Error.Msg (Type.ToText(trhs) & " rhs is not assignable to " & Type.ToText(tlhs) & " lhs");
    ELSIF Type.IsEqual (trhs, Addr.T, NIL) THEN 
      (* that is legal only in UNSAFE modules *)
      IF Module.IsSafe() THEN Error.Msg ("unsafe implicit NARROW") END;
    ELSIF ObjectType.Is (trhs) THEN
      (* ok *)
    ELSIF lhs_info.isTraced THEN
      (* ok *)
    ELSE
      Error.Msg (Type.ToText(trhs) & " rhs is not assignable to " & Type.ToText(tlhs) & " lhs");
    END;
  END CheckReference;

PROCEDURE CheckProcedure (rhs: Expr.T) =
  VAR 
    tmp: Type.T;  (* ignored *)
  BEGIN
    IF NOT Host.method_ok AND QualifyExpr.IsExplicitMethod (rhs, tmp) THEN
      Error.Msg ("Explicitly named method cannot be stored");
    END;

    IF NeedsClosureCheck (rhs, TRUE) THEN
      (* may generate more detailed message *)
    END;
  END CheckProcedure;

PROCEDURE NeedsClosureCheck (proc: Expr.T;  errors: BOOLEAN): BOOLEAN =
  (* returns TRUE if proc could be a nested procedure *)
  VAR name: M3ID.T;  obj: Value.T;  class: Value.Class;  nested: BOOLEAN;
  BEGIN
    IF NOT (NamedExpr.Split (proc, name, obj)
            OR QualifyExpr.Split (proc, obj)
	    OR ProcExpr.Split (proc, obj)) THEN
      (* non-constant, non-variable => OK *)
      RETURN FALSE;
    END;
    obj := Value.Base (obj);
    class := Value.ClassOf (obj);
    IF (class = Value.Class.Procedure) THEN
      nested := Procedure.IsNested (obj);
      IF (nested) AND (errors) THEN
        Error.ID (Value.CName (obj), "cannot assign nested procedures");
      END;
      RETURN FALSE;
    ELSIF (class = Value.Class.Var) AND Variable.HasClosure (obj) THEN
      RETURN TRUE;
    ELSE (* non-formal, non-const => no check *)
      RETURN FALSE;
    END;
  END NeedsClosureCheck;

(*------------------------------------------------------- code generation ---*)

(* NOTES:
   gets called to init record/obj/array fields, variables
   set return value, exception value
   generate object/record initializer
 *)
PROCEDURE Emit (tlhs: Type.T;  rhs: Expr.T;
                isGlobal : M3.Global := M3.Global.No;
                compiled := FALSE) =
  (* on entry the lhs is compiled and the rhs is prepped.
     IF compiled is TRUE, then the rhs has been compiled already
       only used in an array constructor
   *)
  VAR
    t := Type.Base (tlhs); (* strip renaming and packing *)
    lhs_info, t_info: Type.Info;
  BEGIN
    t := Type.CheckInfo (t, t_info);
    tlhs := Type.CheckInfo (tlhs, lhs_info);

    IF compiled THEN
      <* ASSERT t_info.class = Type.Class.OpenArray *>
    END;
    
    CASE t_info.class OF
    | Type.Class.Integer, Type.Class.Subrange, Type.Class.Enum =>
        AssignOrdinal (tlhs, rhs, lhs_info);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended =>
        AssignFloat (rhs, lhs_info);
    | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
        AssignReference (tlhs, rhs, lhs_info, isGlobal);
    | Type.Class.Array, Type.Class.OpenArray =>
        AssignArray (tlhs, rhs, lhs_info, isGlobal, compiled);
    | Type.Class.Procedure =>
        AssignProcedure (rhs, lhs_info);
    | Type.Class.Record =>
        AssignRecord (tlhs, rhs, lhs_info, isGlobal);
    | Type.Class.Set =>
        AssignSet (tlhs, rhs, lhs_info);
    ELSE <* ASSERT FALSE *>
    END;
  END Emit;

PROCEDURE AssignOrdinal (tlhs: Type.T;  rhs: Expr.T;
                         READONLY lhs_info: Type.Info) =
  VAR min, max : Target.Int;
  BEGIN
    EVAL Type.GetBounds (tlhs, min, max);
    CheckExpr.Emit (rhs, min, max);
    CG.Store_indirect (lhs_info.cg_type, 0, lhs_info.size);
  END AssignOrdinal;

PROCEDURE AssignFloat (rhs: Expr.T;  READONLY lhs_info: Type.Info) =
  BEGIN
    Expr.Compile (rhs);
    CG.Store_indirect (lhs_info.cg_type, 0, lhs_info.size);
  END AssignFloat;

PROCEDURE AssignReference (tlhs: Type.T;  rhs: Expr.T;
                           READONLY lhs_info: Type.Info;
                           isGlobal: M3.Global := M3.Global.No) =
  VAR
    ok             : CG.Label;
    lhs, t1, test  : CG.Val;
    proc           : Procedure.T;
    callConv       : CG.CallingConvention;
    proc_type      : Type.T;
    (*
      writes of traced refs to global locations get
      handled specially, if we are tracing them
     *)
    special := lhs_info.isTraced AND
               (Host.trace_ref OR Host.ref_count) AND
               isGlobal # M3.Global.No;
  BEGIN
    IF NOT NeedsClosureCheck (rhs, FALSE) THEN
      IF NOT special THEN
        (* standard, old code *)
        lhs := CG.Pop ();
        Expr.Compile (rhs);
        IF Host.doNarrowChk THEN Narrow.Emit (tlhs, Expr.TypeOf (rhs)) END;
        CG.Push (lhs);
        CG.Swap ();
        CG.Store_indirect (lhs_info.cg_type, 0, lhs_info.size);

        (* tracing assignment of traced refs to locals *)
        IF lhs_info.isTraced AND Host.trace_ref THEN
          proc := Runtime.LookUpProc (Runtime.Hook.TraceCount);
          proc_type := Procedure.Signature (proc);
          callConv := ProcType.CallConv (proc_type);

          Procedure.StartCall (proc);

          IF callConv.args_left_to_right THEN
            Type.Compile (tlhs);
            Type.LoadInfo (tlhs, M3RT.TC_typecode);
            CG.Pop_param (CG.Type.Word);
            CG.Load_intt (1);
            CG.Pop_param (CG.Type.Word);
          ELSE
            CG.Load_intt (1);
            CG.Pop_param (CG.Type.Word);
            Type.Compile (tlhs);
            Type.LoadInfo (tlhs, M3RT.TC_typecode);
            CG.Pop_param (CG.Type.Word);
          END;
          test := Procedure.EmitCall (proc);
          <* ASSERT test = NIL *>
        END;
        CG.Free (lhs);
      ELSE
        (* lhs is compiled, rhs is prepped, but need the narrow check *)
        lhs := CG.Pop ();
        Expr.Compile (rhs);
        IF Host.doNarrowChk THEN Narrow.Emit (tlhs, Expr.TypeOf (rhs)) END;
        CG.Push (lhs);
        CG.Swap ();
        Type.GenRC (tlhs, isGlobal = M3.Global.Yes);
        CG.Free (lhs);
      END;
    ELSE
      (* generate closure check -- value is a procedure type *)
      lhs := CG.Pop ();
      Expr.Compile (rhs);
      t1 := CG.Pop ();
      ok := CG.Next_label ();
      CG.If_closure (t1, CG.No_label, ok, CG.Always);
      CG.Narrow_fault ();
      CG.Set_label (ok);
      CG.Push (t1);  CG.Free (t1);
      IF Host.doNarrowChk THEN Narrow.Emit (tlhs, Expr.TypeOf (rhs)) END;
      CG.Push (lhs);
      CG.Swap ();
      CG.Store_indirect (lhs_info.cg_type, 0, lhs_info.size);
      CG.Free (lhs);
    END;
  END AssignReference;

PROCEDURE AssignProcedure (rhs: Expr.T;  READONLY lhs_info: Type.Info) =
  VAR ok: CG.Label;  lhs, t1: CG.Val;
  BEGIN
    IF NOT Host.doNarrowChk THEN
      Expr.Compile (rhs);
    ELSIF NOT NeedsClosureCheck (rhs, FALSE) THEN
      Expr.Compile (rhs);
    ELSE
      lhs := CG.Pop ();
      Expr.Compile (rhs);
      t1 := CG.Pop ();
      ok := CG.Next_label ();
      CG.If_closure (t1, CG.No_label, ok, CG.Always);
      CG.Narrow_fault ();
      CG.Set_label (ok);
      CG.Push (t1);  CG.Free (t1);
      CG.Push (lhs);
      CG.Swap ();
      CG.Free (lhs);
    END;
    CG.Store_indirect (lhs_info.cg_type, 0, lhs_info.size);
  END AssignProcedure;

PROCEDURE AssignRecord (tlhs: Type.T;  rhs: Expr.T;
                        READONLY lhs_info: Type.Info;
                        isGlobal: M3.Global) =
  VAR
    special := lhs_info.isTraced AND
               (Host.trace_ref OR
                (Host.ref_count AND isGlobal # M3.Global.No));
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhs));
    IF Expr.IsDesignator (rhs)
      THEN Expr.CompileLValue (rhs);
      ELSE Expr.Compile (rhs);
    END;
    IF NOT special THEN
      CG.Copy (lhs_info.size, overlap := FALSE);
    ELSE
      Type.GenRC (tlhs, isGlobal = M3.Global.Yes);
    END;
  END AssignRecord;

PROCEDURE AssignSet (tlhs: Type.T;  rhs: Expr.T;
                     READONLY lhs_info: Type.Info) =
  BEGIN
    AssertSmallerSize (tlhs, Expr.TypeOf (rhs));
    IF Type.IsStructured (tlhs) THEN
      IF Expr.IsDesignator (rhs)
        THEN Expr.CompileLValue (rhs);
        ELSE Expr.Compile (rhs);
      END;
      CG.Copy (lhs_info.size, overlap := FALSE);
    ELSE (* small set *)
      Expr.Compile (rhs);
      CG.Store_indirect (lhs_info.cg_type, 0, lhs_info.size);
    END;
  END AssignSet;

PROCEDURE AssertSameSize (a, b: Type.T) = 
  VAR a_info, b_info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (a, a_info);
    EVAL Type.CheckInfo (b, b_info);
    IF (a_info.size # b_info.size) THEN
      Error.Msg ("INTERNAL ERROR: trying to assign values of differing sizes");
      <* ASSERT FALSE *>
    END;
  END AssertSameSize;

PROCEDURE AssertSmallerSize (a, b: Type.T) = 
  VAR a_info, b_info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (a, a_info);
    EVAL Type.CheckInfo (b, b_info);
    IF (a_info.size > b_info.size) THEN
      Error.Msg ("INTERNAL ERROR: rhs SET smaller than lhs SET");
      <* ASSERT FALSE *>
    END;
  END AssertSmallerSize;

PROCEDURE AssignArray (tlhs: Type.T;  e_rhs: Expr.T;
                       READONLY lhs_info: Type.Info;
                       isGlobal: M3.Global;
                       compiled: BOOLEAN) =
  VAR
    trhs    := Expr.TypeOf (e_rhs);
    openRHS := OpenArrayType.Is (trhs);
    openLHS := OpenArrayType.Is (tlhs);
    alignLHS:= ArrayType.EltAlign (tlhs);
    alignRHS:= ArrayType.EltAlign (trhs);
    lhs, rhs: CG.Val;
    rhs_info: Type.Info;
    special := lhs_info.isTraced AND
               (Host.trace_ref OR
                (Host.ref_count AND isGlobal # M3.Global.No));
  BEGIN
    IF compiled THEN
      <* ASSERT openRHS AND openLHS *>
      rhs := CG.Pop ();
    END;

    (* capture the lhs & rhs pointers *)
    IF (openRHS) OR (openLHS) THEN
      lhs := CG.Pop ();
    END;

    IF NOT compiled THEN
      IF Expr.IsDesignator (e_rhs) THEN
        Expr.CompileLValue (e_rhs);
      ELSE
        Expr.Compile (e_rhs);
      END;
      IF (openRHS) OR (openLHS) THEN rhs := CG.Pop (); END;
    END;

    IF openRHS AND openLHS THEN
      IF NOT compiled THEN
        GenOpenArraySizeChecks (lhs, rhs, tlhs, trhs);
      END;

      IF NOT special THEN
        (* standard, old code *)
        CG.Push (lhs);
        CG.Open_elt_ptr (alignLHS);
        CG.Force ();
        CG.Push (rhs);
        CG.Open_elt_ptr (alignRHS);
        CG.Force ();
        GenOpenArrayCopy (rhs, tlhs, trhs, overlap := NOT compiled);
      ELSE
        (* reference counting *)
        CG.Push (lhs);
        CG.Open_elt_ptr (alignLHS); (* compute the data pointer *)
        CG.Push (rhs);
        CG.Open_elt_ptr (alignRHS); (* compute the data pointer *)
        CG.Push (lhs);          (* push header pointers *)
        CG.Push (rhs);          (* push header pointers *)
        Type.GenRC (tlhs, isGlobal = M3.Global.Yes);
      END;
    ELSIF openRHS THEN
      GenOpenArraySizeChecks (lhs, rhs, tlhs, trhs);

      IF NOT special THEN
        CG.Push (lhs);
        CG.Push (rhs);
        CG.Open_elt_ptr (alignRHS);
        CG.Copy (lhs_info.size, overlap := TRUE);
      ELSE
        CG.Push (lhs);
        CG.Push (rhs);
        CG.Open_elt_ptr (alignRHS);
        Type.GenRC (tlhs, isGlobal = M3.Global.Yes);
      END;
    ELSIF openLHS THEN
      EVAL Type.CheckInfo (trhs, rhs_info);
      GenOpenArraySizeChecks (lhs, rhs, tlhs, trhs);

      IF NOT special THEN
        CG.Push (lhs);
        CG.Open_elt_ptr (alignLHS);
        CG.Push (rhs);
        CG.Copy (rhs_info.size, overlap := TRUE);
      ELSE
        CG.Push (lhs);
        CG.Open_elt_ptr (alignLHS);
        CG.Push (rhs);
        Type.GenRC (trhs, isGlobal = M3.Global.Yes);
      END;
    ELSE (* both sides are fixed length arrays *)
      IF NOT special THEN
        (* do not need to worry about reference count *)
        CG.Copy (lhs_info.size, overlap := TRUE);
      ELSE
        (* reference counting *)
        Type.GenRC (tlhs, isGlobal = M3.Global.Yes);
      END;
    END;

    IF (openRHS) OR (openLHS) THEN
      CG.Free (lhs);
      CG.Free (rhs);
    END;
  END AssignArray;

PROCEDURE GenOpenArraySizeChecks (READONLY lhs, rhs: CG.Val;
                                           tlhs, trhs: Type.T) =
  VAR ilhs, irhs, elhs, erhs: Type.T;  n := 0;
  BEGIN
    IF NOT Host.doNarrowChk THEN RETURN END;
    WHILE ArrayType.Split (tlhs, ilhs, elhs)
      AND ArrayType.Split (trhs, irhs, erhs) DO

      IF (ilhs # NIL) AND (irhs # NIL) THEN
        RETURN;
      ELSIF (ilhs # NIL) THEN
        CG.Push (rhs);
        CG.Open_size (n);
        CG.Load_integer (Type.Number (ilhs));
        CG.Check_eq ();
      ELSIF (irhs # NIL) THEN
        CG.Push (lhs);
        CG.Open_size (n);
        CG.Load_integer (Type.Number (irhs));
        CG.Check_eq ();
      ELSE (* both arrays are open *)
        CG.Push (lhs);
        CG.Open_size (n);
        CG.Push (rhs);
        CG.Open_size (n);
        CG.Check_eq ();
      END;
      INC (n);
      tlhs := elhs;
      trhs := erhs;
    END;
  END GenOpenArraySizeChecks;

PROCEDURE GenOpenArrayCopy (READONLY rhs: CG.Val;  tlhs, trhs: Type.T;
                            overlap: BOOLEAN) =
  VAR
    lhs_depth := OpenArrayType.OpenDepth (tlhs);
    rhs_depth := OpenArrayType.OpenDepth (trhs);
  BEGIN
    <*ASSERT (lhs_depth > 0) AND (rhs_depth > 0) *>

    FOR i := 0 TO MIN (lhs_depth, rhs_depth) - 1 DO
      CG.Push (rhs);
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (CG.Type.Word) END;
    END;

    IF (lhs_depth < rhs_depth)
      THEN CG.Copy_n (OpenArrayType.EltPack (tlhs), overlap := overlap);
      ELSE CG.Copy_n (OpenArrayType.EltPack (trhs), overlap := overlap);
    END;
  END GenOpenArrayCopy;

(*---------------------------------------- code generation: checking only ---*)

PROCEDURE EmitCheck (tlhs: Type.T;  rhs: Expr.T) =
  (* on entry the lhs is compiled and the rhs is prepped. *)
  VAR
    t := Type.Base (tlhs); (* strip renaming and packing *)
    lhs_info, t_info: Type.Info;
  BEGIN
    t := Type.CheckInfo (t, t_info);
    tlhs := Type.CheckInfo (tlhs, lhs_info);

    CASE t_info.class OF
    | Type.Class.Integer, Type.Class.Subrange, Type.Class.Enum =>
        DoCheckOrdinal (tlhs, rhs);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended =>
        DoCheckFloat (rhs);
    | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
        DoCheckReference (tlhs, rhs);
    | Type.Class.Array, Type.Class.OpenArray =>
        DoCheckArray (tlhs, rhs);
    | Type.Class.Procedure =>
        DoCheckProcedure (rhs);
    | Type.Class.Record =>
        DoCheckRecord (tlhs, rhs);
    | Type.Class.Set =>
        DoCheckSet (tlhs, rhs);
    ELSE <* ASSERT FALSE *>
    END;
  END EmitCheck;

PROCEDURE DoCheckOrdinal (tlhs: Type.T;  rhs: Expr.T) =
  VAR min, max : Target.Int;
  BEGIN
    EVAL Type.GetBounds (tlhs, min, max);
    CheckExpr.Emit (rhs, min, max);
  END DoCheckOrdinal;

PROCEDURE DoCheckFloat (rhs: Expr.T) =
  BEGIN
    Expr.Compile (rhs);
  END DoCheckFloat;

PROCEDURE DoCheckReference (tlhs: Type.T;  rhs: Expr.T) =
  VAR ok: CG.Label;  t1: CG.Val;
  BEGIN
    (* need to check when RHS is a PROCEDURE *)
    IF NOT NeedsClosureCheck (rhs, FALSE) THEN
      Expr.Compile (rhs);
    ELSE
      Expr.Compile (rhs);
      t1 := CG.Pop ();
      ok := CG.Next_label ();
      CG.If_closure (t1, CG.No_label, ok, CG.Always);
      CG.Narrow_fault ();
      CG.Set_label (ok);
      CG.Push (t1);  CG.Free (t1);
    END;
    IF Host.doNarrowChk THEN Narrow.Emit (tlhs, Expr.TypeOf (rhs)) END;
  END DoCheckReference;

PROCEDURE DoCheckProcedure (rhs: Expr.T) =
  VAR ok: CG.Label;  t1: CG.Val;
  BEGIN
    IF NOT Host.doNarrowChk THEN
      Expr.Compile (rhs);
    ELSIF NOT NeedsClosureCheck (rhs, FALSE) THEN
      Expr.Compile (rhs);
    ELSE
      Expr.Compile (rhs);
      t1 := CG.Pop ();
      ok := CG.Next_label ();
      CG.If_closure (t1, CG.No_label, ok, CG.Always);
      CG.Narrow_fault ();
      CG.Set_label (ok);
      CG.Push (t1);  CG.Free (t1);
    END;
  END DoCheckProcedure;

PROCEDURE DoCheckRecord (tlhs: Type.T;  rhs: Expr.T) =
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhs));
    IF Expr.IsDesignator (rhs)
      THEN Expr.CompileLValue (rhs);
      ELSE Expr.Compile (rhs);
    END;
  END DoCheckRecord;

PROCEDURE DoCheckSet (tlhs: Type.T;  rhs: Expr.T) =
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhs));
    IF Type.IsStructured (tlhs) THEN
      IF Expr.IsDesignator (rhs)
        THEN Expr.CompileLValue (rhs);
        ELSE Expr.Compile (rhs);
      END;
    ELSE (* small set *)
      Expr.Compile (rhs);
    END;
  END DoCheckSet;

PROCEDURE DoCheckArray (tlhs: Type.T;  e_rhs: Expr.T) =
  VAR
    trhs    := Expr.TypeOf (e_rhs);
    openRHS := OpenArrayType.Is (trhs);
    openLHS := OpenArrayType.Is (tlhs);
    rhs     : CG.Val;
  BEGIN
    (* evaluate the right-hand side *)
    IF Expr.IsDesignator (e_rhs)
      THEN Expr.CompileLValue (e_rhs);
      ELSE Expr.Compile (e_rhs);
    END;

    IF openLHS THEN
      Error.Msg ("INTERNAL ERROR: AssignStmt.EmitCheck (OPEN ARRAY)");

    ELSIF openRHS THEN
      rhs := CG.Pop ();
      GenOpenArraySizeChk (rhs, tlhs, trhs);
      CG.Push (rhs);
      CG.Open_elt_ptr (ArrayType.EltAlign (trhs));
      CG.Free (rhs);

    ELSE (* both sides are fixed length arrays *)
      (* no more code to generate *)

    END;

  END DoCheckArray;

PROCEDURE GenOpenArraySizeChk (READONLY rhs: CG.Val;  tlhs, trhs: Type.T) =
  VAR ilhs, irhs, elhs, erhs: Type.T;  n := 0;
  BEGIN
    IF NOT Host.doNarrowChk THEN RETURN END;
    WHILE ArrayType.Split (tlhs, ilhs, elhs)
      AND ArrayType.Split (trhs, irhs, erhs)
      AND (irhs = NIL) DO

      CG.Push (rhs);
      CG.Open_size (n);
      CG.Load_integer (Type.Number (ilhs));
      CG.Check_eq ();

      INC (n);
      tlhs := elhs;
      trhs := erhs;
    END;
  END GenOpenArraySizeChk;

BEGIN
END AssignStmt.

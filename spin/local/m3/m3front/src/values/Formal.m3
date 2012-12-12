(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Formal.m3                                             *)
(* Last modified on Wed Mar  1 08:43:33 PST 1995 by kalsow     *)
(*      modified on Fri Nov  9 20:39:07 1990 by muller         *)

(*
 * HISTORY
 * 04-Nov-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fix error msg
 *
 * 05-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fix arg handling of packed sets -- removed incorrect ASSERTs
 *
 * 18-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	better error msg for formal not matching a VAR/CVAR parameter 
 *
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Check whether a DerefExpr would dereference NIL before passing it
 *	 as a VAR parameter.
 *
 * 08-Jul-96  Wilson Hsieh (whsieh) at the University of Washington
 *	must call Type.Base on tlhs and trhs in ReshapeArray,
 *        because the array could be packed or aligned
 *
 * 25-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	removed unnecessary call to Expr.TypeCheck from DoCheckArgs
 *      simplified some CVAR code
 *      cleaned up last whist entry
 *      Split returns BOOLEAN
 *
 * 15-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	nested procedures cannot be passed as PROCANY/ADDRESS
 *
 * 15-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	ensure that nested procedures that get passed are passed
 *        as closures
 *
 * 29-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	allow UNSAFE EPHEMERAL procedure to pass structures,
 *	 but print out a warning
 *
 * 24-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	bug fix from SRC Web page
 *	READONLY procedure arguments crashed
 *	had to add my own patch to handle CVAR
 *
 * 07-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed up dealing with ALIGNED FOR ARRAYs and VAR/CVAR parameters
 *
 * 03-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	have to deal with Type.Class.Aligned
 *
 * 15-Feb-96  Wilson Hsieh (whsieh) at the University of Washington
 *	moved some ASSERTs that broke because of my CVAR changes
 *
 * 14-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for CVAR formals
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	support for bounded
 *
 *)

MODULE Formal;

IMPORT M3, M3ID, CG, Value, ValueRep, Type, Error, Expr, ProcType;
IMPORT KeywordExpr, OpenArrayType, RefType, CheckExpr;
IMPORT ArrayType, Host, Narrow, M3Buf, Tracer;
IMPORT Procedure, UserProc, Target, M3RT;
IMPORT Null, NamedExpr, Constant, AlignedType;

IMPORT AssignStmt, QualifyExpr, ProcExpr, DerefExpr;
(* for disallowing passing nested PROCEDUREs as ADDRESS/PROCANY *)

IMPORT Ephemeral;

TYPE
  T = Value.T BRANDED OBJECT 
        offset   : INTEGER;
        tipe     : Type.T;
        dfault   : Expr.T;
        refType  : Type.T;
        cg_type  : CG.TypeUID;
        mode     : Mode;
        kind     : Type.Class;
        trace    : Tracer.T;
      OVERRIDES
        typeCheck   := Check;
        set_globals := SetGlobals;
        load        := Load;
        declare     := ValueRep.Never;
        const_init  := ValueRep.NoInit;
        need_init   := ValueRep.Never;
        lang_init   := Compile;
        user_init   := ValueRep.NoInit;
	toExpr      := ValueRep.NoExpr;
	toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := TypeOf;
      END;

TYPE
  ArgSlot = RECORD
    formal  : T;
    actual  : Expr.T;
    name    : M3ID.T;
    matched : BOOLEAN;
    errored : BOOLEAN;
  END;

PROCEDURE NewBuiltin (name: TEXT;  offset: INTEGER;  type: Type.T): Value.T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, M3ID.Add (name), Value.Class.Formal);
    t.readonly := FALSE;
    t.offset   := offset;
    t.mode     := Mode.mVALUE;
    t.tipe     := type;
    t.dfault   := NIL;
    t.unused   := FALSE;
    t.kind     := Type.Class.Error;
    t.refType  := NIL;
    t.cg_type  := 0;
    t.trace    := NIL;
    RETURN t;
  END NewBuiltin;

PROCEDURE New (READONLY info: Info): Value.T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, info.name, Value.Class.Formal);
    t.readonly := (info.mode = Mode.mCONST);
    t.offset   := info.offset;
    t.mode     := info.mode;
    t.tipe     := info.type;
    t.dfault   := info.dfault;
    t.unused   := info.unused;
    t.kind     := Type.Class.Error;
    t.refType  := NIL;
    t.cg_type  := 0;
    t.trace    := info.trace;
    RETURN t;
  END New;

PROCEDURE Split (formal: Value.T;  VAR info: Info) : BOOLEAN =
  BEGIN
    TYPECASE formal OF
    | T (t) =>
      info.name   := t.name;
      info.offset := t.offset;
      info.mode   := t.mode;
      info.type   := TypeOf (t);
      info.dfault := t.dfault;
      info.unused := t.unused;
      info.trace  := t.trace;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Split;

PROCEDURE EmitDeclaration (formal: Value.T;  types_only, param: BOOLEAN) =
  VAR
    t     : T := formal;
    type  : Type.T;
    mtype : CG.Type;
    size  : CG.Size;
    align : CG.Alignment;
    info  : Type.Info;
  BEGIN
    IF (types_only) THEN
      type := TypeOf (t);
      Type.Compile (type);
      t.cg_type := Type.GlobalUID (type);
      IF (t.mode # Mode.mVALUE) OR (t.refType # NIL) THEN
        t.cg_type := CG.Declare_indirect (t.cg_type);
        Type.Compile (t.refType);
      END;
    ELSIF (param) THEN
      type  := TypeOf (t);
      IF (t.mode = Mode.mVALUE) AND (t.refType = NIL) THEN
        EVAL Type.CheckInfo (type, info);
        size  := info.size;
        align := info.alignment;
        mtype := info.cg_type;
      ELSE
        size  := Target.Address.size;
        align := Target.Address.align;
        mtype := CG.Type.Addr;
      END;
      EVAL CG.Declare_param (t.name, size, align, mtype,
                             t.cg_type, in_memory := FALSE, up_level := FALSE,
                             f := CG.Maybe);
    ELSE
      CG.Declare_formal (t.name, t.cg_type);
    END;
  END EmitDeclaration;

PROCEDURE HasClosure (formal: Value.T): BOOLEAN =
  BEGIN
    TYPECASE formal OF
    | NULL => RETURN FALSE;
    | T(t) => RETURN (t.mode # Mode.mVAR)
                 AND (t.mode # Mode.mCVAR)
                 AND ((t.kind = Type.Class.Procedure)
                      OR ProcType.Is (Type.Base (TypeOf (t))));
    ELSE      RETURN FALSE;
    END;
  END HasClosure;

PROCEDURE RefOpenArray (formal: Value.T;  VAR ref: Type.T): BOOLEAN =
  BEGIN
    TYPECASE formal OF
    | NULL => RETURN FALSE;
    | T(t) => ref := t.refType;  RETURN (ref # NIL);
    ELSE      RETURN FALSE;
    END;
  END RefOpenArray;

PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    IF (t.tipe = NIL) THEN t.tipe := Expr.TypeOf (t.dfault) END;
    RETURN t.tipe;
  END TypeOf;

PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
  VAR info: Type.Info;
  BEGIN
    t.tipe := Type.CheckInfo (TypeOf (t), info);
    t.kind := info.class;
    IF (info.class = Type.Class.Packed) OR (info.class = Type.Class.Aligned)
     THEN
      EVAL Type.CheckInfo (Type.Base (t.tipe), info);
      t.kind := info.class;
    END;

    IF (t.dfault # NIL) THEN
      Expr.TypeCheck (t.dfault, cs);
      IF (t.mode = Mode.mVAR) THEN
        Error.ID (t.name, "VAR parameters cannot have defaults");
      END;
      IF  NOT Type.IsAssignable (t.tipe, Expr.TypeOf (t.dfault)) THEN
        Error.ID (t.name, "default is not assignable to formal");
      END;
      IF (Expr.ConstValue (t.dfault) = NIL) THEN
        Error.ID (t.name, "default is not constant");
      END;
      (* NOTE: we don't save the constant-folded version of the default,
         otherwise we'd loose references to large named constants. *)
    END;

    IF (t.mode = Mode.mVALUE) AND OpenArrayType.Is (Type.Base (t.tipe)) THEN
      t.refType := RefType.New (t.tipe, traced := TRUE, brand := NIL);
      t.refType := Type.Check (t.refType);
    END;

  END Check;

PROCEDURE Load (t: T) =
  BEGIN
    IF (t.dfault = NIL) THEN
      Error.ID (t.name, "formal has no default value");
    END;
    Expr.Prep (t.dfault);
    Expr.Compile (t.dfault);
  END Load;

PROCEDURE Compile (t: T) =
  BEGIN
    Type.Compile (t.tipe);
    Type.Compile (t.refType);
    IF (t.dfault # NIL) THEN
      Type.Compile (Expr.TypeOf (t.dfault));
    END;
  END Compile;

PROCEDURE SetGlobals (<*UNUSED*> t: T) =
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    (* Type.SetGlobals (t.refType); *)
    (* IF (t.dfault # NIL) THEN Type.SetGlobals (Expr.TypeOf (t.dfault)) END; *)
  END SetGlobals;

PROCEDURE AddFPTag  (t: T;  VAR x: M3.FPInfo): CARDINAL =
  CONST Tags = ARRAY Mode OF TEXT { "VALUE ", "VAR ", "READONLY ", "CVAR "};
  BEGIN
    ValueRep.FPStart (t, x, Tags[t.mode], 0, global := FALSE);
    IF (t.dfault # NIL) THEN
      M3Buf.PutText (x.buf, " := ");
      Expr.GenFPLiteral (t.dfault, x.buf);
    END;
    RETURN 1;
  END AddFPTag;

(*--------------------------------------------------- actual typechecking ---*)

PROCEDURE CheckArgs (VAR cs       : Value.CheckState;
                     VAR actuals  : Expr.List;
                         formals  : Value.T;
                         proc     : Expr.T): BOOLEAN =
  VAR slots: ARRAY [0..19] OF ArgSlot;  v: Value.T;  n: INTEGER;
  BEGIN
    v := formals;  n := 0;
    WHILE (v # NIL) DO  INC (n);  v := v.next;  END;
    IF (n <= NUMBER (slots))
      THEN RETURN DoCheckArgs (cs, actuals, formals, n, slots, proc);
      ELSE RETURN DoCheckArgs (cs, actuals, formals, n,
                                NEW (REF ARRAY OF ArgSlot, n)^, proc);
    END;
  END CheckArgs;

     
PROCEDURE DoCheckArgs (<* UNUSED *> VAR cs : Value.CheckState;
                       VAR actuals  : Expr.List;
                           formals  : Value.T;
                           nFormals : INTEGER;
                       VAR slots    : ARRAY OF ArgSlot;
                           proc     : Expr.T): BOOLEAN =
  VAR
    j                 : INTEGER;
    e, value          : Expr.T;
    index, elt, t, te : Type.T; 
    posOK, ok         : BOOLEAN;
    name              : M3ID.T;
    tt                : T;
    tmp               : INTEGER;
  BEGIN
    ok := TRUE;

    IF (nFormals < NUMBER (actuals^)) THEN
      Error.Msg ("too many actual parameters" & ProcName (proc));
      ok := FALSE;
    END;

    (* initialize the argument list *)
    tt := formals;
    WHILE (tt # NIL) DO
      WITH z = slots[tt.offset] DO
        z.formal  := tt;
        z.actual  := tt.dfault;
        z.matched := FALSE;
        z.errored := FALSE;
        z.name    := tt.name;
      END;
      tt := tt.next;
    END;

    (* bind the parameters *)
    posOK := TRUE;
    FOR i := 0 TO MIN (LAST (actuals^) , nFormals -1) DO
      e := actuals[i];
      IF KeywordExpr.Split (e, name, value) THEN
        posOK := FALSE;
        e := value;
        j := 0;
        LOOP
          IF (j >= nFormals) THEN
            Error.ID (name, "unknown parameter");
            ok := FALSE;
            j := i;
            EXIT;
          END;
          IF (slots[j].name = name) THEN EXIT END;
          INC (j);
        END;
      ELSE
        IF (NOT posOK) THEN
          Error.Msg ("positional parameters must precede keyword parameters"
                       & ProcName (proc));
          ok := FALSE;
        END;
        j := i;
      END;
      WITH z = slots[j] DO
        IF (z.matched) THEN
          Err (z, "parameter already specified");
          ok := FALSE;
        END;
        z.matched := TRUE;
        z.actual := e;
      END;
    END;

    (* check for any unspecified parameters *)
    FOR i := 0 TO nFormals - 1 DO
      IF (slots[i].actual # NIL) THEN slots[i].matched := TRUE END;
      IF NOT slots[i].matched THEN
        Err (slots[i], "parameter not specified");
        ok := FALSE;
      END;
    END;

    (* typecheck each binding *)
    FOR i := 0 TO nFormals - 1 DO
      e  := slots[i].actual;
      tt := slots[i].formal;
      IF (e # NIL) AND (tt # NIL) THEN
        (* we have both a formal and an actual *)
        (* try to fold scalar constant values *)
        (* NOTE: if we fold named structured constants, we lose the
             names and hence generate code to build the value... *)
        IF NOT Type.IsStructured (tt.tipe) THEN
          value := Expr.ConstValue (e);
          IF (value # NIL) THEN e := value;  slots[i].actual := e END;
        END;

        te := Expr.TypeOf (e);
        t  := tt.tipe;

        (* need to disallow passing of nested procedure as ADDRESS/PROCANY *)
        VAR name: M3ID.T;  obj: Value.T;  class: Value.Class;  nested: BOOLEAN;
        BEGIN
          IF (NamedExpr.Split (e, name, obj)
            OR QualifyExpr.Split (e, obj)
            OR ProcExpr.Split (e, obj)) THEN
            obj := Value.Base (obj);
            class := Value.ClassOf (obj);
            IF (class = Value.Class.Procedure) THEN
              nested := Procedure.IsNested (obj);
              IF (nested) AND RefType.Is (t) THEN
                Err (slots[i], "passing a nested procedure as an ADDRESS/PROCANY");
              END;
            END;
          END;
        END;

        CASE tt.mode OF
        | Mode.mVALUE =>
            IF NOT Type.IsAssignable (t, te) THEN
              Err (slots[i], Type.ToText(te) & " argument is not compatible with " & Type.ToText(t) & " formal");
              ok := FALSE;
            END;

            IF Type.IsStructured (t) THEN
              CASE Ephemeral.Check () OF
              | Ephemeral.Return.Error =>
                Err (slots[i], "passing a structure/set/array by value inside a EPHEMERAL procedure");
                ok := FALSE;
              | Ephemeral.Return.Warn =>
                Error.WarnID (2, slots[i].name, "passing a structure/set/array by value inside an UNSAFE EPHEMERAL procedure");
              | Ephemeral.Return.Ok =>
              END;
            END;
        | Mode.mVAR, Mode.mCVAR =>
            IF tt.mode = Mode.mCVAR AND IsNil (e) THEN
              (* OK *)
            ELSIF NOT Expr.IsDesignator (e) THEN
              IF tt.mode = Mode.mCVAR THEN
                Err (slots[i], "Actual passed as CVAR must be a designator");
              ELSE
                Err (slots[i], "Actual passed as VAR must be a designator");
              END;
              ok := FALSE;
            ELSIF NOT Expr.IsWritable (e) THEN
              IF tt.mode = Mode.mCVAR THEN
                Err (slots[i], "Actual passed as VAR must be writable");
              ELSE
                Err (slots[i], "Actual passed as CVAR must be writable or NIL");
              END;
              ok := FALSE;
            ELSIF Type.IsEqual (t, te, NIL) THEN
              Expr.NeedsAddress (e);
            ELSIF ArrayType.Split (t, index, elt)
              AND Type.IsAssignable (t, te) THEN
              Expr.NeedsAddress (e);
            ELSIF AlignedType.Split (te, tmp, elt)
              AND Type.IsEqual (elt, t, NIL) THEN
              Expr.NeedsAddress (e);
            ELSE
              Err (slots[i], Type.ToText(te) & " argument is not compatible with " & Type.ToText(t) & " VAR/CVAR formal");
              ok := FALSE;
            END;
        | Mode.mCONST =>
            IF NOT Type.IsAssignable (t, te) THEN
              Err (slots[i], Type.ToText(te) & " argument is not compatible with " & Type.ToText(t) & " formal");
              ok := FALSE;
            ELSIF NOT Expr.IsDesignator (e) THEN
              (* we'll make a copy when it's generated *)
            ELSIF Type.IsEqual (t, te, NIL) THEN
              Expr.NeedsAddress (e);
            ELSE (* Type.IsAssignable (t, te) *)
              (* we'll make a copy when it's generated *)
            END;
        END; (*case*)
      END; (* if got actual & formal *)
    END; (* for *)

    IF (NOT ok) THEN RETURN FALSE END;

    (* no more possible errors => build the new argument list *)
    IF (NUMBER (actuals^) # nFormals) THEN 
      actuals := NEW (Expr.List, nFormals) 
    END;
    FOR i := 0 TO nFormals - 1 DO  actuals[i] := slots[i].actual  END;
    RETURN TRUE;
  END DoCheckArgs;

PROCEDURE Err (VAR slot: ArgSlot;  msg: TEXT) =
  BEGIN
    IF (NOT slot.errored) THEN
      Error.ID (slot.name, msg);
      slot.errored := TRUE;
    END;
  END Err;

PROCEDURE ProcName (proc: Expr.T): TEXT =
  VAR v: Value.T;
  BEGIN
    IF (proc # NIL) AND UserProc.IsProcedureLiteral (proc, v) THEN
      RETURN ": " & Value.GlobalName (v, dots := TRUE, with_module := TRUE);
    ELSE
      RETURN "";
    END;
  END ProcName;

(*----------------------------------------------------------- caller code ---*)

PROCEDURE PrepArg (formal: Value.T; actual: Expr.T) =
  VAR t: T := formal;
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Prep (actual);
    | Mode.mVAR =>
        Expr.PrepLValue (actual);
    | Mode.mCVAR =>
        IF IsNil(actual) THEN
          Expr.Prep (actual);
        ELSE
          Expr.PrepLValue (actual);
        END;
    | Mode.mCONST =>
        IF NOT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) THEN
          Expr.Prep (actual);
        ELSIF Expr.IsDesignator (actual) THEN
          Expr.PrepLValue (actual);
        ELSE (* non-designator, same type *)
          Expr.Prep (actual);
        END;
    END;

    IF t.kind = Type.Class.Ref AND
       AssignStmt.NeedsClosureCheck (actual, FALSE)
     THEN
      (* AssignStmt.EmitCheck requires that Prep has been called on actual
         PrepLValue (not Prep) called for:
           VAR/CVAR parameter, READONLY parameter where types
             are equal and actual is a designator
           In these cases, AssignStmt.NeedsClosureCheck will not
             be true (because types will not be equal
       *)
      AssignStmt.EmitCheck (t.tipe, actual);
      EVAL CG.Pop ();
    END;

  END PrepArg;

PROCEDURE EmitArg (proc: Expr.T;  formal: Value.T; actual: Expr.T) =
  VAR t: T := formal;   is_struct := FALSE;  info: Type.Info;
      trhs := Type.Base (Expr.TypeOf (actual)); rhs_info: Type.Info;
  BEGIN
    trhs := Type.CheckInfo (trhs, rhs_info);

    CASE t.kind OF
    | Type.Class.Error, Type.Class.Named, Type.Class.Packed, Type.Class.Aligned
      =>  <*ASSERT FALSE*>
    | Type.Class.Integer, Type.Class.Enum, Type.Class.Subrange
      =>  GenOrdinal (t, actual);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended
      =>  GenFloat (t, actual);
    | Type.Class.Ref, Type.Class.Object, Type.Class.Opaque
      =>  GenReference (t, actual);
    | Type.Class.Procedure
      =>  GenProcedure (t, actual, proc);
    | Type.Class.Record
      =>  GenRecord (t, actual);  is_struct := TRUE;
    | Type.Class.Set
      =>  GenSet (t, actual);     is_struct := Type.IsStructured (t.tipe);
    | Type.Class.Array
      =>  GenArray (t, actual);   is_struct := TRUE;
    | Type.Class.OpenArray
      =>  GenArray (t, actual);   is_struct := FALSE;
    END;

    IF (t.mode = Mode.mCVAR) AND IsNil(actual) THEN
      CG.Pop_param (CG.Type.Word);
    ELSIF (t.mode # Mode.mVALUE) THEN
      IF DerefExpr.Is(actual) THEN
        CG.Check_nil();
      END;
      CG.Pop_param (CG.Type.Addr)
    ELSIF (is_struct) THEN
      EVAL Type.CheckInfo (t.tipe, info);
      CG.Pop_struct (info.size, info.alignment);
    ELSE (* by-value scalar *)
      CG.Pop_param (Type.CGType (t.tipe));
    END;
    IF (t.mode = Mode.mVAR) OR
      ((t.mode = Mode.mCVAR) AND NOT IsNil(actual)) THEN
      Expr.NoteWrite (actual);
    END;
  END EmitArg;

PROCEDURE GenOrdinal (t: T;  actual: Expr.T) =
  VAR min, max: Target.Int;  (** constant := Expr.ConstValue (actual); **)
  BEGIN
    (***
      -- we cannot fold constant actuals since they may have been precompiled
         and have allocated temporaries that still need to be freed ....
    IF (constant # NIL) THEN actual := constant END;
    ***)
    EVAL Type.GetBounds (t.tipe, min, max);
    CASE t.mode OF
    | Mode.mVALUE =>
        CheckExpr.Emit (actual, min, max);
    | Mode.mVAR =>
        Expr.CompileAddress (actual);
    | Mode.mCONST =>
        IF NOT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) THEN
          CheckExpr.Emit (actual, min, max);
          GenCopy (t.tipe);
        ELSIF Expr.IsDesignator (actual) THEN
          Expr.CompileAddress (actual);
        ELSE (* non-designator, same type *)
          Expr.Compile (actual);
          GenCopy (t.tipe);
        END;
    | Mode.mCVAR =>
        IF IsNil(actual) THEN
          Expr.Compile (actual);
        ELSE
          Expr.CompileAddress (actual);
        END;
    END;
  END GenOrdinal;

PROCEDURE GenFloat (t: T;  actual: Expr.T) =
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actual);
    | Mode.mVAR =>
        Expr.CompileAddress (actual);
    | Mode.mCONST =>
        IF Expr.IsDesignator (actual) THEN
          Expr.CompileAddress (actual);
        ELSE
          Expr.Compile (actual);
          GenCopy (t.tipe);
        END;
    | Mode.mCVAR =>
        IF IsNil(actual) THEN
          Expr.Compile (actual);
        ELSE
          Expr.CompileAddress (actual);
        END;
    END;
  END GenFloat;

PROCEDURE GenReference (t: T;  actual: Expr.T) =
  VAR t_actual := Expr.TypeOf (actual);
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actual);
        IF Host.doNarrowChk THEN Narrow.Emit (t.tipe, t_actual) END;
    | Mode.mVAR =>
        Expr.CompileAddress (actual);
    | Mode.mCONST =>
        IF NOT Type.IsEqual (t.tipe, t_actual, NIL) THEN
          Expr.Compile (actual);
          IF Host.doNarrowChk THEN Narrow.Emit (t.tipe, t_actual) END;
          GenCopy (t.tipe);
        ELSIF Expr.IsDesignator (actual) THEN
          Expr.CompileAddress (actual);
        ELSE
          Expr.Compile (actual);
          GenCopy (t.tipe);
        END;
    | Mode.mCVAR =>
        IF IsNil(actual) THEN
          Expr.Compile (actual);
        ELSE
          Expr.CompileAddress (actual);
        END;
    END;
  END GenReference;

PROCEDURE GenProcedure (t: T;  actual: Expr.T;  proc: Expr.T) =
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actual);
        GenClosure (actual, proc);
    | Mode.mVAR =>
        Expr.CompileAddress (actual);
    | Mode.mCONST =>
        IF Expr.IsDesignator (actual) THEN
          Expr.CompileAddress (actual);
        ELSE
          Expr.Compile (actual);
          GenClosure (actual, proc);
          GenCopy (t.tipe);
        END;
    | Mode.mCVAR =>
        IF IsNil(actual) THEN
          Expr.Compile (actual);
        ELSE
          Expr.CompileAddress (actual);
        END;
    END;
  END GenProcedure;

PROCEDURE GenClosure (actual: Expr.T;  proc: Expr.T) =
  VAR tmp: CG.Var;  proc_v: Value.T;  n_elts: INTEGER;
    ASIZE := Target.Address.size;
  BEGIN
    IF RequiresClosure (actual, proc_v) THEN
      (* actual is a nested procedure literal passed by value *)
      IF IsExternalProcedure (proc) THEN
        Error.Warn (1, "passing nested procedure to external procedure");
      END;

      (* allocate space for the closure *)
      n_elts := (M3RT.CL_SIZE + ASIZE - 1) DIV ASIZE;
      tmp := CG.Declare_temp (M3RT.CL_SIZE, Target.Address.align,
                              CG.Type.Struct, in_memory := TRUE);

      (* and fill it in *)
      CG.Store_addr (tmp, M3RT.CL_proc);
      CG.Load_intt  (M3RT.CL_marker_value);
      CG.Store_int (tmp, M3RT.CL_marker);
      Procedure.LoadStaticLink (proc_v);
      CG.Store_addr (tmp, M3RT.CL_frame);
      CG.Load_addr_of_temp (tmp,  0, Target.Address.align);
    END;
  END GenClosure;

PROCEDURE RequiresClosure (e: Expr.T;  VAR proc: Value.T): BOOLEAN =
  BEGIN
    RETURN UserProc.IsProcedureLiteral (e, proc) AND Procedure.IsNested (proc);
  END RequiresClosure;

PROCEDURE IsExternalProcedure (e: Expr.T): BOOLEAN =
  VAR proc: Value.T;
  BEGIN
    RETURN UserProc.IsProcedureLiteral (e, proc) AND Value.IsExternal (proc);
  END IsExternalProcedure;

PROCEDURE GenRecord (t: T;  actual: Expr.T) =
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        <* ASSERT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) *>
        Expr.Compile (actual);
    | Mode.mVAR =>
        <* ASSERT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) *>
        Expr.CompileAddress (actual);
    | Mode.mCONST =>
        <* ASSERT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) *>
        IF Expr.IsDesignator (actual) THEN
          Expr.CompileAddress (actual);
        ELSE
          Expr.Compile (actual);
          (* not needed because of the ASSERT above: GenCopy (t.tipe); *)
        END;
    | Mode.mCVAR =>
        IF IsNil(actual) THEN
          Expr.Compile (actual);
        ELSE
          <* ASSERT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) *>
          Expr.CompileAddress (actual);
        END;
    END;
  END GenRecord;

PROCEDURE GenSet (t: T;  actual: Expr.T) =
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actual);
    | Mode.mVAR =>
        <* ASSERT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) *>
        Expr.CompileAddress (actual);
    | Mode.mCONST =>
        IF Expr.IsDesignator (actual) THEN
          Expr.CompileAddress (actual);
        ELSIF Type.IsStructured (t.tipe) THEN
          Expr.Compile (actual);
          (* not needed because of the ASSERT above: GenCopy (t.tipe); *)
        ELSE (* small, non-designator set *)
          Expr.Compile (actual);
          GenCopy (t.tipe);
        END;
    | Mode.mCVAR =>
        IF IsNil(actual) THEN
          Expr.Compile (actual);
        ELSE
          <* ASSERT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) *>
          Expr.CompileAddress (actual);
        END;
    END;
  END GenSet;

PROCEDURE GenArray (t: T;  actual: Expr.T) =
  VAR t_actual := Expr.TypeOf (actual);
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actual);
        ReshapeArray (t.tipe, t_actual);
    | Mode.mVAR =>
        Expr.CompileAddress (actual);
        ReshapeArray (t.tipe, t_actual);
    | Mode.mCONST =>
        IF NOT Type.IsEqual (t.tipe, t_actual, NIL) THEN
          Expr.Compile (actual);
          ReshapeArray (t.tipe, t_actual);
        ELSIF Expr.IsDesignator (actual) THEN
          Expr.CompileAddress (actual);
        ELSE
          Expr.Compile (actual);
        END;
    | Mode.mCVAR =>
        IF IsNil(actual) THEN
          Expr.Compile (actual);
        ELSE
          Expr.CompileAddress (actual);
          ReshapeArray (t.tipe, t_actual);
        END;
    END;
  END GenArray;

PROCEDURE ReshapeArray (tlhs, trhs: Type.T) =
  VAR
    d_lhs, d_rhs: INTEGER;
    index, elt: Type.T;
    tmp: CG.Var;
    rhs: CG.Val;
    b: BOOLEAN;
  BEGIN
    (* get rid of special packing/alignment *)
    tlhs := Type.Base (tlhs);
    trhs := Type.Base (trhs);

    IF Type.IsEqual (tlhs, trhs, NIL) THEN RETURN END;

    d_lhs := OpenArrayType.OpenDepth (tlhs);
    d_rhs := OpenArrayType.OpenDepth (trhs);

    IF (d_lhs = d_rhs) THEN RETURN END;

    (* capture the actual *)
    rhs := CG.Pop ();

    IF (d_lhs > d_rhs) THEN
      (* build a bigger dope vector *)
      tmp := OpenArrayType.DeclareTemp (tlhs);

      (* copy the data pointer *)
      CG.Push (rhs);
      IF (d_rhs > 0) THEN CG.Open_elt_ptr (Target.Byte) END;
      CG.Store_addr (tmp, M3RT.OA_elt_ptr);

      (* fill in the sizes *)
      FOR i := 0 TO d_lhs-1 DO
        b := ArrayType.Split (trhs, index, elt);  <*ASSERT b*>
        IF (index = NIL) THEN
          CG.Push (rhs);
          CG.Open_size (i);
        ELSE
          CG.Load_integer (Type.Number (index));
        END;
        CG.Store_int (tmp, M3RT.OA_sizes + i * Target.Integer.pack);
        trhs := elt;
      END;

      (* leave the result *)
      CG.Load_addr_of_temp (tmp, 0, Target.Address.align);
    ELSE
      <*ASSERT d_lhs < d_rhs *>
      (* check some array bounds;  do not build a smaller dope vector
         just reuse the existing one! *)

      tlhs := OpenArrayType.OpenType (tlhs);
      FOR i := d_lhs TO d_rhs - 1 DO
        b := ArrayType.Split (tlhs, index, elt); <*ASSERT b*>
        <*ASSERT index # NIL*>
        CG.Push (rhs);
        CG.Open_size (i);
        CG.Load_integer (Type.Number (index));
        CG.Check_eq ();
      END;

      (* leave the old dope vector as the result *)
      CG.Push (rhs);
      IF (d_lhs <= 0) THEN CG.Open_elt_ptr (Target.Byte); END;
      CG.Force ();
    END;

    CG.Free (rhs);
  END ReshapeArray;

PROCEDURE GenCopy (type: Type.T) =
  VAR info: Type.Info;  tmp: CG.Var;  id: CG.TypeUID;
  BEGIN
    EVAL Type.CheckInfo (type, info);
    id := Type.GlobalUID (type);
    IF Type.IsStructured (type) THEN
      tmp := CG.Declare_local (M3ID.NoID, info.size, info.alignment,
                               CG.Type.Struct, id, in_memory := TRUE,
                               up_level := FALSE, f := CG.Never);
      CG.Load_addr_of (tmp, 0, info.alignment);
      CG.Swap ();
      CG.Copy (info.size, overlap := FALSE);
    ELSE
      tmp := CG.Declare_local (M3ID.NoID, info.size, info.alignment,
                               info.cg_type, id, in_memory := TRUE,
                               up_level := FALSE, f := CG.Never);
      CG.Store (tmp, 0, info.size, info.alignment, info.cg_type);
    END;
    CG.Load_addr_of (tmp, 0, info.alignment);
  END GenCopy;

PROCEDURE IsNil (e: Expr.T): BOOLEAN =
  VAR
    name: M3ID.T;
    value: Value.T;
    base: Expr.T;
  BEGIN
    RETURN
      ((e = Null.Nil) OR
        (NamedExpr.Split (e, name, value) AND
         Constant.Split (value, base) AND
         base = Null.Nil));
  END IsNil;

BEGIN
END Formal.

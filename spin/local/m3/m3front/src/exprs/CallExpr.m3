(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CallExpr.m3                                           *)
(* Last modified on Mon Dec  5 15:33:06 PST 1994 by kalsow     *)
(*      modified on Wed Nov  7 01:30:54 1990 by muller         *)

(*
 * HISTORY
 * 16-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	make sure -SpinRelax turns off Type.Method checking for calls
 *
 * 21-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	check SUBARRAY for globalness too
 *
 * 09-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed checking of globalness
 *
 * 27-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	note whether a call is to an inline procedure
 *
 * 25-Jul-96  Wilson Hsieh (whsieh) at the University of Washington
 *	allow method restrictions to be turned off
 *
 * 10-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	More accurate check for a full revelation when using TYPE.METHOD
 *	 procedure call expressions.
 *
 * 29-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	allow UNSAFE EPHEMERAL procedure to call non-EPHEMERAL procedure,
 *	 but print out a warning
 *	fixed whist entry conflict
 *
 * 16-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed error message to print EPHEMERAL (not BOUNDED)
 *      removed auto-magic dereference for REF PROCEDUREs
 *
 * 05-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for restricting TYPE.METHOD, which cannot be passed as an
 *	 argument
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	support for bounded
 *
 * 23-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	allow implicit dereferencing of REF PROCEDURE in call
 *
 *)

MODULE CallExpr;

IMPORT CG, Expr, ExprRep, Error, ProcType, Type, UserProc;
IMPORT KeywordExpr, ESet, QualifyExpr, ErrType, Value, Revelation;

IMPORT Ephemeral, Functional;
IMPORT Host;                   (* for disabling method restrictions *)
IMPORT M3, Loophole, View, Subarray;     (* for checking globalness *)
IMPORT Target;

REVEAL
  MethodList = BRANDED "CallExpr.MethodList" REF RECORD
                 minArgs      : INTEGER;
                 maxArgs      : INTEGER;
                 functional   : BOOLEAN;
                 keywords     : BOOLEAN;
                 strict       : BOOLEAN;
                 fixedType    : Type.T;
                 typeOf       : Typer;
                 need_addr    : Visitor;
                 checker      : TypeChecker;
                 prep         : Compiler;
                 compiler     : Compiler;
                 prepLV       : CompilerLV;
                 compilerLV   : CompilerLV;
                 prepBR       : CompilerBR;
                 compilerBR   : CompilerBR;
                 evaluator    : Evaluator;
                 isWritable   : Predicate;
                 isDesignator : Predicate;
                 noteWriter   : NoteWriter;
                 (* SPIN *)
                 getBounds    : Bounder;
               END;

REVEAL
  T = T_ BRANDED "CallExpr.P" OBJECT
	methods  : MethodList;
        proc_type: Type.T;
        (* SPIN *)
        is_bounded   : BOOLEAN := FALSE;
        is_functional: BOOLEAN := FALSE;
        is_inline    : BOOLEAN := FALSE;
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := PrepLV;
        compileLV    := CompileLV;
        prepBR       := PrepBR;
        compileBR    := CompileBR;
        evaluate     := Fold;
        isEqual      := ExprRep.NeverEq;
        getBounds    := GetBounds;
        isWritable   := IsWritable;
        isDesignator := IsDesignator;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := ExprRep.NoFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
	note_write   := NoteWrites;
        global       := IsGlobal;
      END;

PROCEDURE New (proc: Expr.T;  args: Expr.List): Expr.T =
  VAR p := NEW (T);
  BEGIN
    ExprRep.Init (p);
    p.proc     := proc;
    p.args     := args;
    p.tmp      := NIL;
    p.methods  := NIL;
    p.proc_type:= NIL;
    RETURN p;
  END New;

PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
      | NULL => RETURN FALSE;
      | T    => RETURN TRUE;
      ELSE      RETURN FALSE;
    END;   
  END Is;

PROCEDURE NewMethodList (minArgs, maxArgs: INTEGER;
                         functional   : BOOLEAN;
                         keywords     : BOOLEAN;
                         strict       : BOOLEAN;
                         fixedType    : Type.T;
                         typeOf       : Typer;
                         need_addr    : Visitor;
                         checker      : TypeChecker;
                         prep         : Compiler;
			 compiler     : Compiler;
                         prepLV       : CompilerLV;
			 compilerLV   : CompilerLV;
                         prepBR       : CompilerBR;
			 compilerBR   : CompilerBR;
                         evaluator    : Evaluator;
                         isWritable   : Predicate;
                         isDesignator : Predicate;
                         noteWriter   : NoteWriter;
                         getBounds    : Bounder := NoBoundsInfo): MethodList =
  VAR m: MethodList;
  BEGIN
    m := NEW (MethodList);
    m.minArgs      := minArgs;
    m.maxArgs      := maxArgs;
    m.functional   := functional;
    m.keywords     := keywords;
    m.strict       := strict;
    m.fixedType    := fixedType;
    m.typeOf       := typeOf;
    m.need_addr    := need_addr;
    m.checker      := checker;
    m.prep         := prep;
    m.compiler     := compiler;
    m.prepLV       := prepLV;
    m.compilerLV   := compilerLV;
    m.prepBR       := prepBR;
    m.compilerBR   := compilerBR;
    m.evaluator    := evaluator;
    m.isWritable   := isWritable;
    m.isDesignator := isDesignator;
    m.noteWriter   := noteWriter;
    m.getBounds    := getBounds;
    RETURN m;
  END NewMethodList;

PROCEDURE IsNever (<*UNUSED*> t: T): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END IsNever;

PROCEDURE IsAlways (<*UNUSED*> t: T): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END IsAlways;

PROCEDURE NoValue (<*UNUSED*> t: T): Expr.T =
  BEGIN
    RETURN NIL;
  END NoValue;

PROCEDURE NotAddressable (<*UNUSED*> t: T) =
  BEGIN
    <* ASSERT FALSE *>
  END NotAddressable;

PROCEDURE PrepArgs (t: T) =
  BEGIN
    FOR i := 0 TO LAST (t.args^) DO
      Expr.Prep (t.args[i]);
    END;
  END PrepArgs;

PROCEDURE NoLValue (<*UNUSED*> t: T) =
  BEGIN
    <*ASSERT FALSE*>
  END NoLValue;

PROCEDURE NotBoolean (<*UNUSED*> t: T;
                      <*UNUSED*> true, false: CG.Label;
                      <*UNUSED*> freq: CG.Frequency) =
  BEGIN
    <*ASSERT FALSE*>
  END NotBoolean;

PROCEDURE PrepNoBranch (t: T;  true, false: CG.Label;  freq: CG.Frequency) =
  BEGIN
    t.prep ();
    t.compile ();
    IF (true # CG.No_label)
      THEN CG.If_true (true, freq);
      ELSE CG.If_false (false, freq);
    END;
  END PrepNoBranch;

PROCEDURE NoBranch (<*UNUSED*> t: T;
                    <*UNUSED*> true, false: CG.Label;
                    <*UNUSED*> freq: CG.Frequency) =
  BEGIN
    (* all the work was done by prep *)
  END NoBranch;

PROCEDURE NotWritable (<*UNUSED*> t: T)=
  BEGIN
    (* skip *)
  END NotWritable;

PROCEDURE NoBoundsInfo(t: T; VAR min, max: Target.Int) =
  BEGIN
    (* Compute the bounds based on the return type of the call. *)
    ExprRep.NoBounds(t, min, max);
  END NoBoundsInfo;

(***********************************************************************)

PROCEDURE Resolve (p: T) =
  VAR t: Type.T;
  BEGIN
    IF (p.methods # NIL) THEN RETURN END;
    t := Expr.TypeOf (p.proc);
    IF (t = NIL) THEN
      t := QualifyExpr.MethodType (p.proc);
      (* we need this hack because "TypeOf(obj.method)" returns NIL
         so that you cannot use it as a vanilla procedure value. *)
    END;
    p.methods := ProcType.Methods (t);
    p.proc_type := t;
    p.is_bounded := ProcType.IsBounded (t);
    p.is_functional := ProcType.IsFunctional (t);
    p.is_inline := ProcType.IsInline (t);
  END Resolve;

PROCEDURE TypeOf (p: T): Type.T =
  BEGIN
    Resolve (p);
    IF (p.methods = NIL) THEN
      p.type := NIL;
    ELSIF (p.methods.fixedType # NIL) OR (p.methods.typeOf = NIL) THEN
      p.type := p.methods.fixedType;
    ELSE
      FixArgs (p);
      p.type := p.methods.typeOf (p);
    END;
    RETURN p.type;
  END TypeOf;

PROCEDURE Check (p: T;  VAR cs: Expr.CheckState) =
  VAR
    nErrs0, nErrs1, nWarns: INTEGER;
    arg: Expr.T;
    keywords: BOOLEAN;
    tmp: Type.T; (* ignored *)
  BEGIN
    (* check the procedure *)
    Error.Count (nErrs0, nWarns);
    Expr.TypeCheck (p.proc, cs);
    Resolve (p);
    Expr.TypeCheck (p.proc, cs);
    Error.Count (nErrs1, nWarns);
    IF (p.methods = NIL) AND (nErrs0 = nErrs1) THEN
      Error.Msg ("attempting to call a non-procedure" & ProcName (p));
      p.type := ErrType.T;
    END;

    (* check its args *)
    keywords := (p.methods = NIL) OR (p.methods.keywords);
    FOR i := 0 TO LAST (p.args^) DO
      arg := p.args[i];
      Expr.TypeCheck (arg, cs);

      IF NOT Host.method_ok AND QualifyExpr.IsExplicitMethod (arg, tmp) THEN
        (* args cannot be type.method *)
        Error.Msg ("Explicitly named method cannot be passed as a parameter");
      END;
      IF (Expr.TypeOf (arg) = ErrType.T) THEN
        p.type := ErrType.T;
      ELSIF (NOT keywords) AND KeywordExpr.Is (arg) THEN
        Error.Msg ("keyword parameters not allowed on builtin operations" &
                   ProcName (p));
      END;
    END;

    (* finally, do the procedure specific checking *)
    IF (p.type # ErrType.T) AND (p.methods # NIL) THEN
      VAR
        objtype, argtype: Type.T;
        arg0: Expr.T;
        info: Type.Info;
      BEGIN
        IF QualifyExpr.IsExplicitMethod (p.proc, objtype) AND
          NUMBER (p.args^) > 0 THEN
          (* the first argument must be a subtype of objtype,
             and also must be fully revealed *)
          arg0 := p.args[0];
          argtype := Expr.TypeOf (arg0);
          EVAL Type.CheckInfo (argtype, info);

          (* SPIN - check calls that use the Type.Method syntax *)
          IF NOT Host.method_ok THEN
            IF NOT Type.IsSubtype (argtype, objtype) THEN
              Error.Msg ("Explicitly named method can only be called on a subtype object");
            ELSIF info.class = Type.Class.Opaque THEN
              (* Apparently Revelation.LookUp only returns a full revelation
                 or NIL if there is no full revelation. Use LookUpAll to get
                 at the partial revelations. *)
              IF Revelation.LookUp(argtype) = NIL THEN
                Error.Msg ("First argument to explicitly named method must have a fully revealed type");
              END;
            END;
          END;

        END;
      END;

      (* SPIN *)
      IF NOT p.is_bounded THEN
        CASE Ephemeral.Check () OF
        | Ephemeral.Return.Error =>
          Error.Msg ("non-EPHEMERAL procedure call inside EPHEMERAL procedure");
        | Ephemeral.Return.Warn =>
          Error.Warn (2, "non-EPHEMERAL procedure call inside UNSAFE EPHEMERAL procedure");
        | Ephemeral.Return.Ok =>
        END;
      END;
      
      IF NOT p.is_functional AND Functional.Check () THEN
        Error.Msg ("non-FUNCTIONAL procedure call inside FUNCTIONAL procedure");
      END;
      IF p.is_inline THEN
        cs.calls_inline := TRUE;
      END;

      FixArgs (p);
      p.methods.checker (p, cs);
    END;

    (* check the exceptions *)
    ESet.NoteExceptions (cs, ProcType.Raises (p.proc_type));
  END Check;

PROCEDURE FixArgs (p: T) =
  VAR z: Expr.List;
  BEGIN
    IF (NUMBER (p.args^) < p.methods.minArgs) THEN
      Error.Msg ("too few arguments" & ProcName (p));
      z := NEW (Expr.List, p.methods.minArgs);
      FOR i := 0 TO LAST (p.args^) DO z[i] := p.args[i] END;
      p.args := z;
    ELSIF (NUMBER (p.args^) > p.methods.maxArgs) THEN
      Error.Msg ("too many arguments" & ProcName (p));
      z := NEW (Expr.List, p.methods.maxArgs);
      FOR i := 0 TO p.methods.maxArgs - 1 DO z[i] := p.args[i] END;
      p.args := z;
    END;
  END FixArgs;

PROCEDURE ProcName (p: T): TEXT =
  VAR v: Value.T;
  BEGIN
    IF (p.proc # NIL) AND UserProc.IsProcedureLiteral (p.proc, v) THEN
      RETURN ": " & Value.GlobalName (v, dots := TRUE, with_module := TRUE);
    ELSE
      RETURN "";
    END;
  END ProcName;

PROCEDURE NeedsAddress (p: T) =
  BEGIN
    IF (p.methods # NIL) THEN
      p.methods.need_addr (p);
    END;
  END NeedsAddress;

PROCEDURE Prep (p: T) =
  BEGIN
    p.methods.prep (p);
  END Prep;

PROCEDURE Compile (p: T) =
  BEGIN
    p.methods.compiler (p);
  END Compile;

PROCEDURE PrepLV (p: T) =
  BEGIN
    p.methods.prepLV (p);
  END PrepLV;

PROCEDURE CompileLV (p: T) =
  BEGIN
    p.methods.compilerLV (p);
  END CompileLV;

PROCEDURE PrepBR (p: T;  true, false: CG.Label;  freq: CG.Frequency) =
  BEGIN
    p.methods.prepBR (p, true, false, freq);
  END PrepBR;

PROCEDURE CompileBR (p: T;  true, false: CG.Label;  freq: CG.Frequency) =
  BEGIN
    p.methods.compilerBR (p, true, false, freq);
  END CompileBR;

PROCEDURE NoteWrites (p: T) =
  BEGIN
    IF p.methods # NIL THEN
      p.methods.noteWriter (p);
    END;
  END NoteWrites;

PROCEDURE Fold (p: T): Expr.T =
  BEGIN
    Resolve (p);
    IF (p.methods = NIL) THEN RETURN NIL END;
    RETURN p.methods.evaluator (p);
  END Fold;

PROCEDURE GetBounds (p: T;  VAR min, max: Target.Int) =
  BEGIN
    (* Check for the builtin Word procedures, which we know something
       about. If it is not one of them, then call Type.GetBounds which
       is the most pessimistic answer. *)
    IF p.methods # NIL THEN
      p.methods.getBounds (p, min, max);
    END;
  END GetBounds;

PROCEDURE IsDesignator (p: T): BOOLEAN =
  BEGIN
    Resolve (p);
    IF p.methods = NIL THEN RETURN FALSE END;
    RETURN p.methods.isDesignator (p);
  END IsDesignator;

PROCEDURE IsWritable (p: T): BOOLEAN =
  BEGIN
    Resolve (p);
    IF p.methods = NIL THEN RETURN FALSE END;
    RETURN p.methods.isWritable (p);
  END IsWritable;

PROCEDURE IsGlobal (p: T): M3.Global =
  BEGIN
    (* 
     * LOOPHOLE, VIEW, and SUBARRAY carry the "globalness" of
     *   their first arguments.
     *)
    IF p.methods = Loophole.Z OR
       p.methods = View.Z OR
       p.methods = Subarray.Z THEN
      RETURN Expr.IsGlobal (p.args[0]);
    ELSE
      RETURN M3.Global.No;
    END;
  END IsGlobal;

BEGIN
END CallExpr.

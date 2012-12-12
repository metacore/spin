
(*
 * HISTORY
 * 06-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for VIEW between representation-equivalent types
 *
 * 03-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *	based on PackedType.m3
 *
 *)

MODULE AlignedType;

IMPORT M3, Word, Type, TypeRep, Error, Expr, Target, TInt;
IMPORT Token, IntegerExpr, Scanner, Fmt;
IMPORT CG, M3Buf;

CONST
  NO_SIZE = -1;

TYPE
  P = Type.T OBJECT
        alignE      : Expr.T;
        newAlign   : INTEGER;
        baseType   : Type.T;
      OVERRIDES
        check      := Check;
        check_align:= CheckAlign;
        isEqual    := EqualChk;
        isEquiv    := EquivChk;
        isSubtype  := Subtyper;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := GenInit;
        mapper     := GenMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
        to_text    := ToText;
      END;

PROCEDURE Parse (): Type.T =
  TYPE TK = Token.T;
  VAR p: P := New (NO_SIZE, NIL);
  BEGIN
    Scanner.Match (TK.tALIGNED);
    p.alignE := Expr.Parse ();
    Scanner.Match (TK.tFOR);
    p.baseType := Type.Parse ();
    RETURN p;
  END Parse;

PROCEDURE New (size: INTEGER;  base: Type.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Aligned);
    p.alignE   := NIL;
    p.newAlign := size;
    p.baseType := base;
    RETURN p;
  END New;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Aligned) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE GetAlign (p: P): INTEGER =
  VAR new_al: INTEGER;  new_align: Target.Int;  e: Expr.T;
  BEGIN
    IF (p.newAlign = NO_SIZE) AND (p.alignE # NIL) THEN
      e := Expr.ConstValue (p.alignE);
      IF (e = NIL) OR NOT IntegerExpr.Split (e, new_align)
        OR NOT TInt.ToInt (new_align, new_al)
        THEN Error.Msg ("ALIGNED FOR size must be a constant integer");
        ELSE p.alignE := e;  p.newAlign := new_al;
      END;
    END;
    RETURN p.newAlign;
  END GetAlign;

PROCEDURE Split (t: Type.T;  VAR align: INTEGER;  VAR base: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    align := GetAlign (p);
    base := p.baseType;
    RETURN TRUE;
  END Split;

PROCEDURE Base (t: Type.T): Type.T =
  VAR p: P := t;
  BEGIN
    RETURN p.baseType;
  END Base;

PROCEDURE Check (p: P) =
  VAR
    new_al, old_al: INTEGER;
    cs := M3.OuterCheckState;
    info: Type.Info;
  BEGIN
    p.baseType := Type.CheckInfo (p.baseType, info);
    old_al := info.alignment;
    new_al  := old_al;

    IF (p.alignE # NIL) THEN
      Expr.TypeCheck (p.alignE, cs);
      new_al := GetAlign (p);
      IF (new_al = NO_SIZE) THEN new_al := old_al; END;
    END;

    IF new_al MOD old_al # 0 THEN
      Error.Int (old_al, "ALIGNED FOR value must be a multiple of");
    ELSIF new_al < old_al THEN
      Error.Int (old_al, "ALIGNED FOR value too small, must be at least");
    END;

    IF info.min_size < new_al THEN
      p.info.min_size := new_al;
    ELSE
      p.info.min_size := info.min_size;
    END;

    IF info.size < new_al THEN
      IF info.size = -1 THEN
        Error.Msg ("ALIGNED FOR cannot apply to an open array");
      ELSE
        Error.Int (info.size, "ALIGNED FOR value cannot be larger than");
      END;
    END;

    (* push the alignment downwards *)
    Type.SetAlign (p.baseType, new_al);

    p.info.size      := info.size;
    p.info.alignment := new_al;
    p.info.cg_type   := info.cg_type;
    p.info.class     := Type.Class.Aligned;
    p.info.isTraced  := info.isTraced;
    p.info.isEmpty   := info.isEmpty;
    p.info.isSolid   := info.isSolid;
    p.info.hash      := Word.Plus (Word.Times (67, info.hash), new_al);
  END Check;

PROCEDURE CheckAlign (p: P;  offset: INTEGER): BOOLEAN =
  VAR z0: INTEGER;  info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (p.baseType, info);
    IF Type.IsStructured (p.baseType) THEN
      (* the scalar crossing can't be any worse than in the full structure *)
      RETURN Type.IsAlignedOk (p.baseType, offset);
    ELSE
      z0 := offset DIV Target.Integer.align * Target.Integer.align;
      RETURN (offset + info.size) <= (z0 + Target.Integer.size);
    END;
  END CheckAlign;

PROCEDURE Compiler (p: P) =
  BEGIN
    Type.Compile (p.baseType);
    CG.Declare_aligned (Type.GlobalUID (p), p.info.size, p.info.alignment,
                        Type.GlobalUID (p.baseType));
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN GetAlign (a) = GetAlign (b)
       AND Type.IsEqual (a.baseType, b.baseType, x);
  END EqualChk;

PROCEDURE EquivChk (a: P;  t: Type.T; x: Type.Assumption; canTruncate: BOOLEAN)
  : BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN GetAlign (a) = GetAlign (b)
       AND Type.IsEquiv (a.baseType, b.baseType, x, canTruncate);
  END EquivChk;

PROCEDURE Subtyper (a: P;  b: Type.T): BOOLEAN =
  BEGIN
    RETURN Type.IsEqual (b, a.baseType, NIL);
  END Subtyper;

PROCEDURE InitCoster (p: P;  zeroed: BOOLEAN): INTEGER =
  BEGIN
    RETURN Type.InitCost (p.baseType, zeroed);
  END InitCoster;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  BEGIN
    Type.InitValue (p.baseType, zeroed);
  END GenInit;

PROCEDURE GenMap (p: P;  offset, size: INTEGER;  refs_only: BOOLEAN) =
  BEGIN
    Type.GenMap (p.baseType, offset, size, refs_only);
  END GenMap;

PROCEDURE GenDesc (<* UNUSED *> p: P) =
  BEGIN
    (* this is never used in SPIN *)
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    M3Buf.PutText (x.buf, "ALIGNED-FOR ");
    M3Buf.PutInt  (x.buf, p.newAlign);
    x.n_nodes  := 1;
    x.nodes[0] := p.baseType;
  END FPrinter;

PROCEDURE ToText (p: P): TEXT =
  BEGIN
    RETURN ("ALIGNED " & Fmt.Int(p.newAlign) & " FOR " & p.baseType.to_text());
  END ToText;

BEGIN
END AlignedType.

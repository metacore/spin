(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CastExpr.m3                                           *)
(* Last Modified On Fri Feb 24 16:40:53 PST 1995 By kalsow     *)
(*      Modified On Sun Dec 23 08:07:22 1990 By muller         *)

(*
 * HISTORY
 * 18-Apr-97  Wilson Hsieh (whsieh) at the University of Washington
 *	only check alignment on architectures that care
 *
 * 09-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	globalness is more precise
 *
 * 06-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for VIEW between representation-equivalent types
 *
 * 19-Aug-96  Wilson Hsieh (whsieh) at the University of Washington
 *	VIEW now generates open arrays
 *
 * 28-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	add Split
 *
 * 21-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	remove ViewAlignmentOff
 *
 * 16-Feb-96  Wilson Hsieh (whsieh) at the University of Washington
 *	turn off alignment check if compiler flag set
 *
 * 08-Feb-96  Wilson Hsieh (whsieh) at the University of Washington
 *	Fixed GenerateAlignmentCheck so that alignment in bits is
 *	 properly converted to a mask in terms of bytes
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *    Computed mask improperly in GenerateAlignmentCheck. 
 *
 * 06-Feb-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added code to check alignment of VIEW
 *      turned off warning message about VIEW alignment
 *
 * 26-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	cleaned up code to generate size check
 *
 * 19-Dec-95  Wilson Hsieh (whsieh) at the University of Washington
 *	added UNUSED pragma to eliminate warning
 *
 * 15-Nov-95  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed line number in CastExpr.New
 *
 * 31-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	most of the code copied from CastExpr.m3
 *
 *)

MODULE ViewExpr;

IMPORT M3Buf, CG, Expr, ExprRep, Type, Error, OpenArrayType;
IMPORT M3, Target, TInt, Word, M3RT;
IMPORT Scanner, Host, Fmt;

TYPE
  Kind = {
    Noop,    (* code generator cannot tell the difference *)

    D_to_S,  (* designator -> structure *)
    D_to_V,  (* designator -> value *)
    D_to_A,  (* designator -> open array *)

    A_to_S,  (* open array -> structure *)
    A_to_V,  (* open array -> value *)
    A_to_A   (* open array -> open array *)
  };

TYPE
  P = Expr.T BRANDED "ViewExpr" OBJECT
        kind    : Kind;
        expr    : Expr.T;
	tipe    : Type.T;
        tmp     : CG.Val;
        tmp_cnt : INTEGER;
        align_in : INTEGER;
        align_out: INTEGER;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := PrepLV;
        compileLV    := CompileLV;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := Bounder;
        isWritable   := IsWritable;
        isDesignator := IsDesignator;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := GenFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
        global       := IsGlobal;
      END;

PROCEDURE New (a: Expr.T;  t: Type.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.origin := Scanner.offset;
    p.expr   := a;
    p.tipe   := t;
    p.type   := t;
    p.tmp_cnt:= 0;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR
    src, dest, elt_in, elt_out: Type.T;  sz0, sz1: INTEGER;
    desig_in, struct_in, struct_out: BOOLEAN;
    align_in, align_out: INTEGER;
    dest_info, src_info, elt_in_info, elt_out_info: Type.Info;
    open_in, writable_in, open_out: BOOLEAN;
    err := FALSE;
  BEGIN

    Expr.TypeCheck (p.expr, cs);
    p.tipe := Type.CheckInfo (p.tipe, dest_info);

    src        := Type.CheckInfo (Expr.TypeOf (p.expr), src_info);
    dest       := p.tipe;
    desig_in   := Expr.IsDesignator (p.expr);
    struct_in  := Type.IsStructured (src);
    struct_out := Type.IsStructured (dest);
    open_in    := OpenArrayType.Split (src, elt_in);
    open_out   := OpenArrayType.Split (dest, elt_out);
    elt_in     := Type.CheckInfo (elt_in, elt_in_info);
    elt_out    := Type.CheckInfo (elt_out, elt_out_info);
    writable_in := Expr.IsWritable (p.expr);

    IF NOT desig_in THEN
      Error.Msg ("VIEW: source must be a designator");
      err := TRUE;
    END;

    IF NOT Type.IsEquiv (src, p.tipe, NIL, TRUE) THEN
      IF writable_in THEN
        Error.Msg ("VIEW: source is writable, and types are not representation-equivalent");
        err := TRUE;
      ELSIF NOT Type.RepresentationComplete (p.tipe) THEN
        Error.Msg ("VIEW: source is readonly, types are not representation-equivalent, and destination type is not representation-complete");
        err := TRUE;
      END;
    END;

    (* done if there was an error *)
    IF err THEN RETURN; END;

    (* check to see that the destination type is not too big *)
    sz0 := src_info.size;
    sz1 := dest_info.size;
    IF sz0 # -1 AND sz1 > sz0 THEN
      Error.Msg ("VIEW: expression's size smaller than type's");
    END;

    (* classify the type of VIEW operation *)
    IF open_in THEN
      (* alignment of input is actually that of array elements *)
      align_in := elt_in_info.alignment;

      IF open_out THEN
        IF elt_in_info.size = elt_out_info.size AND
          elt_in_info.alignment MOD elt_out_info.alignment = 0 THEN
          Error.Warn (0, "VIEW Noop");
          p.kind := Kind.Noop;
        ELSE
          Error.Warn (0, "VIEW A TO A");
          p.kind := Kind.A_to_A;
        END;
        align_out  := elt_out_info.alignment;
      ELSIF struct_out THEN
        Error.Warn (0, "VIEW A to S");
        align_out  := dest_info.alignment;
        p.kind := Kind.A_to_S;
      ELSE
        Error.Warn (0, "VIEW A to V");
        align_out  := dest_info.alignment;
        p.kind := Kind.A_to_V;
      END;
    ELSE
      (* alignment of input is that of type itself *)
      align_in := src_info.alignment;

      IF open_out THEN
        Error.Warn (0, "VIEW D to A");
        align_out  := elt_out_info.alignment;
        p.kind := Kind.D_to_A;
      ELSIF (src_info.cg_type = dest_info.cg_type) THEN
        Error.Warn (0, "VIEW Noop");
        align_out  := dest_info.alignment;
        p.kind := Kind.Noop;
      ELSIF struct_out THEN
        Error.Warn (0, "VIEW D to S");
        align_out  := dest_info.alignment;
        p.kind := Kind.D_to_S;
      ELSE
        Error.Warn (0, "VIEW D to V");
        align_out  := dest_info.alignment;
        p.kind := Kind.D_to_V;
      END;
    END;

    (* save the alignments for later use *)
    p.align_in := align_in;
    p.align_out := align_out;

    (* we are going to take the address of this value *)
    Expr.NeedsAddress (p.expr);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN Type.IsEqual (a.tipe, b.tipe, x)
                 AND Expr.IsEqual (a.expr, b.expr, x);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  BEGIN
    CASE p.kind OF
    | Kind.Noop,
      Kind.D_to_S,
      Kind.D_to_V,
      Kind.D_to_A =>
        Expr.NeedsAddress (p.expr);
    | Kind.A_to_S,
      Kind.A_to_V,
      Kind.A_to_A =>
      (* skip *)
    END;
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  VAR
    e  := p.expr;
    u  := Expr.TypeOf (e);
    t  := p.tipe;
    t_elt, u_elt : Type.T;
    sz, t_align, u_align: INTEGER;
    t_cg, u_cg: CG.Type;
    u_info, t_info, t_elt_info, u_elt_info: Type.Info;
    header: CG.Var;
    tmp: CG.Val;
    size: Target.Int;
  BEGIN
    IF (p.tmp_cnt > 0) THEN  INC (p.tmp_cnt);  RETURN;  END;
    u := Type.CheckInfo (u, u_info);
    t := Type.CheckInfo (t, t_info);
    t_cg := t_info.cg_type;  t_align := t_info.alignment;
    u_cg := u_info.cg_type;  u_align := u_info.alignment;
    sz := u_info.size;
    Type.Compile (t);
    Type.Compile (u);

    CASE p.kind OF
    | Kind.Noop =>
        Expr.Prep (e);
    | Kind.D_to_S, Kind.D_to_V =>
        Expr.PrepLValue (e);
    | Kind.A_to_S, Kind.A_to_V =>
        Expr.Prep (e);
    | Kind.D_to_A =>
        header := OpenArrayType.DeclareTemp (t);
        Expr.PrepLValue (e);
        Expr.CompileAddress (e);

        (* store the element pointer *)
        GenerateAlignmentCheck (p);
        CG.Store_addr (header, M3RT.OA_elt_ptr);
        
        (* store the size *)
        EVAL OpenArrayType.Split (t, t_elt);
        EVAL Type.CheckInfo (t_elt, t_elt_info);
        EVAL TInt.FromInt (u_info.size DIV t_elt_info.size, size);
        CG.Load_integer (size);
        CG.Store_int (header, M3RT.OA_size_0);

        (* leave the new subarray as the result. *)
        CG.Load_addr_of_temp (header, 0, Target.Address.align);
        p.tmp := CG.Pop ();

    | Kind.A_to_A =>
        header := OpenArrayType.DeclareTemp (t);
        Expr.PrepLValue (e);
        Expr.CompileAddress (e);
        tmp := CG.Pop ();

        (* store the element pointer *)
        CG.Push (tmp);
        GenerateAlignmentCheck (p);
        CG.Open_elt_ptr (p.align_in);
        CG.Store_addr (header, M3RT.OA_elt_ptr);

        (* store the size *)
        EVAL OpenArrayType.Split (t, t_elt);
        EVAL Type.CheckInfo (t_elt, t_elt_info);
        EVAL OpenArrayType.Split (u, u_elt);
        EVAL Type.CheckInfo (u_elt, u_elt_info);

        CG.Push (tmp);
        CG.Free (tmp);
        CG.Open_size (0);
        EVAL TInt.FromInt (u_elt_info.size, size);
        CG.Load_integer (size);
        CG.Multiply (CG.Type.Word);

        EVAL TInt.FromInt (t_elt_info.size, size);
        CG.Load_integer (size);
        CG.Div (CG.Type.Word, CG.Sign.Positive, CG.Sign.Positive);
        CG.Store_int (header, M3RT.OA_size_0);

        (* leave the new subarray as the result. *)
        CG.Load_addr_of_temp (header, 0, Target.Address.align);
        p.tmp := CG.Pop ();
        
    ELSE
      <* ASSERT FALSE *>
    END;
  END Prep;

PROCEDURE Compile (p: P) =
  VAR
    e  := p.expr;
    u  := Expr.TypeOf (e);
    t  := p.tipe;
    sz, t_align, u_align: INTEGER;
    t_cg, u_cg: CG.Type;
    u_info, t_info: Type.Info;
  BEGIN
    u := Type.CheckInfo (u, u_info);
    t := Type.CheckInfo (t, t_info);
    t_cg := t_info.cg_type;  t_align := t_info.alignment;
    u_cg := u_info.cg_type;  u_align := u_info.alignment;
    sz := u_info.size;
    Type.Compile (t);
    Type.Compile (u);

    CASE p.kind OF
    | Kind.Noop =>
        Expr.Compile (e);
        GenerateAlignmentCheck (p);
        CG.Boost_alignment (t_align);
    | Kind.D_to_S =>
        Expr.CompileAddress (e);
        GenerateAlignmentCheck (p);
        CG.Boost_alignment (t_align);
    | Kind.D_to_V =>
        Expr.CompileAddress (e);
        GenerateAlignmentCheck (p);
        CG.Boost_alignment (t_align);
        CG.Load_indirect (t_cg, 0, t_info.size);
    | Kind.A_to_S =>
        Expr.CompileAddress (e);
        GenerateAlignmentCheck (p);
        GenerateSizeCheck (t_info, u);
        CG.Open_elt_ptr (t_align);
    | Kind.A_to_V =>
        Expr.CompileAddress (e);
        GenerateAlignmentCheck (p);
        GenerateSizeCheck (t_info, u);
        CG.Open_elt_ptr (t_align);
        CG.Load_indirect (t_cg, 0, t_info.size);

    | Kind.D_to_A,
      Kind.A_to_A =>
      (* work done in Prep *)
      CG.Push (p.tmp);
      CG.Boost_alignment (Target.Address.align);
      CG.Free (p.tmp);
      p.tmp := NIL;

    ELSE
      <* ASSERT FALSE *>
    END;
  END Compile;

PROCEDURE PrepLV (p: P) =
  VAR
    e  := p.expr;
    u  := Expr.TypeOf (e);
    t  := p.tipe;
    t_elt, u_elt : Type.T;
    sz, t_align, u_align: INTEGER;
    t_cg, u_cg: CG.Type;
    u_info, t_info, t_elt_info, u_elt_info: Type.Info;
    header: CG.Var;
    tmp: CG.Val;
    size: Target.Int;
  BEGIN
    IF (p.tmp_cnt > 0) THEN  INC (p.tmp_cnt);  RETURN;  END;
    u := Type.CheckInfo (u, u_info);
    t := Type.CheckInfo (t, t_info);
    t_align := t_info.alignment;
    u_align := u_info.alignment;
    t_cg := t_info.cg_type;
    u_cg := u_info.cg_type;
    sz := u_info.size;
    Type.Compile (t);
    Type.Compile (u);

    CASE p.kind OF
    | Kind.Noop,
      Kind.D_to_S,
      Kind.D_to_V =>
        Expr.PrepLValue (p.expr);
    | Kind.A_to_S,
      Kind.A_to_V =>
        Expr.PrepLValue (p.expr);
    | Kind.D_to_A =>
        header := OpenArrayType.DeclareTemp (t);
        Expr.PrepLValue (e);
        Expr.CompileAddress (e);

        (* store the element pointer *)
        GenerateAlignmentCheck (p);
        CG.Store_addr (header, M3RT.OA_elt_ptr);
        
        (* store the size *)
        EVAL OpenArrayType.Split (t, t_elt);
        EVAL Type.CheckInfo (t_elt, t_elt_info);
        EVAL TInt.FromInt (u_info.size DIV t_elt_info.size, size);
        CG.Load_integer (size);
        CG.Store_int (header, M3RT.OA_size_0);

        (* leave the new subarray as the result. *)
        CG.Load_addr_of_temp (header, 0, Target.Address.align);
        p.tmp := CG.Pop ();

    | Kind.A_to_A =>
        header := OpenArrayType.DeclareTemp (t);
        Expr.PrepLValue (e);
        Expr.CompileAddress (e);
        tmp := CG.Pop ();

        (* store the element pointer *)
        CG.Push (tmp);
        GenerateAlignmentCheck (p);
        CG.Open_elt_ptr (p.align_in);
        CG.Store_addr (header, M3RT.OA_elt_ptr);

        (* store the size *)
        EVAL OpenArrayType.Split (t, t_elt);
        EVAL Type.CheckInfo (t_elt, t_elt_info);
        EVAL OpenArrayType.Split (u, u_elt);
        EVAL Type.CheckInfo (u_elt, u_elt_info);

        CG.Push (tmp);
        CG.Free (tmp);
        CG.Open_size (0);
        EVAL TInt.FromInt (u_elt_info.size, size);
        CG.Load_integer (size);
        CG.Multiply (CG.Type.Word);

        EVAL TInt.FromInt (t_elt_info.size, size);
        CG.Load_integer (size);
        CG.Div (CG.Type.Word, CG.Sign.Positive, CG.Sign.Positive);
        CG.Store_int (header, M3RT.OA_size_0);

        (* leave the new subarray as the result. *)
        CG.Load_addr_of_temp (header, 0, Target.Address.align);
        p.tmp := CG.Pop ();
    ELSE
      <* ASSERT FALSE *>
    END;
  END PrepLV;

PROCEDURE CompileLV (p: P) =
  VAR
    e  := p.expr;
    u  := Expr.TypeOf (e);
    t  := p.tipe;
    sz, t_align, u_align: INTEGER;
    u_cg: CG.Type;
    u_info, t_info: Type.Info;
  BEGIN
    u := Type.CheckInfo (u, u_info);
    t := Type.CheckInfo (t, t_info);
    t_align := t_info.alignment;
    u_align := u_info.alignment;
    u_cg := u_info.cg_type;
    sz := u_info.size;
    Type.Compile (t);
    Type.Compile (u);

    CASE p.kind OF
    | Kind.Noop =>
        Expr.CompileLValue (p.expr);
        GenerateAlignmentCheck (p);
        CG.Boost_alignment (t_align);
    | Kind.D_to_S,
      Kind.D_to_V =>
        Expr.CompileLValue (p.expr);
        GenerateAlignmentCheck (p);
        CG.Boost_alignment (t_align);
    | Kind.A_to_S,
      Kind.A_to_V =>
        Expr.CompileAddress (e);
        GenerateAlignmentCheck (p);
        GenerateSizeCheck (t_info, u);
        CG.Open_elt_ptr (t_align);

    | Kind.D_to_A,
      Kind.A_to_A =>
      (* work done in PrepLV *)
      CG.Push (p.tmp);
      CG.Boost_alignment (Target.Address.align);
      CG.Free (p.tmp);
      p.tmp := NIL;

    ELSE
      <* ASSERT FALSE *>
    END;
  END CompileLV;

PROCEDURE Fold (p: P): Expr.T =
  VAR e: Expr.T;
  BEGIN
    e := Expr.ConstValue (p.expr);
    IF (e = NIL) THEN RETURN NIL END;
    p.expr := e;
    RETURN p;
  END Fold;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  VAR min1, max1: Target.Int;
  BEGIN
    Expr.GetBounds (p.expr, min, max);
    EVAL Type.GetBounds (p.tipe, min1, max1);
    IF TInt.LT (min, min1) THEN min := min1 END;
    IF TInt.LT (max1, max) THEN max := max1 END;
  END Bounder;

PROCEDURE IsDesignator (<*UNUSED*> p: P): BOOLEAN =
  BEGIN
    (* a VIEW is always a designator
       the first argument to VIEW must always be a designator
     *)
    RETURN TRUE;
  END IsDesignator;

PROCEDURE IsWritable (p: P): BOOLEAN =
  BEGIN
    RETURN Expr.IsWritable (p.expr);
  END IsWritable;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    Expr.GenFPLiteral (p.expr, buf);
  END GenFPLiteral;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Expr.NoteWrite (p.expr);
  END NoteWrites;


PROCEDURE GenerateAlignmentCheck (p: P) =
  (*
    when called, the address of the first argument to VIEW must
    be on the code generation stack
  *)
  VAR
    mask: Word.T;
    mask_rep: Target.Int;
    check: BOOLEAN;

    tmp: CG.Val;
  BEGIN
    (* if the architecture supports unaligned accesses, no problem *)
    IF Target.Unaligned_memory_reference_ok THEN RETURN; END;

    (* if the alignment of the input is subsumed by that of
       the output, no problem *)
    IF (p.align_in MOD p.align_out) = 0 THEN RETURN; END;

    (* otherwise, we have to check the alignment *)
    (* assume that size of word = size of address *)
    mask := (p.align_out DIV Target.Byte) - 1;
    check := TInt.FromInt (mask, mask_rep);
    <* ASSERT check *>

    (* copy the address *)
    tmp := CG.Pop ();
    CG.Push (tmp);

    CASE p.kind OF
    | Kind.A_to_S,
      Kind.A_to_V,
      Kind.A_to_A =>
        CG.Open_elt_ptr (p.align_in);
    ELSE
      (* skip *)
    END;

    (* put the args on the stack and check them *)
    IF Host.verbose_checks THEN
      Error.Warn(1, "Emitting VIEW alignment check against " & Fmt.Int(mask) &
        "\n");
    END;
    CG.Check_align (mask_rep);

    (* put the address back on the stack *)
    CG.Push (tmp);
    CG.Free (tmp);
  END GenerateAlignmentCheck;

PROCEDURE GenerateSizeCheck (t_info: Type.Info; u: Type.T) =
  BEGIN
    (* generate the bounds check *)
    IF Host.doRangeChk THEN
      VAR size: INTEGER;
          size_rep: Target.Int;
          check: BOOLEAN;
          t1: CG.Val;
          elt_size: INTEGER;
      BEGIN
        (* make a copy of the array *)
        t1 := CG.Pop ();

        (* compute the size in bits *)
        size := t_info.size;
        check := TInt.FromInt (size, size_rep);
        <* ASSERT check *>

        (* range check the size *)
        CG.Push (t1);
        CG.Open_size (0);
        elt_size := OpenArrayType.EltPack (u);
        <* ASSERT elt_size > 0 *>
        CG.Load_intt (elt_size);
        CG.Multiply (Target.CGType.Word);
        IF Host.verbose_checks THEN
          Error.Warn(1, "Emitting VIEW size check against " & Fmt.Int(size) &
            "\n");
        END;
        CG.Check_size (size_rep);

        (* put the array back on the stack *)
        CG.Push (t1);
        CG.Free (t1);
      END;
    END;
  END GenerateSizeCheck;

PROCEDURE IsGlobal (p: P) : M3.Global =
  BEGIN
    RETURN Expr.IsGlobal (p.expr);
  END IsGlobal;

BEGIN
END ViewExpr.

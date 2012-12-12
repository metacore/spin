(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ArrayType.m3                                          *)
(* Last modified on Tue Dec  6 09:23:51 PST 1994 by kalsow     *)
(*      modified on Sun Feb 24 04:39:01 1991 by muller         *)

(*
 * HISTORY
 * 09-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fix reference counting code to check if array size=0
 *
 * 02-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	reference counting code
 *
 * 06-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for VIEW between representation-equivalent types
 *
 * 24-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed Subtyper to use TInt.EQ instead of # to compare TInts
 *
 * 07-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	removed call to Type.Base from within Reduce (which I added),
 *	 because it screwed up typing
 *
 * 06-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added SetAlign method to set alignment from a containing
 *	 AlignedType
 *
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added ToText, which overrides the to_text inherited 
 *	method
 *
 *)

MODULE ArrayType;

IMPORT M3, CG, Type, TypeRep, Error, Token, OpenArrayType;
IMPORT Word, Target, TInt, RecordType, TipeMap, TipeDesc, ErrType;
FROM Scanner IMPORT Match, GetToken, cur;

(* for ref counting *)
IMPORT Procedure, Runtime, Addr, Int, M3ID, RefType, ObjectType, OpaqueType, Value;

CONST
  MAXSIZE = LAST (INTEGER);

TYPE
  P = Type.T BRANDED "ArrayType.P" OBJECT
        index      : Type.T;
        element    : Type.T;
        alignment  : INTEGER;
        n_elts     : INTEGER;
        elt_align  : INTEGER;
        elt_pack   : INTEGER;
        total_size : INTEGER;
        openCousin : Type.T;  (* == ARRAY OF element *)
        packed     : BOOLEAN;
      OVERRIDES
        check      := Check;
        check_align:= CheckAlign;
        set_align  := SetAlign;
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
        genRC      := GenRC;
      END;

PROCEDURE Parse (): Type.T =
  TYPE TK = Token.T;
  VAR p, p0: P;
  BEGIN
    Match (TK.tARRAY);
    IF (cur.token IN Token.TypeStart) THEN
      p0 := New (NIL, NIL);  p := p0;
      LOOP
        p.index := Type.Parse ();
        IF (cur.token # TK.tCOMMA) THEN EXIT END;
        GetToken (); (* , *)
	p.element := New (NIL, NIL);
        p := p.element;
      END;
      Match (TK.tOF);
      p.element := Type.Parse ();
      RETURN p0;
    ELSE
      (* must be an open array *)
      Match (TK.tOF);
      RETURN OpenArrayType.New (Type.Parse ());
    END;
  END Parse;

PROCEDURE New (index, element: Type.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Array);
    p.index      := index;
    p.element    := element;
    p.alignment  := 0;
    p.n_elts     := 0;
    p.total_size := 0;
    p.elt_align  := 0;
    p.elt_pack   := 0;
    p.openCousin := NIL;
    p.packed     := FALSE;
    RETURN p;
  END New;

PROCEDURE Split (t: Type.T;  VAR index, element: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      index := p.index;  element := p.element;
      RETURN TRUE;
    ELSIF OpenArrayType.Split (t, element) THEN
      index := NIL;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Split;

PROCEDURE EltPack (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      RETURN p.elt_pack;
    ELSIF OpenArrayType.Is (t) THEN
      RETURN OpenArrayType.EltPack (t);
    ELSE
      RETURN 0;
    END;
  END EltPack;

PROCEDURE EltAlign (t: Type.T): INTEGER =
  VAR p:= Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      RETURN p.elt_align;
    ELSIF OpenArrayType.Is (t) THEN
      RETURN OpenArrayType.EltAlign (t);
    ELSE
      RETURN Target.Byte;
    END;
  END EltAlign;

PROCEDURE OpenCousin (t: Type.T): Type.T =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN
      IF (p.openCousin = NIL) THEN
        p.openCousin := OpenArrayType.New (p.element);
      END;
      RETURN p.openCousin;
    ELSE
      RETURN t;
    END;
  END OpenCousin;

PROCEDURE IsBitAddressed (t: Type.T): BOOLEAN =
  VAR p:= Reduce (t);
  BEGIN
    RETURN (p # NIL) AND (p.packed);
  END IsBitAddressed;

PROCEDURE GenIndex (t: Type.T) =
  VAR p := Reduce (t);  index: CG.Val;
  BEGIN
    IF (p = NIL) THEN
      CG.Index_bytes (OpenArrayType.EltPack (t));
    ELSIF NOT p.packed THEN
      CG.Index_bytes (p.elt_pack);
    ELSE
      (* we have a packed array with non-byte-aligned elements... *)
      IF (p.elt_pack # 1) THEN
        (* compute the bit-offset of the indexed element *)
        CG.Load_intt (p.elt_pack);
        CG.Multiply (CG.Type.Int);
      END;
      IF (p.total_size <= p.alignment) THEN
        CG.Index_bits ();
      ELSE
        index := CG.Pop ();
        CG.Push (index);
        CG.Load_intt (p.alignment);
        CG.Div (CG.Type.Int, CG.Sign.Positive, CG.Sign.Positive);
        CG.Index_bytes (p.alignment);
        CG.Push (index);
        CG.Load_intt (p.alignment);
        CG.Mod (CG.Type.Int, CG.Sign.Positive, CG.Sign.Positive);
        CG.Index_bits ();
        CG.Free (index);
      END;
    END;
  END GenIndex;

PROCEDURE Check (p: P) =
  VAR align, full_size: INTEGER;  elt_info: Type.Info;
  BEGIN
    p.index := Type.Check (p.index);
    IF NOT Type.IsOrdinal (p.index) THEN
      Error.Msg ("array index type must be an ordinal type");
      p.index := ErrType.T;
    END;

    p.element := Type.CheckInfo (p.element, elt_info);
    IF (elt_info.class = Type.Class.OpenArray) THEN
      Error.Msg ("array element type cannot be an open array");
    END;

    IF NOT TInt.ToInt (Type.Number (p.index), p.n_elts) THEN
      Error.Msg ("SRC Modula-3 restriction: array has too many elements");
      p.n_elts := 1;
    END;

    align       := elt_info.alignment;
    p.elt_align := elt_info.alignment;
    p.elt_pack  := elt_info.size;
    IF (elt_info.class # Type.Class.Packed) THEN
      (* naturally aligned elements must be OK *)
      p.elt_pack  := (elt_info.size + align - 1) DIV align * align;
      p.alignment := elt_info.alignment;
      p.packed    := FALSE;
    ELSE
      (* find a packing that is allowed *)
      p.alignment := FindAlignment (p);
      p.packed := (p.elt_pack < Target.Byte)
               OR (p.elt_pack MOD p.alignment # 0);
    END;

    IF (p.n_elts > 0) AND (p.elt_pack > 0)
      AND (p.n_elts > MAXSIZE DIV p.elt_pack) THEN
      Error.Msg ("SRC Modula-3 restriction: array type too large");
      full_size := 0;
      p.total_size := 0;
    ELSE
      full_size := p.elt_pack * p.n_elts;
      p.total_size := RecordType.RoundUp (full_size, p.alignment);
    END;

    p.info.size      := p.total_size;
    p.info.min_size  := p.total_size;
    p.info.alignment := p.alignment;
    p.info.cg_type   := CG.Type.Addr;
    p.info.class     := Type.Class.Array;
    p.info.isTraced  := elt_info.isTraced;
    p.info.isEmpty   := elt_info.isEmpty;
    p.info.isSolid   := elt_info.isSolid AND (p.elt_pack <= elt_info.size)
                            AND (p.total_size <= full_size);
    p.info.hash      := Word.Plus (Word.Times (23, p.n_elts),
                              Word.Times (29, p.elt_pack));
  END Check;

PROCEDURE FindAlignment (p: P): INTEGER =
  VAR x: INTEGER;
  BEGIN
    FOR a := FIRST (Target.Alignments) TO LAST (Target.Alignments) DO
      x := Target.Alignments[a];
      IF (x >= p.elt_align) AND Type.IsAlignedOk (p, x) THEN
        RETURN x;
      END;
    END;
    Error.Msg ("SRC Modula-3 restriction: scalars in packed array elements cannot cross word boundaries");
    RETURN Target.Byte;
  END FindAlignment;

PROCEDURE CheckAlign (p: P;  offset: INTEGER): BOOLEAN =
  VAR x0 := offset MOD Target.Integer.size;  x := x0;
  BEGIN
    FOR i := 0 TO p.n_elts-1 DO
      IF NOT Type.IsAlignedOk (p.element, x) THEN RETURN FALSE END;
      x := (x + p.elt_pack) MOD Target.Integer.size;
      IF (x = x0) THEN EXIT END;
    END;
    RETURN TRUE;
  END CheckAlign;

PROCEDURE SetAlign (p: P; new_align: INTEGER) =
  BEGIN
    p.info.alignment := new_align;
  END SetAlign;

PROCEDURE Compiler (p: P) =
  VAR self, index, elt: INTEGER;
  BEGIN
    Type.Compile (p.index);
    Type.Compile (p.element);
    self  := Type.GlobalUID (p);
    index := Type.GlobalUID (p.index);
    elt   := Type.GlobalUID (p.element);
    CG.Declare_array (self, index, elt, p.total_size);
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN Type.IsEqual (a.element, b.element, x)
       AND Type.IsEqual (a.index, b.index, x);
  END EqualChk;

PROCEDURE EquivChk (a: P;  t: Type.T; x: Type.Assumption;
                    <* UNUSED *> canTruncate: BOOLEAN) : BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN Type.IsEqual (a.element, b.element, x)
       AND Type.IsEquiv (a.index, b.index, x, FALSE);
  END EquivChk;

PROCEDURE Subtyper (a: P;  tb: Type.T): BOOLEAN =
  VAR ta, eb: Type.T;  b: P;
  BEGIN
    ta := a;

    (* peel off the fixed dimensions of A and open dimensions of B *)
    LOOP
      a := Reduce (ta);
      IF (a = NIL) OR NOT OpenArrayType.Split (tb, eb) THEN EXIT END;
      ta := a.element;
      tb := eb;
    END;

    (* peel off the fixed dimensions as long as the sizes are equal *)
    LOOP
      a := Reduce (ta);  b := Reduce (tb);
      IF (a = NIL) OR (b = NIL) THEN EXIT END;
      IF (a.index # b.index) THEN
        IF NOT TInt.EQ (Type.Number (a.index), Type.Number (b.index)) THEN RETURN FALSE END;
      END;
      ta := a.element;
      tb := b.element;
    END;

    RETURN Type.IsEqual (ta, tb, NIL);
  END Subtyper;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Array) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE InitCoster (p: P; zeroed: BOOLEAN): INTEGER =
  VAR n, m, res: Target.Int;  x: INTEGER;
  BEGIN
    x := Type.InitCost (p.element, zeroed);
    IF NOT TInt.FromInt (x, m) THEN RETURN LAST (INTEGER) END;
    n := Type.Number (p.index);
    IF TInt.LT (n, TInt.Zero) THEN (*open array?*) RETURN 20 * x END;
    IF NOT TInt.Multiply (m, n, res) THEN RETURN LAST (INTEGER) END;
    IF NOT TInt.ToInt (res, x) THEN RETURN LAST (INTEGER) END;
    RETURN x;
  END InitCoster;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  VAR
    top   : CG.Label;
    cnt   : CG.Val;
    array := CG.Pop ();  (* capture the l-value of the array *)
  BEGIN
    (* put down a loop to initialize the additional elements *)
    CG.Load_integer (TInt.Zero);
    cnt := CG.Pop_temp ();
    top := CG.Next_label ();
    CG.Set_label (top);

    (* init ARRAY[cnt] *)
    CG.Push (array);
    CG.Push (cnt);
    GenIndex (p);
    Type.InitValue (p.element, zeroed);

    (* cnt := cnt + 1 *)
    CG.Push (cnt);
    CG.Load_integer (TInt.One);
    CG.Add (CG.Type.Int);
    CG.Store_temp (cnt);

    (* IF (cnt < NUMBER(ARRAY) GOTO TOP-OF-LOOP *)
    CG.Push (cnt);
    CG.Load_intt (p.n_elts);
    CG.If_lt (top, CG.Type.Int, CG.Likely);

    (* release the temps *)
    CG.Free (cnt);
    CG.Free (array);
  END GenInit;

PROCEDURE GenRC (p: P; definitelyGlobal: BOOLEAN;
                 <* UNUSED *> noOverlap := FALSE) =
  VAR
    lhs, rhs, test       : CG.Val;
    increment, rhs_start, lhs_start, lhs_end : CG.Var;
    proc_test            : Procedure.T;
    info                 : Type.Info;
    l_forward            : CG.Label;
    l_not, l_end, l_loop : CG.Label;
    ignore               : Value.T;
    ignore1              : Type.T;
  BEGIN
    CG.Comment (-1, "Generating reference counted array assignment code\n");

    rhs := CG.Pop (); (* capture the rhs array address *)
    lhs := CG.Pop (); (* capture the lhs array address *)

    (* empty array? *)
    IF p.n_elts = 0 THEN
      CG.Comment (-1,
                  "Done generating reference counted array assignment code: empty array!\n");
      RETURN;
    END;

    (* find info for refcount test *)
    proc_test := Runtime.LookUpProc (Runtime.Hook.IsShared);

    lhs_start := CG.Declare_local (M3ID.NoID, Target.Address.size,
                                   Target.Address.align, CG.Type.Addr,
                                   Type.GlobalUID (Addr.T), in_memory := FALSE,
                                   up_level := FALSE, f := CG.Always);

    rhs_start := CG.Declare_local (M3ID.NoID, Target.Address.size,
                                   Target.Address.align, CG.Type.Addr,
                                   Type.GlobalUID (Addr.T), in_memory := FALSE,
                                   up_level := FALSE, f := CG.Always);

    lhs_end := CG.Declare_local (M3ID.NoID, Target.Address.size,
                                 Target.Address.align, CG.Type.Addr,
                                 Type.GlobalUID (Addr.T), in_memory := FALSE,
                                 up_level := FALSE, f := CG.Always);

    increment := CG.Declare_local (M3ID.NoID, Target.Integer.size,
                                   Target.Integer.align, CG.Type.Int,
                                   Type.GlobalUID (Int.T), in_memory := FALSE,
                                   up_level := FALSE, f := CG.Always);

    IF NOT definitelyGlobal THEN
      CG.Comment (-1, "Generating heap test");
      Procedure.StartCall (proc_test);
      CG.Push (lhs);
      CG.Pop_param (CG.Type.Addr);
      test := Procedure.EmitCall (proc_test);

      l_not := CG.Next_label ();
      CG.Push (test);
      CG.If_false (l_not, CG.Always - CG.Likely);  (* unlikely *)
      CG.Free (test);
    END;

    (******************** test the direction of the loop **********)

    CG.Push (lhs);
    CG.Push (rhs);
    l_forward := CG.Next_label ();
    l_loop := CG.Next_label ();
    CG.If_gt (l_forward, CG.Type.Addr, CG.Maybe);

    (******************** loop goes backward **********************)

    CG.Load_intt (-1);
    CG.Store_int (increment);

    CG.Push (lhs);
    CG.Load_intt (p.n_elts-1);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (lhs_start);

    CG.Push (rhs);
    CG.Load_intt (p.n_elts-1);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (rhs_start);

    CG.Push (lhs);
    CG.Load_intt (-1);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (lhs_end);

    CG.Jump (l_loop);

    (********************* loop goes forward **********************)

    CG.Set_label (l_forward);

    CG.Load_intt (1);
    CG.Store_int (increment);

    CG.Push (lhs);
    CG.Store_addr (lhs_start);

    CG.Push (rhs);
    CG.Store_addr (rhs_start);

    CG.Push (lhs);
    CG.Load_intt (p.n_elts);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (lhs_end);
    
    (******************** generate the loop ************************)

    CG.Set_label (l_loop);

    (* compute element address *)
    CG.Load_addr (lhs_start);
    CG.Load_addr (rhs_start);

    IF RefType.Is (p.element) OR ObjectType.Is (p.element) OR
       OpaqueType.Is (p.element) THEN
      (* materalize the actual value, not its address *)
      EVAL Type.CheckInfo (p.element, info);
      CG.Boost_alignment (info.alignment);
      Type.LoadScalar (p.element);
      Type.GenRC (p.element, TRUE);  (* already done heap check *)
    ELSE
      (* it is a record *)
      <* ASSERT RecordType.Split (p.element, ignore) OR
                Split (p.element, ignore1, ignore1) *>
      Type.GenRC (p.element, TRUE);  (* already done heap check *)
    END;

    (******************** bottom of loop ***************************)

    CG.Load_addr (lhs_start);
    CG.Load_int (increment);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (lhs_start);

    CG.Load_addr (rhs_start);
    CG.Load_int (increment);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (rhs_start);

    CG.Load_addr (lhs_start);
    CG.Load_addr (lhs_end);
    CG.If_ne (l_loop, CG.Type.Addr, CG.Likely);

    IF NOT definitelyGlobal THEN
      EVAL Type.CheckInfo (p, info);
      l_end := CG.Next_label ();

      CG.Jump (l_end);
      CG.Set_label (l_not);
      (* generate original call *)
      CG.Push (lhs);
      CG.Push (rhs);
      CG.Copy (info.size, overlap := FALSE);
      CG.Set_label (l_end);
    END;

    CG.Free (rhs);
    CG.Free (lhs);

    CG.Comment (-1, "Done generating reference counted array assignment code\n");
  END GenRC;

PROCEDURE GenMap (p: P;  offset, size: INTEGER;  refs_only: BOOLEAN) =
  BEGIN
    EVAL size;
    IF (p.n_elts <= 0) THEN RETURN END;
    TipeMap.Add (offset, TipeMap.Op.Mark, 0);
    Type.GenMap (p.element, offset, p.elt_pack, refs_only);
    TipeMap.Add (offset + p.elt_pack, TipeMap.Op.Array_1, p.n_elts);
    TipeMap.SetCursor (offset + p.total_size);
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.Array, p) THEN
      TipeDesc.AddX (Type.Number (p.index));
      Type.GenDesc (p.element);
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag      := "ARRAY";
    x.n_nodes  := 2;
    x.nodes[0] := p.index;
    x.nodes[1] := p.element;
  END FPrinter;

PROCEDURE ToText (p: P): TEXT =
  BEGIN
    RETURN ("ARRAY " & p.index.to_text() & " OF " & p.element.to_text());
  END ToText;

BEGIN
END ArrayType.

(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OpenArrayType.m3                                      *)
(* Last modified on Wed Sep  7 15:53:00 PDT 1994 by kalsow     *)
(*      modified on Sun Feb 24 04:39:01 1991 by muller         *)

(*
 * HISTORY
 * 09-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fix reference counting code to check if array size=0
 *
 * 09-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	ref-counting code
 *
 * 06-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for VIEW between representation-equivalent types
 *
 * 24-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed Subtyper to use TInt.EQ instead of # to compare TInts
 *
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added ToText, which overrides the to_text inherited 
 *	method
 *
 * 13-Nov-95  Wilson Hsieh (whsieh) at the University of Washington
 *	call to PackedType.Split needs EVAL because it returns BOOLEAN now
 *
 *)

MODULE OpenArrayType;

IMPORT M3, CG, Type, TypeRep, Error, Target, TInt, Word;
IMPORT ArrayType, PackedType, TipeMap, TipeDesc;

(* for ref counting *)
IMPORT Procedure, Runtime, Addr, Int, M3ID, RefType, ObjectType, OpaqueType, RecordType, Value;
IMPORT Fmt;

TYPE
  P = Type.T BRANDED "OpenArrayType.P" OBJECT
        element    : Type.T;
        baseElt    : Type.T;
        depth      : INTEGER;
        elt_align  : INTEGER;
        elt_pack   : INTEGER;
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
        genRC      := GenRC;
      END;

PROCEDURE New (element: Type.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.OpenArray);
    p.element    := element;
    p.baseElt    := NIL;
    p.depth      := -1;
    p.elt_pack   := 0;
    RETURN p;
  END New;

PROCEDURE Is (t: Type.T): BOOLEAN =
  BEGIN
    RETURN (Reduce (t) # NIL);
  END Is;

PROCEDURE Split (t: Type.T;  VAR element: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    element := p.element;
    RETURN TRUE;
  END Split;

PROCEDURE EltPack (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL)
      THEN RETURN p.elt_pack;
      ELSE RETURN 0;
    END;
  END EltPack;

PROCEDURE EltAlign (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL)
      THEN RETURN p.elt_align;
      ELSE RETURN Target.Byte;
    END;
  END EltAlign;

PROCEDURE OpenDepth (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN 0 END;
    IF (p.depth <= 0) THEN  p.depth := 1 + OpenDepth (p.element)  END;
    RETURN p.depth;
  END OpenDepth;

PROCEDURE OpenType (t: Type.T): Type.T =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN t END;
    IF (p.baseElt = NIL) THEN  p.baseElt := OpenType (p.element)  END;
    RETURN p.baseElt;
  END OpenType;

PROCEDURE Check (p: P) =
  VAR
    elt, elt_base : Type.T;
    align         : INTEGER;
    elt_info      : Type.Info;
    MinAlign := MAX (MAX (Target.Byte, Target.Structure_size_boundary),
                     MAX (Target.Address.align, Target.Integer.align));
  BEGIN
    p.element := Type.Check (p.element);
    elt := Type.CheckInfo (OpenType (p), elt_info);
    align := elt_info.alignment;
    p.elt_align := align;

    IF (elt_info.class = Type.Class.Packed) THEN
      EVAL PackedType.Split (elt, p.elt_pack, elt_base);
    ELSE (* naturally aligned elements must be OK *)
      p.elt_pack := (elt_info.size + align - 1) DIV align * align;
    END;

    align := MAX (align, MinAlign); (* == whole array alignment *)
    IF (p.elt_pack MOD Target.Byte) # 0 THEN
      Error.Msg ("SRC Modula-3 restriction: open array elements must be byte-aligned");
    ELSIF NOT Type.IsAlignedOk (p, align) THEN
      Error.Msg ("SRC Modula-3 restriction: scalars in packed array elements cannot cross word boundaries");
    END;

    p.info.size      := -1;
    p.info.min_size  := -1;
    p.info.alignment := align;
    p.info.cg_type   := CG.Type.Addr;
    p.info.class     := Type.Class.OpenArray;
    p.info.isTraced  := elt_info.isTraced;
    p.info.isEmpty   := elt_info.isEmpty;
    p.info.isSolid   := elt_info.isSolid AND (p.elt_pack <= elt_info.size);
    p.info.hash      := Word.Plus (Word.Times (23, OpenDepth (p)),
                              Word.Times (37, p.elt_pack));
  END Check;

PROCEDURE CheckAlign (p: P;  offset: INTEGER): BOOLEAN =
  VAR
    x0 := offset MOD Target.Integer.size;  x := x0;
    t  := OpenType (p);
  BEGIN
    REPEAT
      IF NOT Type.IsAlignedOk (t, x) THEN RETURN FALSE END;
      x := (x + p.elt_pack) MOD Target.Integer.size;
    UNTIL (x = x0);
    RETURN TRUE;
  END CheckAlign;

PROCEDURE DeclareTemp (t: Type.T): CG.Var =
  VAR
    p    := Reduce (t);
    size := Target.Address.pack + OpenDepth (p) * Target.Integer.pack;
  BEGIN
    RETURN CG.Declare_temp (size, Target.Address.align,
                            CG.Type.Struct, in_memory := TRUE);
  END DeclareTemp;

PROCEDURE Compiler (p: P) =
  VAR size := Target.Address.pack + OpenDepth (p) * Target.Integer.pack;
  BEGIN
    Type.Compile (p.element);
    CG.Declare_open_array (Type.GlobalUID(p), Type.GlobalUID(p.element), size);
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN (OpenDepth (a) = OpenDepth (b))
       AND Type.IsEqual (a.element, b.element, x);
  END EqualChk;

PROCEDURE EquivChk (a: P;  t: Type.T; x: Type.Assumption;
                    <* UNUSED *> canTruncate: BOOLEAN) : BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN (OpenDepth (a) = OpenDepth (b))
       AND Type.IsEquiv (a.element, b.element, x, FALSE);
  END EquivChk;

PROCEDURE Subtyper (a: P;  tb: Type.T): BOOLEAN =
  VAR ta, ia, ea, ib, eb: Type.T;  b: P;
  BEGIN
    ta := a;

    (* peel off the common open dimensions *)
    LOOP
      a := Reduce (ta);
      b := Reduce (tb);
      IF (a = NIL) OR (b = NIL) THEN EXIT END;
      ta := a.element;
      tb := b.element;
    END;

    (* peel off the remaining fixed dimensions of A and open dimensions of B *)
    LOOP
      b := Reduce (tb);
      IF (b = NIL) OR NOT ArrayType.Split (ta, ia, ea) THEN EXIT END;
      ta := ea;
      tb := b.element;
    END;

    (* peel off the fixed dimensions as long as the sizes are equal *)
    WHILE ArrayType.Split (ta, ia, ea) AND ArrayType.Split (tb, ib, eb) DO
      IF NOT TInt.EQ (Type.Number (ia), Type.Number (ib)) THEN RETURN FALSE END;
      ta := ea;
      tb := eb;
    END;

    RETURN Type.IsEqual (ta, tb, NIL);
  END Subtyper;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.OpenArray) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE InitCoster (p: P; zeroed: BOOLEAN): INTEGER =
  VAR n, m, res: Target.Int;  x: INTEGER;
  BEGIN
    IF    TInt.FromInt (Type.InitCost (p.element, zeroed), m)
      AND TInt.FromInt (20, n) (* guess that there are 20 elements *)
      AND TInt.Multiply (m, n, res)
      AND TInt.ToInt (res, x)
      THEN RETURN x;
      ELSE RETURN LAST (INTEGER);
    END;
  END InitCoster;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  VAR
    depth := OpenDepth (p);
    elt   := OpenType (p);
    top   : CG.Label;
    cnt   : CG.Val;
    max   : CG.Val;
    array := CG.Pop (); (* capture the l-value of the array *)
  BEGIN
    (* compute the number of elements *)
    FOR i := 0 TO depth-1 DO
      CG.Push (array);
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (CG.Type.Word) END;
    END;
    max := CG.Pop ();

    (* capture the pointer to the array elements *)
    CG.Push (array);
    CG.Open_elt_ptr (ArrayType.EltAlign (p));
    CG.Free (array);
    array := CG.Pop ();

    (* put down a loop to map the elements *)
    CG.Load_integer (TInt.Zero);
    cnt := CG.Pop_temp ();
    top := CG.Next_label (2);
    CG.Jump (top+1);
    CG.Set_label (top);

    (* map ARRAY[cnt] *)
    CG.Push (array);
    CG.Push (cnt);
    CG.Index_bytes (p.elt_pack);
    Type.InitValue (elt, zeroed);

    (* cnt := cnt + 1 *)
    CG.Push (cnt);
    CG.Load_integer (TInt.One);
    CG.Add (CG.Type.Int);
    CG.Store_temp (cnt);

    (* IF (cnt < NUMBER(ARRAY) GOTO TOP-OF-LOOP *)
    CG.Set_label (top+1);
    CG.Push (cnt);
    CG.Push (max);
    CG.If_lt (top, CG.Type.Int, CG.Likely);

    (* release the temps *)
    CG.Free (cnt);
    CG.Free (max);
    CG.Free (array);
  END GenInit;


PROCEDURE GenRC (p: P; definitelyGlobal: BOOLEAN; noOverlap := FALSE) =
  VAR
    lhs_depth := OpenDepth (p);
    lhs_hdr, rhs_hdr, lhs, rhs, test, size  : CG.Val;
    increment, rhs_start, lhs_start, lhs_end : CG.Var;
    proc_test            : Procedure.T;
    l_forward            : CG.Label;
    l_not, l_end, l_loop : CG.Label;
    element, ignore1     : Type.T;
    ignore               : Value.T;
    info                 : Type.Info;
  BEGIN
    CG.Comment (-1, "Generating reference counted open array assignment code\n");

    rhs_hdr := CG.Pop (); (* capture the rhs array header *)
    lhs_hdr := CG.Pop (); (* capture the lhs array header *)

    rhs := CG.Pop (); (* capture the rhs array *)
    lhs := CG.Pop (); (* capture the lhs array *)

    CG.Begin_block ();

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
      Procedure.StartCall (proc_test);
      CG.Push (lhs);
      CG.Pop_param (CG.Type.Addr);
      test := Procedure.EmitCall (proc_test);

      l_not := CG.Next_label ();
      CG.Push (test);
      CG.If_false (l_not, CG.Always - CG.Likely);  (* unlikely *)
      CG.Free (test);
    END;

    (******** compute the sizes of the open directions *****)
    (* can use either lhs or rhs *)
    CG.Comment (-1,
                "Finding length of copy, depth " & Fmt.Int (lhs_depth) & "\n");
    FOR i := 0 TO lhs_depth - 1 DO
      CG.Push (lhs_hdr);
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (CG.Type.Word) END;
    END;
    size := CG.Pop ();

    (********************** testing size **************************)

    CG.Comment (-1, "Testing size\n");
    l_end := CG.Next_label ();
    CG.Push (size);
    CG.Load_intt (0);
    CG.If_eq (l_end, CG.Type.Int, CG.Maybe);

    (******************** test the direction of the loop **********)

    CG.Comment (-1, "Testing direction of loop\n");
    CG.Push (lhs);
    CG.Push (rhs);
    l_forward := CG.Next_label ();
    l_loop := CG.Next_label ();
    CG.If_gt (l_forward, CG.Type.Addr, CG.Maybe);

    (******************** loop goes backward **********************)
    
    CG.Comment (-1, "Loop goes backwards\n");

    CG.Load_intt (-1);
    CG.Store_int (increment);

    CG.Push (lhs);
    CG.Push (size);
    CG.Load_intt (-1);
    CG.Add (CG.Type.Word);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (lhs_start);

    CG.Push (rhs);
    CG.Push (size);
    CG.Load_intt (-1);
    CG.Add (CG.Type.Word);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (rhs_start);

    CG.Push (lhs);
    CG.Load_intt (-1);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (lhs_end);

    CG.Jump (l_loop);

    (********************* loop goes forward **********************)

    CG.Comment (-1, "Loop goes forwards\n");

    CG.Set_label (l_forward);

    CG.Load_intt (1);
    CG.Store_int (increment);

    CG.Push (lhs);
    CG.Store_addr (lhs_start);

    CG.Push (rhs);
    CG.Store_addr (rhs_start);

    CG.Push (lhs);
    CG.Push (size);
    CG.Index_bytes (p.elt_pack);
    CG.Store_addr (lhs_end);

    (******************** generate the loop ************************)

    CG.Comment (-1, "Loop body\n");
    CG.Set_label (l_loop);

    (* compute element address *)
    CG.Load_addr (lhs_start);
    CG.Load_addr (rhs_start);

    element := OpenType (p);
    IF RefType.Is (element) OR ObjectType.Is (element) OR
       OpaqueType.Is (element) THEN
      (* materalize the actual value, not its address *)
      EVAL Type.CheckInfo (element, info);
      CG.Boost_alignment (info.alignment);
      Type.LoadScalar (element);
      Type.GenRC (element, TRUE);  (* already done heap check *)
    ELSE
      (* it is a record *)
      <* ASSERT RecordType.Split (element, ignore) OR
                ArrayType.Split (element, ignore1, ignore1) *>
      Type.GenRC (element, TRUE);  (* already done heap check *)
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
      CG.Jump (l_end);
      CG.Set_label (l_not);
      (* generate original call *)
      CG.Push (lhs);
      CG.Force ();
      CG.Push (rhs);
      CG.Force ();
      CG.Push (size);
      CG.Copy_n (p.elt_pack, NOT noOverlap);
    END;

    CG.Set_label (l_end);
    CG.End_block ();

    CG.Free (size);
    CG.Free (rhs);
    CG.Free (lhs);
    CG.Free (rhs_hdr);
    CG.Free (lhs_hdr);
    CG.Comment (-1, "Done generating reference counted open array assignment code\n");
  END GenRC;

PROCEDURE GenMap (p: P;  offset: INTEGER;  <*UNUSED*> size: INTEGER;
                  refs_only: BOOLEAN) =
  VAR a: INTEGER;
  BEGIN
    TipeMap.Add (offset, TipeMap.Op.OpenArray_1, OpenDepth (p));
    a := TipeMap.GetCursor ();
    Type.GenMap (OpenType (p), a, p.elt_pack, refs_only);
    TipeMap.Add (a + p.elt_pack, TipeMap.Op.Stop, 0);
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.OpenArray, p) THEN
      TipeDesc.AddI (OpenDepth (p));
      Type.GenDesc (OpenType (p));
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag      := "OPENARRAY";
    x.n_nodes  := 1;
    x.nodes[0] := p.element;
  END FPrinter;

PROCEDURE ToText (p: P): TEXT =
  BEGIN
    RETURN ("ARRAY OF " & p.element.to_text());
  END ToText;

BEGIN
END OpenArrayType.

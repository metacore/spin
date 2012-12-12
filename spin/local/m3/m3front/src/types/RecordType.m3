(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RecordType.m3                                         *)
(* Last modified on Tue Nov 29 14:14:17 PST 1994 by kalsow     *)
(*      modified on Tue Mar 26 02:49:22 1991 by muller         *)

(*
 * HISTORY
 * 11-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	make sure that record initializers get traced
 *
 * 02-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	support for reference counting
 *
 * 06-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for VIEW between representation-equivalent types
 *
 * 06-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added SetAlign method to set alignment from a containing
 *	 AlignedType
 *
 * 04-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	remove code to boost alignment of records
 *      also removed some dead (commented out) code
 *
 * 22-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Modified ToText to print all the field names to be able
 *	to identify a record type by the output of this function.
 *
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	Added ToText, which overrides the to_text inherited method
 *
 * 13-Nov-95  Wilson Hsieh (whsieh) at the University of Washington
 *	Call to PackedType.Split needs EVAL because it returns BOOLEAN now
 *
 *)

MODULE RecordType;

IMPORT M3, M3ID, CG, Type, TypeRep, Scope, Expr, Value, Token;
IMPORT Error, Field, Ident, PackedType, Target, TipeDesc;
IMPORT Word, AssignStmt, M3Buf;
FROM Scanner IMPORT Match, GetToken, cur;

(* for ref counting *)
IMPORT Procedure, Runtime, RefType, ObjectType, OpaqueType;

TYPE
  P = Type.T OBJECT
        fields     : Scope.T;
        recSize    : INTEGER := 0;
        align      : INTEGER := 0;
      OVERRIDES
        check      := Check;
        check_align:= CheckAlign;
        set_align  := SetAlign;
        isEqual    := EqualChk;
        isEquiv    := EquivChk;
        isSubtype  := TypeRep.NoSubtypes;
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
  VAR p := NEW (P);
  BEGIN
    TypeRep.Init (p, Type.Class.Record);

    p.fields := Scope.PushNew (FALSE, M3ID.NoID);
    Match (Token.T.tRECORD);
    ParseFieldList ();
    Match (Token.T.tEND);
    Scope.PopNew ();

    RETURN p;
  END Parse;

PROCEDURE ParseFieldList () =
  TYPE TK = Token.T;
  VAR
    j, n    : INTEGER;
    info    : Field.Info;
    nFields := 0;
  BEGIN
    info.offset := 0;
    WHILE (cur.token = TK.tIDENT) DO
      n := Ident.ParseList ();
      info.type := NIL;
      IF (cur.token = TK.tCOLON) THEN
        GetToken (); (* : *)
        info.type := Type.Parse ();
      END;
      info.dfault := NIL;
      IF (cur.token = TK.tEQUAL) THEN
        Error.Msg ("default value must begin with ':='");
        cur.token := TK.tASSIGN;
      END;
      IF (cur.token = TK.tASSIGN) THEN
        GetToken (); (* := *)
        info.dfault := Expr.Parse ();
      END;
      IF (info.type = NIL) AND (info.dfault = NIL) THEN
        Error.Msg ("fields must include a type or default value");
      END;
      j := Ident.top - n;
      FOR i := 0 TO n - 1 DO
        info.name  := Ident.stack [j + i];
        info.index := nFields;  INC (nFields);
        Scope.Insert (Field.New (info));
      END;
      DEC (Ident.top, n);
      IF (cur.token # TK.tSEMI) THEN EXIT END;
      GetToken (); (* ; *)
    END;
  END ParseFieldList;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Record) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE Split (t: Type.T;  VAR fields: Value.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    fields := Scope.ToList (p.fields);
    RETURN TRUE;
  END Split;

PROCEDURE LookUp (t: Type.T;  field: M3ID.T;  VAR obj: Value.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    obj := Scope.LookUp (p.fields, field, TRUE);
    RETURN (obj # NIL);
  END LookUp;

(***********************************************************************)

PROCEDURE Check (p: P) =
  VAR
    o        : Value.T;
    field    : Field.Info;
    info     : Type.Info;
    cs       := M3.OuterCheckState;
    hash     : INTEGER;
    is_solid : BOOLEAN;
  BEGIN
    Scope.TypeCheck (p.fields, cs);

    (* assign the final offsets to each field *)
    SizeAndAlignment (p.fields, p.recSize, p.align, is_solid);

    (* compute the hash value and per-field predicates *)
    p.info.isTraced := FALSE;
    p.info.isEmpty  := FALSE;
    p.info.isSolid  := is_solid;
    hash := Word.Plus (Word.Times (943, p.recSize), p.align);
    o := Scope.ToList (p.fields);
    WHILE (o # NIL) DO
      Field.Split (o, field);
      EVAL Type.CheckInfo (field.type, info);
      p.info.isTraced := p.info.isTraced OR info.isTraced;
      p.info.isEmpty  := p.info.isEmpty  OR info.isEmpty;
      hash := Word.Plus (Word.Times (hash, 41), M3ID.Hash (field.name));
      hash := Word.Plus (Word.Times (hash, 37), info.size);

      o := o.next;
    END;

    p.info.hash      := hash;
    p.info.size      := p.recSize;
    p.info.min_size  := p.recSize;
    p.info.alignment := p.align;
    p.info.cg_type   := CG.Type.Addr;
    p.info.class     := Type.Class.Record;
  END Check;

PROCEDURE SizeAndAlignment (fields: Scope.T;
                            VAR(*OUT*) recSize, recAlign: INTEGER;
                            VAR(*OUT*) is_solid: BOOLEAN) =
  VAR
    field      : Field.Info;
    base       : Type.T;
    type       : Type.T;
    fieldAlign : INTEGER;
    fieldSize  : INTEGER;
    anyPacked  := FALSE;
    o          : Value.T;
    info       : Type.Info;
    newSize    : INTEGER;
    newAlign   : INTEGER;
    curSize    : INTEGER;
  BEGIN
    (* compute the size of the record *)
    newSize  := 0; (* total size of the record *)
    newAlign := Target.Structure_size_boundary; (* minimum allowed alignment *)
    is_solid := TRUE;

    (* extract the fields and set their offsets *)
    o := Scope.ToList (fields);
    WHILE (o # NIL) DO
      Field.Split (o, field);
      type := Type.CheckInfo (field.type, info);
      is_solid := is_solid AND info.isSolid;
      IF (info.class = Type.Class.Packed) THEN
        EVAL PackedType.Split (type, fieldSize, base);
        anyPacked := TRUE;
      ELSE
        fieldSize  := info.size;
        fieldAlign := info.alignment;
        newAlign   := MAX (newAlign, fieldAlign);
        curSize    := newSize;
        newSize    := RoundUp (curSize, fieldAlign);
        is_solid   := is_solid AND (curSize = newSize);
      END;
      Field.SetOffset (o, newSize);
      INC (newSize, fieldSize);
      o := o.next;
    END;

    IF (anyPacked) THEN
      (**************************************************
      (* add a little bit of C compatibility *)
      IF (Target.PCC_bitfield_type_matters) THEN
        newAlign := MAX (newAlign, Target.Integer.align);
      END;
      ***************************************************)
      (* find an alignment that avoids scalar word crossings *)
      IF NOT FindAlignment (newAlign, fields) THEN
        Error.Msg ("SRC Modula-3 restriction: scalars in packed fields cannot cross word boundaries");
      END;
    END;

    curSize := newSize;
    newSize := RoundUp (curSize, newAlign);
    is_solid := is_solid AND (curSize = newSize);
    (* make sure that all copy operations are an integral number of
       aligned transfers. *)

    recSize  := newSize;
    recAlign := newAlign;
  END SizeAndAlignment;

PROCEDURE FindAlignment (VAR align: INTEGER;  fields: Scope.T): BOOLEAN =
  VAR x: INTEGER;
  BEGIN
    FOR a := FIRST (Target.Alignments) TO LAST (Target.Alignments) DO
      x := Target.Alignments[a];
      IF (x >= align) THEN
        (* see if all the fields are ok at this alignment *)
        IF AlignmentOK (x, fields) THEN
          align := x;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END FindAlignment;

PROCEDURE AlignmentOK (align: INTEGER;  fields: Scope.T): BOOLEAN =
  VAR o: Value.T;  field: Field.Info;  rec_offs := 0;
  BEGIN
    REPEAT
      o := Scope.ToList (fields);
      WHILE (o # NIL) DO
        Field.Split (o, field);
        IF NOT Type.IsAlignedOk (field.type, rec_offs + field.offset) THEN
          RETURN FALSE;
        END;
        o := o.next;
      END;
      rec_offs := (rec_offs + align) MOD Target.Integer.size;
    UNTIL (rec_offs = 0);
    RETURN TRUE;
  END AlignmentOK;

PROCEDURE RoundUp (size, alignment: INTEGER): INTEGER =
  BEGIN
    IF (alignment = 0)
      THEN RETURN size;
      ELSE RETURN ((size + alignment - 1) DIV alignment) * alignment;
    END;
  END RoundUp;

PROCEDURE CheckAlign (p: P;  offset: INTEGER): BOOLEAN =
  BEGIN
    RETURN AlignmentOK (offset, p.fields);
  END CheckAlign;

PROCEDURE SetAlign (p: P;  new_align: INTEGER) =
  BEGIN
    p.info.alignment := new_align;
  END SetAlign;

PROCEDURE Compiler (p: P) =
  VAR fields := Scope.ToList (p.fields);  o: Value.T;  n: INTEGER;
  BEGIN
    Scope.InitValues (p.fields);
    o := fields;  n := 0;
    WHILE (o # NIL) DO  INC (n);  o := o.next;  END;
    CG.Declare_record (Type.GlobalUID (p), p.recSize, n);
    o := fields;
    WHILE (o # NIL) DO  Field.EmitDeclaration (o);  o := o.next;  END;
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;  va, vb: Value.T;
  BEGIN
    (******* too sleazy!  since it depends on type checking ...
    IF (a.align # 0) AND (b.align # 0) THEN
      (* both have already been sized *)
      (* => make some simple sanity checks *)
      IF (a.recSize # b.recSize) THEN RETURN FALSE END;
      IF (a.align # b.align) THEN RETURN FALSE END;
    END;
    ********************************************************)

    (* compare the fields *)
    va := Scope.ToList (a.fields);
    vb := Scope.ToList (b.fields);
    WHILE (va # NIL) AND (vb # NIL) DO
      IF NOT Field.IsEqual (va, vb, x) THEN RETURN FALSE END;
      va := va.next;  vb := vb.next;
    END;

    RETURN (va = NIL) AND (vb = NIL);
  END EqualChk;

PROCEDURE EquivChk (a: P;  t: Type.T; x: Type.Assumption; canTruncate: BOOLEAN)
  : BOOLEAN =
  VAR b: P := t;  va, vb: Value.T;
  BEGIN
    (* compare the fields *)
    va := Scope.ToList (a.fields);
    vb := Scope.ToList (b.fields);
    WHILE (va # NIL) AND (vb # NIL) DO
      IF NOT Field.IsEquiv (va, vb, x, canTruncate AND vb.next = NIL) THEN
        RETURN FALSE;
      END;
      va := va.next;  vb := vb.next;
    END;

    IF canTruncate THEN
      (* a can be "bigger" *)
      RETURN vb = NIL;
    ELSE
      (* a and b must have the same fields *)
      RETURN (va = NIL) AND (vb = NIL);
    END;
  END EquivChk;

PROCEDURE InitCoster (p: P;  zeroed: BOOLEAN): INTEGER =
  VAR cost: INTEGER;  v: Value.T;  field: Field.Info;
  BEGIN
    v := Scope.ToList (p.fields);  cost := 0;
    WHILE (v # NIL) DO
      Field.Split (v, field);
      IF (field.dfault = NIL) THEN
        INC (cost, Type.InitCost (field.type, zeroed ));
      ELSIF (zeroed) AND Expr.IsZeroes (field.dfault) THEN
        (* no charge *)
      ELSE
        INC (cost, MAX (1, Type.InitCost (field.type, FALSE)));
      END;
      IF (cost < 0) THEN RETURN LAST (INTEGER) END;
      v := v.next;
    END;
    RETURN cost;
  END InitCoster;

PROCEDURE GenRC (p: P; definitelyGlobal: BOOLEAN;
                 <* UNUSED *> noOverlap := FALSE) =
  VAR
    field                    : Field.Info;
    v                        := Scope.ToList (p.fields);
    lhs, rhs, test           : CG.Val;
    proc_test                : Procedure.T;
    lhs_info, field_info     : Type.Info;
    l_not, l_end             : CG.Label;
  BEGIN
    rhs := CG.Pop (); (* capture the rhs record address *)
    lhs := CG.Pop (); (* capture the lhs record address *)

    (* find info for refcount test *)
    proc_test := Runtime.LookUpProc (Runtime.Hook.IsShared);

    CG.Comment (-1, "Generating reference counted record assignment code\n");
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

    (* definitely writing into a global ref *)
    (* FIXME: will not work with the following
         RECORD
           BITS 20 FOR blah
           BITS 44 FOR blah
           REF INTEGER

       need to find all REFs: offset is sufficient
       can call CG.Copy on rest of record, and then
       call genRC on REF fields
     *)
    WHILE (v # NIL) DO
      Field.Split (v, field);
      EVAL Type.CheckInfo (field.type, field_info);

      IF NOT field_info.isTraced THEN
        (* first arg *)
        CG.Push (lhs);
        CG.Add_offset (field.offset);
        (* second arg *)
        CG.Push (rhs);
        CG.Add_offset (field.offset);
        (* do the copy *)
        CG.Copy (field_info.size, overlap := FALSE);
      ELSE
        (* first arg *)
        CG.Push (lhs);
        CG.Add_offset (field.offset);
        (* second arg *)
        CG.Push (rhs);
        CG.Add_offset (field.offset);
        IF RefType.Is (field.type) OR ObjectType.Is (field.type) OR
          OpaqueType.Is (field.type) THEN
          (* materalize the actual value, not its address *)
          CG.Boost_alignment (field_info.alignment);
          Type.LoadScalar (field.type);
          field.type.genRC (TRUE);  (* already done heap check *)
        ELSE
          (* it is a record *)
          <* ASSERT Reduce (field.type) # NIL *>
          field.type.genRC (TRUE);  (* already done heap check *)
        END;
      END;

      v := v.next;
    END;

    IF NOT definitelyGlobal THEN
      EVAL Type.CheckInfo (p, lhs_info);
      l_end := CG.Next_label ();

      CG.Jump (l_end);
      CG.Set_label (l_not);
      (* generate original call *)
      CG.Push (lhs);
      CG.Push (rhs);
      CG.Copy (lhs_info.size, overlap := FALSE);
      CG.Set_label (l_end);
    END;
    CG.Free (lhs);
    CG.Free (rhs);
    CG.Comment (-1, "Done generating reference counted record assignment code\n");
  END GenRC;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  VAR
    field  : Field.Info;
    v      := Scope.ToList (p.fields);
    ptr    := CG.Pop (); (* capture the record address *)
  BEGIN
    WHILE (v # NIL) DO
      Field.Split (v, field);
      IF (field.dfault = NIL) THEN
        IF (Type.InitCost (field.type, zeroed) > 0) THEN
          CG.Push (ptr);
          CG.Boost_alignment (p.align);
          CG.Add_offset (field.offset);
          Type.InitValue (field.type, zeroed);
        END;
      ELSIF (NOT zeroed) OR (NOT Expr.IsZeroes (field.dfault)) THEN
        Expr.Prep (field.dfault);
        CG.Push (ptr);
        CG.Boost_alignment (p.align);
        CG.Add_offset (field.offset);
        AssignStmt.Emit (field.type, field.dfault, M3.Global.Maybe);
      END;
      v := v.next;
    END;
    CG.Free (ptr);
  END GenInit;

PROCEDURE GenMap (p: P;  offset: INTEGER;  <*UNUSED*> size: INTEGER;
                  refs_only: BOOLEAN) =
  VAR v := Scope.ToList (p.fields);  field: Field.Info;
  BEGIN
    WHILE (v # NIL) DO
      Field.Split (v, field);
      Type.GenMap (field.type, offset + field.offset, -1, refs_only);
      v := v.next;
    END;
  END GenMap;

PROCEDURE GenDesc (p: P) =
  VAR v := Scope.ToList (p.fields);  field: Field.Info;  n := 0;  vv := v;
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.Record, p) THEN
      (* count the fields *)
      WHILE (vv # NIL) DO  INC (n);  vv := vv.next;  END;
      TipeDesc.AddI (n);

      (* and generate them *)
      WHILE (v # NIL) DO
        Field.Split (v, field);
        Type.GenDesc (field.type);
        v := v.next;
      END;
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  VAR v: Value.T;  n: INTEGER;
  BEGIN
    M3Buf.PutText (x.buf, "RECORD");

    (* count the fields and build the tag *)
    v := Scope.ToList (p.fields);  n := 0;
    WHILE (v # NIL) DO  INC (n, Value.AddFPTag (v, x));  v := v.next;  END;
    x.n_nodes := n;

    (* add the field edges *)
    IF (n > NUMBER (x.nodes)) THEN
      x.others := NEW (REF ARRAY OF Type.T, n);
    END;
    v := Scope.ToList (p.fields);  n := 0;
    WHILE (v # NIL) DO  n := Value.AddFPEdges (v, x, n);  v := v.next;  END;
  END FPrinter;

PROCEDURE ToText (p: P): TEXT =
  VAR t: TEXT := "RECORD";
      o: Value.T;
  BEGIN
    o := Scope.ToList (p.fields);
    WHILE (o # NIL) DO
      t := t & " " & M3ID.ToText(Value.CName (o));
      o := o.next;
    END;
    t := t & " END";
    RETURN t;
  END ToText;

BEGIN
END RecordType.

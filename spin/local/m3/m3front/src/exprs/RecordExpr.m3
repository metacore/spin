(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RecordExpr.m3                                         *)
(* Last modified on Fri Feb 24 16:46:20 PST 1995 by kalsow     *)
(*      modified on Wed Nov 28 02:47:43 1990 by muller         *)

MODULE RecordExpr;

IMPORT M3, M3ID, CG, Expr, ExprRep, Error, Type, RecordType, Module;
IMPORT Value, Field, KeywordExpr, RangeExpr, AssignStmt, M3Buf;

TYPE
  Info = RECORD
    field : Value.T;
    type  : Type.T;
    val   : Expr.T;
    name  : M3ID.T;
    done  : BOOLEAN;
  END;

TYPE
  P = Expr.T OBJECT
        tipe       : Type.T;
        args       : Expr.List;
        map        : REF ARRAY OF Info;
        tmp        : CG.Var;
        tmp_cnt    : INTEGER;
        tmp_offset : INTEGER;
        folded     : BOOLEAN;
        is_const   : BOOLEAN;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := PrepLV;
        compile      := CompileLV;
        prepLV       := PrepLV;
        compileLV    := CompileLV;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
	isZeroes     := IsZeroes;
	genFPLiteral := GenFPLiteral;
	prepLiteral  := PrepLiteral;
	genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (type: Type.T;  args: Expr.List): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.type       := type;
    p.tipe       := type;
    p.args       := args;
    p.map        := NIL;
    p.tmp        := NIL;
    p.tmp_cnt    := 0;
    p.tmp_offset := 0;
    p.folded     := FALSE;
    p.is_const   := FALSE;
    RETURN p;
  END New;

PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    RETURN (TYPECODE (e) = TYPECODE (P));
  END Is;

PROCEDURE Qualify (e: Expr.T;  id: M3ID.T;  VAR result: Expr.T): BOOLEAN =
  VAR
    p      : P;
    val    : Value.T;
    field  : Field.Info;
    z      : Expr.T;
    key    : M3ID.T;
    value  : Expr.T;
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(x) => p := x;
    ELSE      RETURN FALSE;
    END;
    IF NOT RecordType.LookUp (p.tipe, id, val) THEN RETURN FALSE END;
    Field.Split (val, field);
    FOR i := 0 TO LAST (p.args^) DO
      z := p.args[i];
      IF (KeywordExpr.Split (z, key, value)) THEN
        IF (key = id) THEN result := value; RETURN TRUE END;
      ELSIF (i = field.index) THEN
        result := z;  RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END Qualify;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR
    n        : INTEGER;
    key      : M3ID.T;
    value, e : Expr.T;
    posOK    : BOOLEAN;
    offset   : INTEGER;
    dfault   : Expr.T;
    fields   : Value.T;
    v        : Value.T;
    field    : Field.Info;
  BEGIN
    p.tipe := Type.Check (p.tipe);
    FOR i := 0 TO LAST (p.args^) DO Expr.TypeCheck (p.args[i], cs) END;
    p.type := p.tipe;
    IF NOT RecordType.Split (p.tipe, fields) THEN
      Error.Msg ("record constructor must specify a record type");
      RETURN;
    END;

    (* count the fields *)
    v := fields;  n := 0;
    WHILE (v # NIL) DO INC (n);  v := v.next END;
    

    (* build vectors to map the fields *)
    p.map := NEW (REF ARRAY OF Info, n);
    v := fields;  n := 0;
    WHILE (v # NIL) DO
      Field.Split (v, field);
      WITH z = p.map[n] DO
        z.field := v;
        z.name  := field.name;
        z.type  := field.type;
        z.val   := field.dfault;
        z.done  := FALSE;
      END;
      v := v.next;
      INC (n);
    END;
    posOK := TRUE;
    EVAL Fold (p);  (* make sure that the everything that can be folded is *)

    FOR i := 0 TO LAST (p.args^) DO
      e := p.args[i];
      IF RangeExpr.Split (e, value, dfault) THEN
        Error.Msg ("range expressions not allowed in record constructors");
      END;

      IF KeywordExpr.Split (e, key, value) THEN
        posOK := FALSE;
        offset := 0;
        e := value;
        LOOP
          IF (offset >= n) THEN
            Error.ID (key, "unknown field");
            offset := i;
            EXIT;
          END;
          IF (p.map[offset].name = key) THEN EXIT END;
          INC (offset);
        END;
      ELSE (* positional parameter *)
        IF (NOT posOK) THEN
          Error.Msg ("positional values must precede keyword values");
        END;
        IF (i >= n)
	  THEN  Error.Msg ("too many values");  offset := n - 1;
          ELSE  offset := i;
        END;
      END;

      IF (0 <= offset) AND (offset < n) THEN
        WITH z = p.map[offset] DO
          IF (z.done) THEN Error.ID (z.name, "field already specified"); END;
          z.done := TRUE;
          IF NOT Type.IsAssignable (z.type, Expr.TypeOf (e)) THEN
            Error.ID (z.name, "expression is not assignable to field");
          ELSE
            AssignStmt.Check (z.type, e, cs);
            z.val := e;
          END;
        END;
      ELSE
        (* some other error, so don't even try *)
      END;
    END;

    FOR i := 0 TO n - 1 DO
      WITH z = p.map[i] DO
        IF (NOT z.done) AND (z.val = NIL) THEN
          Error.ID (z.name, "no value specified for field");
        END;
      END;
    END;
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  VAR b: P;
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => b := p;
    ELSE      RETURN FALSE;
    END;
    IF (NOT Type.IsEqual (a.tipe, b.tipe, x))
      OR ((a.args = NIL) # (b.args = NIL))
      OR ((a.args # NIL) AND (NUMBER (a.args^) # NUMBER (b.args^))) THEN
      RETURN FALSE;
    END;
    FOR i := 0 TO LAST (a.args^) DO
      IF NOT Expr.IsEqual (a.args[i], b.args[i], x) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END EqCheck;

PROCEDURE NeedsAddress (<*UNUSED*> p: P) =
  BEGIN
    (* yep, all records get memory addresses *)
  END NeedsAddress;

PROCEDURE PrepLV (p: P) =
  VAR
    info: Type.Info;
    field: Field.Info;
  BEGIN
    IF (Fold (p) # NIL) THEN RETURN END;

    INC (p.tmp_cnt);
    IF (p.tmp # NIL) AND (p.tmp_cnt > 1) THEN RETURN END;
    EVAL Type.CheckInfo (p.type, info);

    p.tmp := CG.Declare_temp (info.size, info.alignment,
                              CG.Type.Struct, in_memory := TRUE);

    FOR i := 0 TO LAST (p.map^) DO
      WITH z = p.map[i] DO
        Expr.Prep (z.val);
        Field.Split (z.field, field);
        CG.Load_addr_of (p.tmp, field.offset, info.alignment);
        AssignStmt.Emit (field.type, z.val);
      END;
    END;
  END PrepLV;

PROCEDURE CompileLV (p: P) =
  VAR info: Type.Info;  offset: INTEGER;
  BEGIN
    EVAL Type.CheckInfo (p.type, info);
    IF (p.is_const) THEN
      offset := Module.Allocate (info.size, info.alignment, "*record*");
      PrepLiteral (p, p.tipe);
      GenLiteral (p, offset, p.tipe);
      CG.Load_addr_of (Module.GlobalData (NIL), offset, info.alignment);
    ELSE
      DEC (p.tmp_cnt);
      IF (p.tmp_cnt <= 0) THEN
        CG.Load_addr_of_temp (p.tmp, 0, info.alignment);
        p.tmp := NIL;
      ELSE
        CG.Load_addr_of (p.tmp, 0, info.alignment);
      END;
    END;
  END CompileLV;

PROCEDURE Fold (p: P): Expr.T =
  VAR e: Expr.T;
  BEGIN
    IF (NOT p.folded) THEN
      p.folded   := TRUE;
      p.is_const := TRUE;
      FOR i := 0 TO LAST (p.args^) DO
        e := Expr.ConstValue (p.args[i]);
        IF (e = NIL) THEN p.is_const := FALSE; ELSE p.args[i] := e; END;
      END;
    END;
    IF p.is_const
      THEN RETURN p;
      ELSE RETURN NIL;
    END;
  END Fold;

PROCEDURE IsZeroes (p: P): BOOLEAN =
  BEGIN
    <* ASSERT p.map # NIL *> (* must already be checked *)
    FOR i := 0 TO LAST (p.map^) DO
      IF NOT Expr.IsZeroes (p.map[i].val) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "RECORD<");
    FOR i := 0 TO LAST (p.map^) DO
      IF (i > 0) THEN M3Buf.PutChar (buf, ',') END;
      Expr.GenFPLiteral (p.map[i].val, buf);
    END;
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

PROCEDURE PrepLiteral (p: P;   <*UNUSED*> type: Type.T) =
  VAR e: Expr.T;  field: Field.Info;
  BEGIN
    <* ASSERT p.map # NIL *> (* must already be checked *)
    FOR i := 0 TO LAST (p.map^) DO
      WITH z = p.map[i] DO
        e := Expr.ConstValue (z.val);  <* ASSERT e # NIL *>
        IF NOT Expr.IsZeroes (e) THEN
          Field.Split (z.field, field);
          Expr.PrepLiteral (e, field.type);
        END;
      END;
    END;
  END PrepLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  <*UNUSED*> type: Type.T) =
  VAR e: Expr.T;  field: Field.Info;
  BEGIN
    <* ASSERT p.map # NIL *> (* must already be checked *)
    FOR i := 0 TO LAST (p.map^) DO
      WITH z = p.map[i] DO
        e := Expr.ConstValue (z.val);  <* ASSERT e # NIL *>
        IF NOT Expr.IsZeroes (e) THEN
          Field.Split (z.field, field);
          Expr.GenLiteral (e, offset + field.offset, field.type);
        END;
      END;
    END;
  END GenLiteral;

BEGIN
END RecordExpr.

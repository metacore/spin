MODULE InterfaceExpr;

IMPORT Module, CG, Target, Expr, ExprRep, Type, M3, M3ID, M3Buf;
IMPORT RTCodeModule;

TYPE
  P = Expr.T BRANDED "InterfaceExpr.T" OBJECT
        value: Module.T;
      OVERRIDES
        typeOf       := TypeOf;
        check        := ExprRep.NoCheck;
        need_addr    := ExprRep.NotAddressable;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := ExprRep.Self;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := GenFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (interface: Module.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW(P);
    ExprRep.Init(p);
    p.value := interface;
    RETURN p;
  END New;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    IF p.value # NIL THEN
      RETURN RTCodeModule.InterfaceDesc;
    ELSE
      RETURN RTCodeModule.ModuleDesc;
    END;
  END TypeOf;

PROCEDURE Compile (p: P) =
  BEGIN
    CG.Load_addr_of(Module.GlobalData(p.value), 0, Target.Byte);
  END Compile;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN a.value = b.value;
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE GenFPLiteral(p: P; buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "INTERFACE_UNIT(");
    M3Buf.PutText (buf, M3ID.ToText(Module.Name(p.value)));
    M3Buf.PutChar (buf, ')');
  END GenFPLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  type: Type.T) =
  VAR info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (type, info);
    CG.Init_var (offset, Module.GlobalData(p.value), 0);
  END GenLiteral;

BEGIN
END InterfaceExpr.



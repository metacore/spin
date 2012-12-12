(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added PROCID pragma
 * 13-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Separated pragma into Pragma.*3
 * 29-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to use hash code for the interface name as the proc base.
 * 17-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	added <* .. *> support & user defined types.
 * 09-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	added ref, enum, set support.
 * 03-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	borrowed from stubgen.
 *)

MODULE Pass1;

IMPORT Declaration;
IMPORT TypeTable, NameSeq;
IMPORT AST, ASTWalk;
IMPORT Type;
IMPORT M3AST_AS;
IMPORT M3CId, M3Error;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F,
       M3AST_TL_F, M3Context, M3ASTNext;
IMPORT M3CConcTypeSpec;
IMPORT M3CTypesMisc;
IMPORT M3CBackEnd, M3CBackEnd_C;

IMPORT SeqM3AST_AS_Enum_id, SeqM3AST_AS_Fields, SeqM3AST_AS_Field_id;
IMPORT SeqM3AST_AS_M3TYPE;
IMPORT SeqM3AST_AS_Method;
IMPORT SeqM3AST_AS_Formal_param;
IMPORT SeqM3AST_AS_FORMAL_ID;
IMPORT SeqM3AST_AS_IMPORTED;
IMPORT SeqM3AST_AS_Import_item;
IMPORT SeqM3AST_AS_Qual_used_id;
IMPORT M3CBitSize;
IMPORT Fmt, Scan, TextSeq, Pragma, TextRefTbl;
IMPORT Msg, Text, Word;
IMPORT Lex;
IMPORT FloatMode;
IMPORT Module;

REVEAL Handle = HandlePublic BRANDED OBJECT
  context : M3Context.T; (* Obscure cookie for m3 frontend. *)
  procId : INTEGER;
OVERRIDES
  callback := Node;
END;

PROCEDURE QualIDToText(q : M3AST_AS.Qual_used_id) : TEXT =
  VAR
    name := M3CId.ToText(q.as_id.lx_symrep);
  BEGIN
    IF q.as_intf_id # NIL THEN
      (* Imported name *)
      name := M3CId.ToText(q.as_intf_id.lx_symrep) & "." & name;
    END;
    RETURN name;
  END QualIDToText;

(* Syntax sugar(?) for "M3CTypesMisc.GetTYPE_SPECFromM3TYPE".
 This converts M3TYPE to TYPE_SPEC. This is essentially just a
 narrowing. 
 *)
PROCEDURE M3typeToTypeSpec(m3 : M3AST_AS.M3TYPE) : M3AST_AS.TYPE_SPEC =
VAR
  ts : M3AST_AS.TYPE_SPEC;
BEGIN
  M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3, ts);
  RETURN ts;
END M3typeToTypeSpec;

(* Create a handle that can be passed to tree-walker.
 XXX Someone document this procedure please! -ys
 *)
PROCEDURE NewHandle (intf, domain: TEXT; context: M3Context.T): Handle=
VAR 
  h: Handle;
  typeTable : TypeTable.T;
BEGIN
  Msg.Debug(Msg.POS_VOID, "Processing "&intf&".\n");
  typeTable := NEW(TypeTable.T).init();
  Type.RegisterStandardTypes(typeTable);
  
  h := NEW(Handle,
	   context := context,
	   typeTable := typeTable);
  h.m.intf := intf;
  h.m.domain := domain;
  h.m.names := NEW(NameSeq.T).init();
  h.m.imports := NEW(TextSeq.T).init();
  h.m.exceptionList := NEW(TextRefTbl.Default).init();
  h.m.descendingProcIDs := FALSE;
  RETURN h;
END NewHandle;

(* Convert sequence of M3TYPEs into array of Type's. *)

(* Convert sequence of Fields into array of Types'. *)
PROCEDURE InternSeqField (h : Handle; f : SeqM3AST_AS_Fields.T)
: REF ARRAY OF Type.Field =
VAR fields : REF ARRAY OF Type.Field;
  iter := SeqM3AST_AS_Fields.NewIter(f);
  item : M3AST_AS.Fields; 
  i : INTEGER := 0;
BEGIN

  (* Allocate the fields. *)
  VAR nFields : INTEGER := 0;
    iter := M3ASTNext.NewIterField(f);
    dummy : M3AST_AS.Field_id;
  BEGIN
    WHILE M3ASTNext.Field(iter, dummy) DO INC(nFields) END;
    fields := NEW(REF ARRAY OF Type.Field, nFields);
  END;
  
  (* Scan input and fill in FIELDS *)
  WHILE SeqM3AST_AS_Fields.Next(iter, item) DO 
    VAR iterId := SeqM3AST_AS_Field_id.NewIter(item.as_id_s);
      fieldId: M3AST_AS.Field_id;
      default : TEXT;
      attr := ParseTypeAttr(h.m, item);
    BEGIN
      IF item.as_default # NIL THEN 
	default := UncompileConstant(item.as_default);
      END;
      
      WHILE SeqM3AST_AS_Field_id.Next(iterId, fieldId) DO
	fields[i].name := M3CId.ToText(fieldId.lx_symrep);
	IF item.as_type = NIL THEN
	  fields[i].type := InternType(h, fieldId.sm_type_spec, attr)
	ELSE
	  fields[i].type := InternType(h, item.as_type, attr);
	END;
	fields[i].default := default;
	INC(i);
      END;
    END;
  END;
  RETURN fields;
END InternSeqField;

(* Return the name of the type, if it is defined(i.e., type was assigned a
 *  name using TYPE declaration). Else, return NIL.
 *)
PROCEDURE GetTypeAlias (m3type : M3AST_SM_F.M3TYPE) : TEXT =
BEGIN
  TYPECASE m3type OF 
  | M3AST_AS.Named_type(nt) =>
    RETURN M3CId.ToText(nt.as_qual_id.as_id.lx_symrep);
  ELSE
    RETURN NIL;
  END;
END GetTypeAlias;

(* Get the brand name for a ref or an object. If t has brand but 
 * does not have explicit brand name, then this procedure returns
 *  TRUE and BRAND becomes NIL. If t does not have brand, then
 *  the procedure returns FALSE.
 *)
PROCEDURE GetBrand(t : M3AST_AS.BRANDED_TYPE; VAR brand : TEXT) : BOOLEAN =
BEGIN
  IF t.as_brand = NIL THEN RETURN FALSE; END;
  IF t.as_brand.as_exp = NIL THEN
    brand := NIL;
  ELSE
    brand := UncompileConstant(t.as_brand.as_exp);
  END;
  RETURN TRUE;
END GetBrand;

TYPE
  Attr = {Internal, Ctext, UserTypeName};
  TypeAttrs = RECORD
    a: SET OF Attr;
    userTypeName: TEXT;
  END;
  
CONST
  NullAttr = TypeAttrs{a := SET OF Attr{}, userTypeName := NIL};
  

PROCEDURE ParseTypeAttr (READONLY m: Module.T; p: AST.NODE): TypeAttrs =
  VAR
    attr: TypeAttrs;
    pragma: TEXT;
  BEGIN
    attr.a := SET OF Attr{};
    attr.userTypeName := NIL;
    pragma := Pragma.Find(m, p, "AS");
    IF pragma # NIL THEN
      (* old pragma names *)
      IF Text.Equal(pragma, "CTEXT") THEN
	attr.a := attr.a + SET OF Attr{Attr.Ctext};
      ELSIF Text.Equal(pragma, "EXTENSIONONLY") THEN
	attr.a := attr.a + SET OF Attr{Attr.Internal};
      ELSE
	attr.a := attr.a + SET OF Attr{Attr.UserTypeName};
	attr.userTypeName := pragma;
      END;
    END;
    IF Pragma.Find(m, p, "CTEXT") # NIL THEN
      attr.a := attr.a + SET OF Attr{Attr.Ctext};
    END;
    IF Pragma.Find(m, p, "INTERNAL") # NIL THEN
      attr.a := attr.a + SET OF Attr{Attr.Internal};
    END;
    RETURN attr;
  END ParseTypeAttr;

(* Convert TYPE_SPEC to Type.
 Also, if Type is not present in h.typeTable, then register it.

 if userTypeName is not NIL, then it specifies the user-side type name.
 *)
PROCEDURE InternType (h : Handle;
		      m3type : M3AST_AS.M3TYPE;
		      attr: TypeAttrs := NullAttr) : Type.T =
VAR
  r : REFANY;
  ts : M3AST_TM_F.TYPE_SPEC;
  retval : Type.T := NIL;
  typeName : TEXT := NIL;
  srcPos : Msg.Pos;
BEGIN
  
  ts := M3typeToTypeSpec(m3type);

  srcPos.pos := ts.lx_srcpos;
  IF ts.tmp_unit_id # NIL THEN
    srcPos.cu := ts.tmp_unit_id.sm_spec.sm_comp_unit;
  END;
  
  (* If the type has an alias, extract it. *)
  TYPECASE m3type OF
  | M3AST_AS.Named_type(nt) =>
    typeName := QualIDToText(nt.as_qual_id);
  ELSE
    (* it's ok to have NIL typename *)
  END;

  (* XXX I forgot why I have to put this thing at the top *)
  IF Attr.Internal IN attr.a THEN 
    IF typeName = NIL THEN
      Msg.Fatal(srcPos, "You can't specify EXTENSIONONLY on an anonymous type.");
    END;
    IF Text.Equal(typeName, "Strand.T") THEN
      RETURN Type.strand;
    ELSIF Text.Equal(typeName, "CPU.SavedState") THEN
      RETURN Type.state;
    ELSIF Text.Equal(typeName, "Space.T") THEN
      RETURN Type.space;
    ELSE
      Msg.Fatal(srcPos, "Unknown EXTENSIONONLY type : " & typeName & "\n");
    END;
  END;

  IF Attr.Ctext IN attr.a AND Text.Equal(typeName, "NameServer.Name") THEN
    RETURN Type.nsname;
  END;

  IF h.typeTable.get(ts, r) THEN
    (* This type is already used before. *)
    retval := NARROW(r, Type.T);
    INC(retval.nUsed);
  ELSE 
    M3CBitSize.Set(ts);
    TYPECASE ts OF
    | M3AST_AS.Opaque_type (ot) =>
      IF ot.sm_concrete_type_spec = NIL THEN
	VAR
	  o : Type.Opaque;
	  revealedPart : Type.Ref;
	BEGIN
	  Msg.Debug(srcPos, "opaque type");
	  (* XXX I don't know what's going on here.
	   Probably, we are trying to know what is revealed
	   at this point. See stubgen codes for detail. - yasushi *)
	  WITH r = M3CConcTypeSpec.CurrentReveal(ot) DO
	    revealedPart := NARROW(InternType(h, r), Type.Ref);
	    o := NEW(Type.Opaque, revealedPart:=revealedPart);
	  END;
	  o.init(ot, srcPos);
	  EVAL h.typeTable.put(ot, o);
	  retval := o;
	END;
      ELSE
	(* Pull the ancestor chain *)
	Msg.Debug(srcPos, "opaque-but concrete");
	retval := InternType(h, ot.sm_concrete_type_spec);
      END;

    | M3AST_AS.Real_type => retval := Type.real;
    | M3AST_AS.LongReal_type => retval := Type.longreal;
    | M3AST_AS.Extended_type => retval := Type.extended;
    | M3AST_AS.Integer_type => retval := Type.integer;
    | M3AST_AS.Null_type => retval := Type.null;
    | M3AST_AS.RefAny_type => retval := Type.refany;
    | M3AST_AS.Root_type => retval := Type.root;
    | M3AST_AS.ProcAny_type =>
      Msg.Debug(srcPos, "PROCANY is not understood.");

    | M3AST_AS.Array_type(t) =>
      VAR types : REF ARRAY OF Type.T;
	idxTypes : REF ARRAY OF Type.Ordinal;
	arrayType : Type.T;
	m3type : M3AST_AS.M3TYPE;
	iter: SeqM3AST_AS_M3TYPE.Iter;
	i : CARDINAL := 0;
      BEGIN
	(* Get the index types, and make sure that they are
	 *  Ordinal types. This is probably a overkill, because
	 *  M3 frontend may be checking such a thing.
	 *)
	types := NEW(REF ARRAY OF Type.T,
		     SeqM3AST_AS_M3TYPE.Length(t.as_indextype_s));
	
	(* Fill-in each element of IDX_TYPES with the Type.Ordinal
	 *  corresponding to M3TYPE.
	 *)
	iter := SeqM3AST_AS_M3TYPE.NewIter(t.as_indextype_s);
	WHILE SeqM3AST_AS_M3TYPE.Next(iter, m3type) DO
	  types[i] := InternType(h, m3type, attr);
	  INC(i);
	END;
	IF i > 0 THEN
	  (* Concrete array *)
	  idxTypes := NEW(REF ARRAY OF Type.Ordinal, NUMBER(types^));
	  FOR i := 0 TO LAST(types^) DO
	    idxTypes[i] := NARROW(types[i], Type.Ordinal);
	  END;
	  arrayType := NEW(Type.Array, index := idxTypes,
			   element := InternType(h, t.as_elementtype, attr));
	  arrayType.init(t, srcPos);
	ELSE
	  (* Open array *)
	  arrayType := NEW(Type.OpenArray,
			   nDims := 1, (* XXX don't support mult. dims*)
			   target := InternType(h, t.as_elementtype, attr));
	  arrayType.init(t, srcPos);

	  (* The size of open array is unknown, but at least we know that
	   *  it doesn't fit in a register. Recall that we use
	   *  call-by-value-result protocol for a VAR-parameter that fits 
	   *  in a register. To prevent that optimization, we assign some
	   *  random 'large' value for the size.
	   *)
	  arrayType.bitsize := 99;
	  arrayType.align := BITSIZE(INTEGER);
	END;
	EVAL h.typeTable.put(t, arrayType);
	retval := arrayType;
      END;

    | M3AST_AS.Subrange_type(t) =>
      VAR
	min, max: INTEGER;
	expMin, expMax : M3AST_AS.EXP;
	baseType, subrangeType : Type.T;
      BEGIN
	baseType := InternType(h, t.sm_base_type_spec, attr);
	expMin := NARROW(t.as_range, M3AST_AS.Range).as_exp1;
	expMax := NARROW(t.as_range,  M3AST_AS.Range).as_exp2;
	EVAL M3CBackEnd.Ord(expMin.sm_exp_value, min);
	EVAL M3CBackEnd.Ord(expMax.sm_exp_value, max);
	
	subrangeType := NEW(Type.Subrange, base := baseType,
			    allowInlinePackingP := TRUE,
			    min := min, max := max);
	subrangeType.init(t, srcPos);
	EVAL h.typeTable.put(t, subrangeType);
	retval := subrangeType;
      END;
    | M3AST_AS.Record_type (t) =>
      VAR recordType : Type.Record;
      BEGIN
	recordType := NEW(Type.Record,
			  fields := InternSeqField(h, t.as_fields_s));
	recordType.init(t, srcPos);
	EVAL h.typeTable.put(t, recordType);
	retval := recordType;
      END;
    |  M3AST_AS.Enumeration_type (t) =>
      VAR enumType := NEW(Type.Enum, values := NEW(REF ARRAY OF TEXT,
						   t.sm_num_elements),
			  allowInlinePackingP := TRUE);
	iter := SeqM3AST_AS_Enum_id.NewIter(t.as_id_s);
	elem : M3AST_AS.Enum_id; 
      BEGIN
	FOR i := 0 TO t.sm_num_elements-1 DO
	  EVAL SeqM3AST_AS_Enum_id.Next(iter, elem);
	  enumType.values[i] := M3CId.ToText(elem.lx_symrep);
	END;
	enumType.init(t, srcPos);
	EVAL h.typeTable.put(t, enumType);
	retval := enumType;
      END;
    | M3AST_AS.Set_type (t) =>
      VAR setType := NEW(Type.Set, range :=
			 InternType(h, t.as_type, attr));
      BEGIN
	setType.init(t, srcPos);
	EVAL h.typeTable.put(t, setType);
	retval := setType;
      END;
    | M3AST_AS.Ref_type(t) =>
      VAR refType : Type.Ref;
      BEGIN
	IF NOT ISTYPE(t.as_trace_mode, NULL) THEN
	  Msg.Error(srcPos, "You can't use untraced references.\n");
	END;
	
	refType := NEW(Type.Ref);
	refType.init(t, srcPos);
	EVAL h.typeTable.put(t, refType);

	(* For some reason, if Ref is a member of some record and
	 * it points to the record itself, the TYPE_SPEC object is
	 * not shared, and different instances are created.
	 * If we just intern a Type object using InternType,
	 * the original record and the one pionted to by this REF
	 * may occupy different place in the typeTable. But we want
	 * to make sure that these two are really same types.
	 *
	 * So, what we do here is to look the "type name" of the type
	 * this REF points to, and it is non-NIL, fill in the name of 
	 * its type manually. what a hack!
	 * 
	 *)
	refType.target := InternType(h, t.as_type, attr);
	
	WITH name = GetTypeAlias(t.as_type) DO
	  IF name # NIL AND refType.target.alias = NIL THEN
	    refType.target.alias := name;
	  END;
	END;

	(* Special case. Open array is dereferenced *)
	TYPECASE refType.target OF
	| Type.OpenArray(a) => 
	  VAR x := NEW(Type.RefArray,
		       nDims := a.nDims,
		       target := a.target);
	  BEGIN 
	    x.init(t.as_type, srcPos);
	    EVAL h.typeTable.put(t, x);
	    retval := x;
	  END;
	ELSE 
	  refType.branded := GetBrand(t, refType.brand);
	  retval := refType;
	END;

      END;
    | M3AST_AS.Object_type(t) =>
      VAR
	o := NEW(Type.Object);
	methodIter := SeqM3AST_AS_Method.NewIter(t.as_method_s);
	(*overrideIter := SeqM3AST_AS_Override.NewIter(t.as_override_s);*)
	method : M3AST_AS.Method;
	i : CARDINAL;
      BEGIN
	o.init(t, srcPos);
	EVAL h.typeTable.put(t, o);
	IF t.as_ancestor # NIL THEN
	  o.ancestor := GetTypeAlias(t.as_ancestor);
	  IF o.ancestor = NIL THEN
	    Msg.Error(srcPos, "parent type doesn't have name???\n");
	  END;
	END;
	IF SeqM3AST_AS_Fields.Length(t.as_fields_s) > 0 THEN
	  Msg.Error(srcPos, "I don't understand fields in objects.\n");
	END;
	o.methods := NEW(REF ARRAY OF Type.Field,
			 SeqM3AST_AS_Method.Length(t.as_method_s));
	i := 0;
	WHILE SeqM3AST_AS_Method.Next(methodIter, method) DO
	  o.methods[i].name := M3CId.ToText(method.as_id.lx_symrep);
(*	  o.methods[i].type := InternType(h, method.as_type); *)
	  (* XXX this causes spuriout exceptions to be added to
	     the module. need to be fixed. *)
	  INC(i);
	END;
	
	(* XXX ignoring overrides for the time being *)
	retval := o;
      END;
      
    | M3AST_AS.Procedure_type(t) =>
      VAR
	p := InternProc(h, t.as_formal_param_s, t.as_result_type, t.as_raises);
      BEGIN
	EVAL h.typeTable.put(t, p);
	retval := p;
      END;
    | M3AST_AS.Packed_type(t) =>
      VAR
	size : INTEGER;
	pt, base : Type.T;
      BEGIN
	EVAL M3CBackEnd.Ord(t.as_exp.sm_exp_value, size);
	base := InternType(h, t.as_type);
	IF base.bitsize # size THEN
	  Msg.Error(srcPos, "sieg doesn't handle handle packed data types\n");
	  Msg.Error(srcPos, "whose base type size is different from specified size.\n");
	  Msg.Error(srcPos, "I pretend they are same.\n");
	  size := base.bitsize;
	END;

	IF ISTYPE(base, Type.Subrange) THEN
	  pt := NEW(Type.ViewableOrd,
		    allowInlinePackingP := TRUE);
	ELSE
	  (* XXX this type of BIT is not quite supported really *)
	  pt := NEW(Type.Packed, size := size,
		    allowInlinePackingP := TRUE,
		    base := InternType(h, t.as_type));
	END;
	pt.init(t, srcPos);
	EVAL h.typeTable.put(t, pt);
	retval := pt;
      END;
    ELSE
      (* Output a message saying this type is not supported, and die.
       *)
      VAR name := GetTypeAlias(ts);
      BEGIN
	IF name = NIL THEN name := "unknown"; END;
	Msg.Fatal(srcPos, " : ", typeName, "; is not supported.\n");
      END;
    END;
  END;
  
  
  IF retval.alias = NIL AND typeName # NIL
    AND (ISTYPE(ts, M3AST_AS.BRANDED_TYPE)
	 OR ISTYPE(ts, M3AST_AS.Opaque_type)) THEN
    retval.alias := typeName;
  END;

  IF Attr.Ctext IN attr.a THEN
    IF ISTYPE(retval, Type.Text) THEN
      RETURN Type.ctext;
    ELSIF ISTYPE(retval, Type.OpenArray) THEN
      RETURN Type.carrayOfChar;
    ELSE
      (* This is simply ignored. This happens when
	 <*AS CTEXT*> appears at the top of a record, and the record
	 contains something other than TEXT or ARRAY OF CHAR.
	 This is not an error.
      *)
      RETURN retval;
    END;
  ELSIF Attr.UserTypeName IN attr.a THEN
    IF (ISTYPE(retval, Type.Ordinal)
	AND retval.bitsize = BITSIZE(Word.T)) THEN
      VAR
	m3ArgTemplate, cArgTemplate : TEXT := NIL;
	i : INTEGER;
      BEGIN
	(* split userTypeName at ":". The 1st half is used as
	 *  m3 template, and the latter half is used as C template if
	 *  exists.
	 *)
	i := Text.FindChar(attr.userTypeName, '!');
	IF i >= 0 THEN
	  m3ArgTemplate := Text.Sub(attr.userTypeName, 0, i);
	  cArgTemplate := Text.Sub(attr.userTypeName,
				   i+1, Text.Length(attr.userTypeName)-i-1);
	ELSE
	  m3ArgTemplate := attr.userTypeName;
	END;
	
	RETURN NEW(Type.RawWord,
		   m3ArgTemplate := m3ArgTemplate,
		   cArgTemplate := cArgTemplate);
      END;
    ELSE
      Msg.Fatal(srcPos, " : ", "AS can't be used with non-ordinal type.\n");
    END;
  END;
  
  RETURN retval;
END InternType;

PROCEDURE InternProc (h : Handle;
		      asParams : SeqM3AST_AS_Formal_param.T;
		      asRettype : M3AST_AS.M3TYPE_NULL;
		      asRaises : M3AST_AS.RAISEES_NULL) : Type.Proc =
VAR 
  retType : Type.T := NIL; (* return type. NIL if it doens't return value. *)
  nParams : CARDINAL;
  params : REF ARRAY OF Type.Param; (* formal params. *)
  param : M3AST_AS.Formal_param;
  iter: SeqM3AST_AS_Formal_param.Iter; (* iterate over paramList *)
  needTrampolineCode := FALSE;
  raisesList := NEW(TextRefTbl.Default).init();
BEGIN
  IF asRettype # NIL THEN
    retType := InternType(h, asRettype);
    retType.externalize();
    IF ISTYPE(retType, Type.Object) THEN
      needTrampolineCode := TRUE;
    END;
  END;

  (* First, roam over the formal params to get total # of them,
   *  because I have to know that number in order to allocate memory
   *  for PARAMS *)
  nParams := 0;
  iter := SeqM3AST_AS_Formal_param.NewIter(asParams);
  WHILE SeqM3AST_AS_Formal_param.Next(iter, param) DO
    INC(nParams, SeqM3AST_AS_FORMAL_ID.Length(param.as_id_s));
  END;
  
  params := NEW(REF ARRAY OF Type.Param, nParams);
  
  (* Re-scan over formal params to fill in PARAMS. *)
  nParams := 0;
  iter := SeqM3AST_AS_Formal_param.NewIter(asParams);
  WHILE SeqM3AST_AS_Formal_param.Next(iter, param) DO
    VAR var : M3AST_AS.FORMAL_ID;
      varIter : SeqM3AST_AS_FORMAL_ID.Iter;
      ts : M3AST_AS.M3TYPE;
      type : Type.T;
      default : TEXT := NIL;
    BEGIN
      IF param.as_formal_type # NIL THEN
	ts := param.as_formal_type;
      ELSIF param.as_default # NIL THEN
	ts := param.as_default.sm_exp_type_spec;
      END;
      IF ts = NIL THEN
	Msg.Fatal(Msg.POS_VOID, "I don't know the type of this expression.");
      END;

      
      type := InternType(h, ts, ParseTypeAttr(h.m, param));
	
      IF param.as_default # NIL THEN
	default := UncompileConstant(param.as_default);
      END;
      
      varIter := SeqM3AST_AS_FORMAL_ID.NewIter(param.as_id_s);
      WHILE SeqM3AST_AS_FORMAL_ID.Next(varIter, var) DO
	WITH f = params[nParams] DO
	  f.name := M3CId.ToText(var.lx_symrep);
	  f.type := type;
	  f.default := default;
	  TYPECASE var OF
	  | M3AST_AS.F_Value_id => f.mode := Type.Mode.Value;
	  | M3AST_AS.F_Var_id =>
	    f.type.externalize();
	    IF ISTYPE(f.type, Type.Object) THEN
	      needTrampolineCode := TRUE;
	    END;
	    IF Pragma.Find(h.m, param, "OUT") # NIL THEN
	      f.mode := Type.Mode.Out;
	    ELSE 
	      f.mode := Type.Mode.InOut;
	    END;
	  | M3AST_AS.F_Readonly_id =>
	    f.mode := Type.Mode.In;
	  ELSE
	    Msg.Fatal(Msg.POS_VOID, "I don't know the attribute of ",
		      "the formal param ", f.name, ".\n");
	  END;

	  (* XXX hack hack *)
	  IF type = Type.carrayOfChar THEN
	    f.mode := Type.Mode.Value;
	  END;
	  
	END;
	INC(nParams);
      END;
    END;
  END;

  IF asRaises # NIL THEN
    WITH r = NARROW(asRaises, M3AST_AS.Raisees_some) DO 
      VAR
	itr := SeqM3AST_AS_Qual_used_id.NewIter(r.as_raisees_s);
	id : M3AST_AS.Qual_used_id;
	exc : TEXT;
      BEGIN
	WHILE SeqM3AST_AS_Qual_used_id.Next(itr, id) DO 
	  exc := QualIDToText(id);
	  EVAL raisesList.put(exc, NIL);

	  (* register the exception in the module *)
	  EVAL h.m.exceptionList.put(exc, NIL);
	END;
      END;
    END;
  END;
	
  RETURN NEW(Type.Proc, retType := retType, params := params,
	     needTrampolineCode := needTrampolineCode,
	     raises := raisesList);
  
END InternProc;
		     
(*
 Return the value(as a string) represented by EXP.
 EXP must be a constant expression.

 XXX : Doesn't support constants of non-primitive types.
 *)
PROCEDURE UncompileConstant(exp : M3AST_SM_F.EXP) : TEXT =
BEGIN
  TYPECASE exp.sm_exp_value OF
  | M3CBackEnd_C.Integer_value(int) =>
    RETURN Fmt.Int(int.sm_value);
  (* texts are done lower with quotes and we want the quotes.
  | M3CBackEnd_C.Text_value(txt) => 
    RETURN txt.sm_value;
  *)
  | M3CBackEnd_C.Real_value(real) => 
    RETURN Fmt.Real(real.sm_value);
  | M3CBackEnd_C.LongReal_value(lreal) => 
    RETURN Fmt.LongReal(lreal.sm_value);
  | M3CBackEnd_C.Extended_value(ereal) => 
    RETURN Fmt.Extended(ereal.sm_value);
  | M3CBackEnd_C.Text_value(t) =>
    RETURN "\"" & t.sm_value & "\"";
    (* XXX I don't know what happens if t.sm_value contains dquote. *)
  ELSE
    VAR (*ts : M3AST_TM_F.TYPE_SPEC := exp;*)
      srcPos := Msg.Pos{pos := exp.lx_srcpos, cu := NIL};
    BEGIN
      Msg.Fatal(srcPos, "constant of unknown type.\n");
      RETURN NIL;
    END;
  END;
END UncompileConstant;

PROCEDURE ProcessConstDecl(h : Handle; c : M3AST_AS.Const_decl) =
  VAR
    name : TEXT;
    value : TEXT;
  BEGIN
    name := M3CId.ToText(c.as_id.lx_symrep);
    value := UncompileConstant(c.as_exp);
    h.m.names.addhi(NEW(Declaration.Const, name := name, value := value));
  END ProcessConstDecl;

PROCEDURE ProcessTypeDecl (h : Handle; p : M3AST_AS.TYPE_DECL) =
VAR name : TEXT;
  type : Type.T;
BEGIN
		
  name := M3CId.ToText(p.as_id.lx_symrep);
  type := InternType(h, p.as_type);
  
  type.alias := name;
  h.m.names.addhi(NEW(Declaration.Type, name := name, type := type));
END ProcessTypeDecl;

PROCEDURE ProcessProcDecl (h : Handle; p : M3AST_AS.Proc_decl) =
VAR
  decl := NEW(Declaration.Proc);
  
  synonyms : TEXT;
BEGIN
  decl.name := M3CId.ToText(p.as_id.lx_symrep);
  decl.proc := InternProc(h, p.as_type.as_formal_param_s,
			  p.as_type.as_result_type,
			  p.as_type.as_raises);
  decl.proc.srcPos.pos := p.lx_srcpos;

  (* Assign the systemcall # *)
  decl.id := h.procId;
  IF h.m.descendingProcIDs THEN
    DEC(h.procId);
  ELSE
    INC(h.procId);
  END;

  synonyms := Pragma.Find(h.m, p, "SYNONYMS");
  IF synonyms # NIL THEN
    VAR i := 0;
      comma : INTEGER;
    BEGIN
      (* Split the "synonym" at each comma, and store them into
         proc.userSideName. *)
      LOOP 
	comma := Text.FindChar(synonyms, ',');
	IF comma < 0 THEN
	  decl.userSideName[i] := synonyms;
	  EXIT;
	ELSE
	  decl.userSideName[i] := Text.Sub(synonyms, 0, comma);
	  INC(i);
	  INC(comma);

	  (* skip whitespaces *)
	  WHILE comma < Text.Length(synonyms)
	        AND Text.GetChar(synonyms, comma) = ' ' DO
	    INC(comma);
	  END;
	  synonyms := Text.Sub(synonyms, comma);
	END;
      END;
    END;
  END;

  
  h.m.names.addhi(decl);
END ProcessProcDecl;

PROCEDURE ParseSkipPragma(skip: TEXT; VAR fromID, toID: INTEGER) =
  VAR
    fromText, toText: TEXT;
    skipLen: CARDINAL;
    i: CARDINAL := 0;
  BEGIN
    TRY
      skipLen := Text.Length(skip);
      (* skip has the format <*SKIP from..to*> *)
      WHILE i < skipLen DO
	(* Look for first ' ' or '.' and extract the from value *)
	WITH ch = Text.GetChar(skip, i) DO
	  IF ch = '.' OR ch = ' ' THEN
	    fromText := Text.Sub(skip, 0, i);
	    EXIT;
	  END;
	END;
	INC(i);
      END;
      IF fromText = NIL THEN
	Msg.Fatal(Msg.POS_VOID, "Wrong skip usage <*SKIP from..to>\n");
      END;
      
      fromID := Scan.Int(fromText);
    
      (* Skip spaces before '.' *)
      WHILE i < skipLen AND Text.GetChar(skip, i) = ' ' DO
	INC(i);
      END;
    
      (* Skip two '.' s *)
      INC(i, 2);
      
      (* Skip spaces after '.' *)
      WHILE i < skipLen AND Text.GetChar(skip, i) = ' ' DO
	INC(i);
      END;
		
      IF i >= skipLen THEN
	Msg.Fatal(Msg.POS_VOID, "No TO value in <*SKIP from..to>\n");
      END;
      toText := Text.Sub(skip, i);
      toID := Scan.Int(toText);
    EXCEPT
    | Lex.Error, FloatMode.Trap =>
      Msg.Fatal(Msg.POS_VOID, "Malformed <*SKIP from..to>\n");
    END;
  END ParseSkipPragma;

PROCEDURE Node (h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode)=
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      TYPECASE n OF
      | M3AST_AS.Compilation_Unit(p) =>
	
	M3Error.SetCu(p);
	h.m.pragmas := p.lx_pragmas;
	h.procId := 0;

	VAR 
	  procBase := Pragma.Find(h.m, NIL, "INTERFACE_PROC_BASE");
	  branchOn := Pragma.Find(h.m, NIL, "EPILOG_BRANCH_ON_REGISTER");
	  descendingProcIDs := Pragma.Find(h.m, NIL, "DESCENDING_PROC_IDS");
	  noVarOpt := Pragma.Find(h.m, NIL, "NO_SMALL_VAR_OPTIMIZATION");
	BEGIN
	  TRY 
	    IF procBase # NIL THEN 
	      h.procId := Scan.Int(procBase);
	    END;
	  EXCEPT
	  ELSE
	  END;
	  
	  IF branchOn # NIL THEN
	    h.m.errorFunction := branchOn;
	  ELSE
	    h.m.errorFunction := NIL;
	  END;

	  h.m.descendingProcIDs := descendingProcIDs # NIL;
	  h.m.noVarOptimization := noVarOpt # NIL;

	  h.m.afterHook := Pragma.Find(h.m, NIL, "AFTER_SYSCALL_HOOK");
	  
	  M3Error.SetCu(p);
	  h.m.pragmas := p.lx_pragmas;
	  h.procId := 0;
	  
	  IF procBase # NIL THEN
	    TRY 
	      h.procId := Scan.Int(procBase);
	    EXCEPT
	    ELSE
	      Msg.Fatal(Msg.POS_VOID,
			"INTERFACE_PROC_BASE is not an integer.\n");
	    END;
	  END;
	    
	  IF branchOn # NIL THEN
	    h.m.errorFunction := branchOn;
	  ELSE
	    h.m.errorFunction := NIL;
	  END;
	    
	  IF h.procId = 0 THEN
	    (* Apply the hash function on the interface name and use it
	     as the ID *)
	    h.procId := Text.Hash(h.m.intf);
	    (* Kind of paranoid, but make sure that procId doesn't
	     overflow. 1000 is a large enough number for a 
	     # of procs in a interface.
	     *)
	    h.procId := h.procId MOD (LAST(CARDINAL) - 1000);
	    (* --- But, we still stick to 777 thing for the time being. *)
	    h.procId := 777;
	  END;
	  Msg.Verbose(Msg.POS_VOID, "Starting proc ID from ",
		      Fmt.Int(h.procId), ".\n");
	END;
      | M3AST_AS.UNIT_NORMAL(p) =>
	VAR 
	  itr := SeqM3AST_AS_IMPORTED.NewIter(p.as_import_s);
	  module : M3AST_AS.IMPORTED;
	BEGIN
	  WHILE SeqM3AST_AS_IMPORTED.Next(itr, module) DO
	    TYPECASE module OF
	    | M3AST_AS.Simple_import(s) =>
	      VAR
		intfItr := SeqM3AST_AS_Import_item.NewIter(s.as_import_item_s);
		intf : M3AST_AS.Import_item;
	      BEGIN
		WHILE  SeqM3AST_AS_Import_item.Next(intfItr, intf) DO
		  h.m.imports.addhi(M3CId.ToText(intf.as_intf_id.lx_symrep));
		END;
	      END;
	    ELSE
	      Msg.Fatal(Msg.POS_VOID,
			"I don't understand FROM..IMPORT or IMPORT..AS.\n");
	    END;
	  END;
	END;
      | M3AST_AS.Const_decl(p) => ProcessConstDecl(h, p);
      | M3AST_AS.TYPE_DECL(p) => ProcessTypeDecl(h, p);
      | M3AST_AS.Proc_decl(p) =>
	(* Check <*SKIP*> and <*PROCID*> pragma here *)
	VAR
	  procId := Pragma.Find(h.m, p, "PROCID");
	  skip := Pragma.Find(h.m, p, "SKIP");
	  fromId, toId: INTEGER;
	BEGIN
	  IF procId # NIL THEN
	    TRY
	      fromId := Scan.Int(procId);
	    EXCEPT
	    | Lex.Error, FloatMode.Trap =>
	      Msg.Fatal(Msg.POS_VOID, "<*PROCID*> param is not numeral.");
	    END;
	    h.procId := fromId;
	  ELSIF skip # NIL THEN
	    ParseSkipPragma(skip, fromId, toId);
	    IF h.procId # fromId THEN
	      Msg.Error(Msg.POS_VOID, skip, ": FROM value wrong : ",
			"it should be ", Fmt.Int(h.procId), ".\n");
	    ELSE
	      IF h.m.descendingProcIDs THEN
		h.procId := toId - 1;
	      ELSE
		h.procId := toId + 1;
	      END;
	    END;
	  END;
	  ProcessProcDecl(h, p);
	END;
      | M3AST_AS.Var_decl =>
	Msg.Fatal(Msg.POS_VOID, "variable declaration is not supported.");
      ELSE
      END;
    END;
  END Node;


BEGIN
END Pass1.



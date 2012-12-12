(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Put in hack for x86 compiler in ToTextSubrange.
 *
 * 01-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Ref array and var array unpacking became efficient.
 * 26-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	unified unpackFrom{Reg,Mem} into unpack by creating ParamAddr object.
 * 09-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	added enum, set, reference support.
 * 03-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created. 
 *)

MODULE Type;
IMPORT M3AST_SM_F;
IMPORT Fmt, IWr, Msg, Word;
IMPORT Target;
IMPORT TypeTable, M3CStdTypes;
IMPORT ParamAddr;

(*
 *  Generic Routines.
 *)

REVEAL T = TPublic BRANDED OBJECT
    name: TEXT; (* canonical name for the type. This is non-NIL only when
		 T is some reserved type such as INTEGER or CARDINAL. *)
OVERRIDES
  toText := ToText;
  declareVar := DeclareVar;
  toCText := ToCText;
  pack := Pack;
  unpack := Unpack;
  viewable := True;
  needSpace := False;
  externalize := Externalize;
  exceptionOnUnpack := NoException;
  exceptionOnPack := NoException;
  instanceName := InstanceName;
  init := Init;
  destruct := Destruct;
END;

PROCEDURE NoException (<*UNUSED*>t : T) : REF ARRAY OF TEXT =
  BEGIN
    RETURN NIL;
  END NoException;
  
PROCEDURE BitsToBytes (v : INTEGER) : INTEGER =
BEGIN
  RETURN (v-1) DIV 8 + 1;
END BitsToBytes ;

PROCEDURE Roundup (v, align : INTEGER) : INTEGER =
BEGIN
  IF v < 0 THEN
    Msg.Fatal(Msg.POS_VOID, "roundup arg < 0");
  END;
  RETURN ((v-1) DIV align + 1) * align;
END Roundup;

PROCEDURE Align (t : T; VAR addr : INTEGER) =
BEGIN
  addr := Roundup(addr, BitsToBytes(t.align));
END Align;

PROCEDURE TryPack (t : T; var : TEXT; addr : ParamAddr.T; wr : IWr.T)
: BOOLEAN =
BEGIN
  (* If the type does not contain reference anywhere, we can write
   *  the value to the user space at once.
   *)
  IF t.viewable() THEN
    addr.write(var, t, wr);
    RETURN TRUE;
  END;
  RETURN FALSE;
END TryPack;
  
PROCEDURE Pack (t : T; var: TEXT; addr : ParamAddr.T; wr : IWr.T) =
BEGIN
  IF NOT TryPack(t, var, addr, wr) THEN
    Msg.Error(t.srcPos, "Can't pack "&var&".\n");
  END;
END Pack;

PROCEDURE Destruct (<*UNUSED*>t : T; <*UNUSED*>var: TEXT;
		    <*UNUSED*>wr : IWr.T) =
  BEGIN
    (* nothing to do *)
  END Destruct;
  
PROCEDURE TryUnpack (t : T; var : TEXT; addr : ParamAddr.T; wr : IWr.T)
  : BOOLEAN =
  BEGIN
    (* If the type does not contain reference anywhere, we can read
       the value from the user space at once. *)
    IF t.viewable() THEN
      addr.read(var, t, wr);
      RETURN TRUE;
    END;
    RETURN FALSE;
  END TryUnpack;
  
PROCEDURE Unpack (t : T; var : TEXT; addr : ParamAddr.T; wr : IWr.T) =
  BEGIN
    IF NOT TryUnpack(t, var, addr, wr) THEN
      Msg.Error(t.srcPos, "Can't unpack "&var&".\n");
    END;
  END Unpack;
    
PROCEDURE ToText (t : T; allowAlias := TRUE;
		  <*UNUSED*>extensionCode := TRUE) : TEXT =
  BEGIN
    IF allowAlias AND t.alias # NIL THEN
      RETURN t.alias;
    END;
    
    IF t.name # NIL THEN
      RETURN t.name;
    END;
    RETURN NIL;
  END ToText;

PROCEDURE InstanceName(<*UNUSED*>t : T; name : TEXT) : TEXT =
  BEGIN
    RETURN name;
  END InstanceName;
  
PROCEDURE DeclareVar (t : T; name : TEXT; allowAlias := TRUE;
		      extensionCode := TRUE) : TEXT =
BEGIN
  RETURN name & " : " & t.toText(allowAlias, extensionCode);
END DeclareVar;

PROCEDURE ToCText(t : T; name : TEXT; allowAlias : BOOLEAN) : TEXT =

BEGIN
  IF allowAlias AND t.alias # NIL THEN
    RETURN t.alias & " " & name;
  END;
  (* We can't use t.name because it is M3 type name, not C name.
   *  We use specialized handler to convert myself *)
  RETURN NIL;    
END ToCText;

PROCEDURE Externalize (<*UNUSED*>t : T) =
  BEGIN
  END Externalize;
  

PROCEDURE True (<*UNUSED*>t : T) : BOOLEAN =
  BEGIN
    RETURN TRUE;
  END True;
  
PROCEDURE False (<*UNUSED*>t : T) : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END False;
  

PROCEDURE Init (t : T; ts : M3AST_SM_F.TYPE_SPEC; READONLY pos : Msg.Pos) =
BEGIN
  t.bitsize := ts.sm_bitsize;
  t.align := ts.sm_align;
  t.srcPos := pos;
END Init;

(*
 *  Arrays
 *)
REVEAL Array = ArrayPublic BRANDED OBJECT
OVERRIDES
  toText := ToTextArray;
  toCText := ToCTextArray;
  viewable := ViewableArray;
  needSpace := NeedSpaceArray;
  exceptionOnUnpack := SpaceExceptionIfSpaceIsUsed;
  exceptionOnPack := SpaceExceptionIfSpaceIsUsed;
  externalize := ExternalizeArray;
END;

PROCEDURE ToTextArray (t : Array; allowAlias, extensionCode : BOOLEAN) : TEXT =
VAR g := ToText(t, allowAlias);
  idx := "";
BEGIN
  IF g # NIL THEN RETURN g; END;
  
  FOR i := 0 TO LAST(t.index^) DO
    IF i > 0 THEN idx := idx & ","; END;
    idx := idx & t.index[0].toText();
  END;
  
  RETURN "ARRAY "&idx&" OF " &t.element.toText(TRUE, extensionCode);
END ToTextArray;

PROCEDURE ToCTextArray (t : Array; name : TEXT; allowAlias : BOOLEAN) : TEXT =
VAR g := ToCText(t, name, allowAlias);
  idx := "";
  range : TEXT;
BEGIN
  IF g # NIL THEN RETURN g; END;

  FOR i := 0 TO LAST(t.index^) DO
    TYPECASE t.index[0] OF
    | Subrange(st) =>
      range := Fmt.Int(st.max-st.min+1);
    ELSE
      Msg.Error(t.srcPos, t.toText(), " can't be expressed in C.\n");
      RETURN NIL;
    END;
    idx := idx & "[" & range & "]";
  END;
  
  RETURN t.element.toCText("") & " " & name & idx;
END ToCTextArray;

PROCEDURE ViewableArray (t : Array) : BOOLEAN =
BEGIN
  RETURN t.element.viewable();
END ViewableArray;
PROCEDURE NeedSpaceArray (t : Array) : BOOLEAN =
BEGIN
  RETURN t.element.needSpace();
END NeedSpaceArray;

VAR SpaceExceptionList : REF ARRAY OF TEXT;
				
PROCEDURE SpaceExceptionIfSpaceIsUsed (t : T) : REF ARRAY OF TEXT =
  BEGIN
    IF t.needSpace() THEN
      RETURN SpaceExceptionList;
    ELSE
      RETURN NIL;
    END;
  END SpaceExceptionIfSpaceIsUsed;

PROCEDURE ExternalizeArray(t : Array) =
BEGIN
  t.element.externalize();
END ExternalizeArray;

(*
 *  Open Arrays
 *)
  
REVEAL OpenArray = OpenArrayPublic BRANDED OBJECT
OVERRIDES
  instanceName := InstanceNameOpenArray;
  toText := ToTextOpenArray;
  toCText := ToCTextOpenArray;
  unpack := UnpackOpenArray;
  pack := PackOpenArray;
  viewable := False;
  exceptionOnUnpack := SpaceExceptionIfSpaceIsUsed;
  exceptionOnPack := SpaceExceptionIfSpaceIsUsed;
  (* XXX I'm ignoring VAR x : ARRAY OF REF xxx *)
END;

REVEAL RefArray = OpenArrayPublic BRANDED OBJECT
OVERRIDES
  toText := ToTextRefArray;
  toCText := ToCTextOpenArray;
  unpack := UnpackRefArray;
  pack := PackRefArray;
  viewable := False;
  needSpace := True;
  exceptionOnUnpack := SpaceExceptionIfSpaceIsUsed;
  exceptionOnPack := SpaceExceptionIfSpaceIsUsed;
  externalize := ExternalizeRef;
END;

PROCEDURE InstanceNameOpenArray (<*UNUSED*>t : OpenArrayPublic; name : TEXT)
  : TEXT =
  BEGIN
    RETURN name & "^";
  END InstanceNameOpenArray;
  
PROCEDURE ToTextOpenArray (t : OpenArrayPublic;
			   allowAlias, extensionCode : BOOLEAN)
: TEXT =
VAR g := ToText(t, allowAlias);
BEGIN
  IF g # NIL THEN RETURN g; END;
  IF extensionCode THEN 
    RETURN "REF ARRAY OF " & t.target.toText(TRUE, extensionCode);
  ELSE
    RETURN "ARRAY OF " & t.target.toText(TRUE, extensionCode);
  END;
END ToTextOpenArray;

PROCEDURE ToTextRefArray (t : OpenArrayPublic;
			  allowAlias, extensionCode : BOOLEAN)
: TEXT =
VAR g := ToText(t, allowAlias);
BEGIN
  IF g # NIL THEN RETURN g; END;
  RETURN "REF ARRAY OF " & t.target.toText(TRUE, extensionCode);
END ToTextRefArray;

PROCEDURE ToCTextOpenArray (t : OpenArrayPublic;
			    name : TEXT; allowAlias : BOOLEAN)
: TEXT =
VAR g := ToCText(t, name, allowAlias);
BEGIN
  IF g # NIL THEN RETURN g; END;

  RETURN t.target.toCText("") & " *" & name;
END ToCTextOpenArray;

(* Ref array and open array are remarkably similar.

 The differences are:
 * ref array might be NIL.
 * ref array might be an externalized reference.

 They seems measle, so we treat them together.
 *)
PROCEDURE UnpackRefOrOpenArray (t : OpenArrayPublic;
				var : TEXT;
				addr : ParamAddr.T;
				wr : IWr.T;
				refArrayP : BOOLEAN) =
VAR dimVar : TEXT;
BEGIN
  IF TryUnpack(t, var, addr, wr) THEN RETURN; END;

  (* Ref array might be an externalized reference. Check it *)
  IF refArrayP THEN
    WITH r = NARROW(t, RefArray) DO
      <*ASSERT r.target # NIL *>
      IF r.capabilityP THEN
	UnpackRef(t, var, addr, wr);
	RETURN;
      END;
    END;
  END;
  
  wr.put("VAR shape_ : ARRAY [0..", Fmt.Int(t.nDims), "] OF Word.T;\n");
  wr.put("    addr_ : Word.T;\n");
  addr.read("tmp_", integer, wr);
  wr.put("BEGIN\n");
  wr.indent();

  (* Only refarray can be NIL. if var array is nil, it's an runtime error,
   and we don't have to check it.*)
  IF refArrayP THEN 
    wr.put("IF tmp_ # 0 THEN\n");
    wr.indent();
  END;

  (* shape_[0] holds the ptr to data.
   *  shape_[1..] holds the dimensions.
   *)
  ParamAddr.User("tmp_").readDyn("shape_",
				 Fmt.Int((t.nDims+1)*BYTESIZE(Word.T)), wr);
  wr.put(var, " := NEW(", t.toText(), ", shape_[1]);\n");
  
  IF t.nDims > 1 THEN
    wr.put("FOR d_ := 1 TO ", Fmt.Int(t.nDims), " DO \n");
    wr.indent();
    dimVar := "d_";
  ELSE
    dimVar := "1";
  END;

  wr.put("addr_ := shape_[0];\n");
  
  IF t.target = char THEN
    WITH arraySize = Fmt.F("shape_[%s]*BYTESIZE(%s[0])", dimVar, var) DO 
      wr.put("Translation.Read(space_, addr_, ", var & "^, ", arraySize, ");\n");
      wr.put("INC(addr_, ", arraySize, ");\n");
    END;
  ELSE 
    wr.put(Fmt.F("FOR i_ := 0 TO shape_[%s] DO\n", dimVar));
    wr.indent();
    t.target.unpack(var&"[i_]", ParamAddr.User("addr_"), wr);
    wr.put("INC(addr_, BYTESIZE(", var, "[0]));\n");
    
    wr.unindent();
    wr.put("END;\n");
  END;
  IF t.nDims > 1 THEN wr.unindent(); wr.put("END;\n"); END;
  
  IF refArrayP THEN 
    wr.unindent(); wr.put("END;\n");
  END;
  wr.unindent(); wr.put("END;\n");
  
END UnpackRefOrOpenArray;

PROCEDURE UnpackOpenArray (t : OpenArrayPublic;
			   var : TEXT; addr : ParamAddr.T; wr : IWr.T) =
BEGIN
  UnpackRefOrOpenArray(t, var, addr, wr, FALSE);
END UnpackOpenArray;

PROCEDURE PackOpenArray (t : OpenArrayPublic;
			 var : TEXT; addr : ParamAddr.T; wr : IWr.T) =
BEGIN
  IF TryUnpack(t, var, addr, wr) THEN RETURN; END;

  wr.put("VAR shape_ : ARRAY [0..", Fmt.Int(t.nDims), "] OF Word.T;\n");
  wr.put("    tmp_ : Word.T;\n");
  wr.put("BEGIN\n");
  wr.indent();
  
  (* shape_[0] holds the ptr to data.
   *  shape_[1..] holds the dimensions.
   *)
  ParamAddr.User("tmp_").readDyn("shape_",
				 Fmt.Int((t.nDims+1)*BYTESIZE(Word.T)), wr);
  wr.put(var, " := NEW(", t.toText(), ", shape_[1]);\n");
  IF t.nDims > 1 THEN wr.put("FOR d_ := 1 TO ", Fmt.Int(t.nDims), " DO "); END;
  WITH arrayAddr = ParamAddr.User("shape_[0]+i_*BYTESIZE("& var&"[0])") DO
    IF t.target.viewable() THEN
      arrayAddr.writeDyn("ADR(" & var & "[0])", "shape_[d_]*BYTESIZE("&var&"[0])", wr);
    ELSE 
      wr.put("FOR i_ := 0 TO shape_[d_] DO\n");
      wr.indent();
      t.target.unpack(var&"[i_]", arrayAddr, wr);
      wr.unindent(); wr.put("END;\n");
    END;
  END;
  IF t.nDims > 1 THEN wr.put("END;\n"); END;
  wr.unindent(); wr.put("END;\n");
END PackOpenArray;


PROCEDURE UnpackRefArray (t : OpenArrayPublic;
			  var : TEXT; addr : ParamAddr.T; wr : IWr.T) =
BEGIN
  UnpackRefOrOpenArray(t, var, addr, wr, TRUE);
END UnpackRefArray;

PROCEDURE PackRefArray (t : OpenArrayPublic;
			 var : TEXT; addr : ParamAddr.T; wr : IWr.T) =
VAR
  dimVar : TEXT;
BEGIN
  IF TryUnpack(t, var, addr, wr) THEN RETURN; END;

  wr.put("VAR shape_ : ARRAY [0..", Fmt.Int(t.nDims+1), "] OF Word.T;\n");
  wr.put("    addr : Word.T;\n");
  wr.put("BEGIN\n");
  wr.indent();
  
  (* shape_[0] holds the ptr to data.
   *  shape_[1..] holds the dimensions.
   *)
  ParamAddr.User("tmp_").readDyn("shape_",
				 Fmt.Int((t.nDims+1)*BYTESIZE(Word.T)), wr);
  IF t.nDims > 1 THEN
    wr.put("FOR d_ := 1 TO ", Fmt.Int(t.nDims), " DO ");
    dimVar := "d_";
  ELSE
    dimVar := "1";
  END;

  wr.put("addr_ := shape_[0];\n");
  
  IF t.target = char THEN
    WITH arraySize = Fmt.F("shape_[%s]*BYTESIZE(%s[0])", dimVar, var) DO 
      wr.put("Translation.Write(space_, ", var&"^", "addr_, ", arraySize, ");\n");
      wr.put("INC(addr_, ", arraySize, ");\n");
    END;
  ELSE 
    wr.put("FOR i_ := 0 TO shape_[d_] DO\n");
    wr.indent();
    t.target.unpack(var&"[i_]", ParamAddr.User("addr_"), wr);
    wr.put("INC(addr_, BYTESIZE(", var, "[0]));\n");
    wr.unindent();
    wr.put("END;\n");
  END;
  IF t.nDims > 1 THEN wr.unindent(); wr.put("END;\n"); END;
  wr.indent();
  wr.put("END;\n");
END PackRefArray;

(*
 *  Subranges(or ordinals)
 *)
REVEAL Subrange = SubrangePublic BRANDED OBJECT
OVERRIDES
  toText := ToTextSubrange;
  toCText := ToCTextSubrange;
  viewable := ViewableSubrange; 
END;

PROCEDURE ViewableSubrange(v : Subrange) : BOOLEAN =
  BEGIN
    (* This is kind of ugly, but we check if the type is
       representation independent by enumerating all the
       possible representation independent subrange types *)
    IF v.min = 0 THEN
      IF v.max = 16_FF OR v.max = 16_FFFF
	OR v.max = 16_FFFFFFFF OR v.max = 16_FFFFFFFFFFFFFFFF THEN
	RETURN TRUE;
      ELSE
	RETURN FALSE;
      END;
    ELSIF v.min = -16_80 AND v.max = 16_7F THEN RETURN TRUE;
    ELSIF v.min = -16_8000 AND v.max = 16_7FFF THEN RETURN TRUE;
    ELSIF v.min = -16_80000000 AND v.max = 16_7FFFFFFF THEN RETURN TRUE;
    ELSIF v.min = -16_8000000000000000 AND v.max = 16_7FFFFFFFFFFFFFFF THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END ViewableSubrange;

PROCEDURE ToTextSubrange (t : Subrange;
			  allowAlias : BOOLEAN;
			  <*UNUSED*>extensionCode : BOOLEAN) : TEXT =
VAR g := ToText(t, allowAlias);
    minText : TEXT;
BEGIN
  IF g # NIL THEN RETURN g; END;
  
  (* This is a hack for the x86 compiler, which rejects subranges when *)
  (* the absolute value of the minimum or maximum element cannot be *)
  (* represented by a 32-bit integer *)
  IF t.min = FIRST(INTEGER) THEN
    minText := "(-" & Fmt.Int(LAST(INTEGER)) & "-1)";
  ELSE
    minText := Fmt.Int(t.min);
  END;

  RETURN "[" & minText &".."&Fmt.Int(t.max) & "]";
END ToTextSubrange;

PROCEDURE ToCTextSubrange (t : Subrange; name : TEXT;
			   allowAlias : BOOLEAN) : TEXT =
VAR g := ToCText(t, name, allowAlias);
BEGIN
  IF g # NIL THEN RETURN g; END;
  RETURN Target.ToCTextOrdinal(t.bitsize) & " " & name;
END ToCTextSubrange;

(*
 *  Opaques
 *)
REVEAL Opaque = OpaquePublic BRANDED OBJECT
OVERRIDES
  toText := ToTextOpaque;
  toCText := ToCTextOpaque;
  externalize := ExternalizeOpaque;
END;
PROCEDURE ExternalizeOpaque (t : Opaque) =
BEGIN
  ExternalizeRef(t);
  t.revealedPart.externalize();
END ExternalizeOpaque;

PROCEDURE ToTextOpaque (t : Opaque; allowAlias, extensionCode : BOOLEAN)
: TEXT =
VAR g := ToText(t, allowAlias);
BEGIN
  IF NOT extensionCode AND t.capabilityP THEN RETURN "Word.T"; END;
  
  IF g # NIL THEN RETURN g; END;
  RETURN t.revealedPart.toText(allowAlias, extensionCode);
END ToTextOpaque;

PROCEDURE ToCTextOpaque (t : Opaque; name : TEXT; allowAlias : BOOLEAN)
  : TEXT =
  VAR g := ToCText(t, name, allowAlias);
  BEGIN
    IF t.capabilityP THEN RETURN "long "&name; END;
    IF g # NIL THEN RETURN g; END;
    Msg.Fatal(t.srcPos, "You can't use partially opaque type ",
	      text.name, ".\n");
    RETURN NIL;
  END ToCTextOpaque;

(*
 *  Ordinals
 *)
REVEAL Ordinal = OrdinalPublic BRANDED OBJECT
OVERRIDES
  unpack := UnpackOrdinal;
  pack := PackOrdinal;
  viewable := False;
END;

PROCEDURE GetAliasForRepresentationIndependentType(bits : INTEGER)
: T =
BEGIN
  IF bits = BITSIZE(INTEGER) THEN
    RETURN integer;
  ELSE
    RETURN NEW(ViewableOrd, bitsize := bits, align := bits);
  END;
END GetAliasForRepresentationIndependentType;

PROCEDURE UnpackOrdinal (t : Ordinal; var: TEXT;
			 addr : ParamAddr.T; wr : IWr.T) =
  VAR canonicalType := GetAliasForRepresentationIndependentType(t.bitsize);
  BEGIN
    IF NOT TryUnpack(t, var, addr, wr) THEN
      wr.f("VAR %s;\nBEGIN\n", canonicalType.declareVar("tmp_"));
      addr.read("tmp_", canonicalType, wr);
      wr.f("%s := VAL(tmp_, %s);\nEND;\n", var, t.toText());
    END;
  END UnpackOrdinal;

PROCEDURE PackOrdinal (t : Ordinal; var: TEXT;
			 addr : ParamAddr.T; wr : IWr.T) =
  VAR
    bits := BitsToBytes(t.bitsize)*8;
    canonicalType := GetAliasForRepresentationIndependentType(bits);
  BEGIN
    IF NOT TryPack(t, var, addr, wr) THEN
      wr.f("VAR %s := ORD(%s);\nBEGIN\n", canonicalType.declareVar("tmp_"),
	   var);
      addr.write("tmp_", canonicalType, wr);
      wr.put("END;\n");
    END;
  END PackOrdinal;

(*
 *  Integers
 *)
REVEAL ViewableOrd = Ordinal BRANDED OBJECT
OVERRIDES
  toText := ToTextViewableOrd;
  toCText := ToCTextViewableOrd;
  unpack := Unpack;
  viewable := True;
END;

PROCEDURE ToTextViewableOrd(t : ViewableOrd;
			    <*UNUSED*>allowAlias, extensionCode : BOOLEAN)
: TEXT =
BEGIN
  WITH bits = BitsToBytes(t.bitsize)*8 DO 
    IF bits = BITSIZE(INTEGER) THEN
      RETURN "INTEGER";
    ELSE
      RETURN "BITS " & Fmt.Int(bits) & " FOR [0.." &
      	Fmt.Int(Word.Shift(1, bits)-1) & "]";
    END;
  END;
END ToTextViewableOrd;
  
PROCEDURE ToCTextViewableOrd(t : ViewableOrd; name : TEXT;
			     <*UNUSED*>allowAlias : BOOLEAN) : TEXT =
BEGIN
  RETURN Target.ToCTextOrdinal(t.bitsize) & " " & name;
END ToCTextViewableOrd;


(*
 *  Records
 *)

REVEAL Record = RecordPublic BRANDED OBJECT
OVERRIDES
  toText := ToTextRecord;
  toCText := ToCTextRecord;
  unpack := UnpackRecord;
  pack := PackRecord;
  viewable := ViewableRecord;
  needSpace:= NeedSpaceRecord;
  exceptionOnUnpack := SpaceExceptionIfSpaceIsUsed;
  exceptionOnPack := SpaceExceptionIfSpaceIsUsed;
  externalize := ExternalizeRecord;
END;

PROCEDURE UnpackRecord (t : Record; var : TEXT; addr : ParamAddr.T;
			wr : IWr.T) =
VAR offset := 0;
BEGIN
  IF NOT TryUnpack(t, var, addr, wr) THEN
    FOR i := 0 TO LAST(t.fields^) DO
      WITH f = t.fields[i] DO
	Align(f.type, offset);
	f.type.unpack(var & "." & f.name, addr.inc(offset), wr);
	INC(offset, BitsToBytes(f.type.bitsize));
      END;
    END;
  END;
END UnpackRecord;

PROCEDURE PackRecord (t : Record; var: TEXT; addr : ParamAddr.T; wr : IWr.T) =
VAR offset : INTEGER := 0;
BEGIN
  IF NOT TryPack(t, var, addr, wr) THEN
    FOR i := 0 TO LAST(t.fields^) DO
      WITH f = t.fields[i] DO
	Align(f.type, offset);
	f.type.pack(var& "." & f.name, addr.inc(offset), wr);
	INC(offset, BitsToBytes(f.type.bitsize))
      END;
    END;
  END;
END PackRecord;

PROCEDURE ToTextRecord (t : Record; allowAlias, extensionCode : BOOLEAN)
: TEXT =
VAR g := ToText(t, allowAlias);
    str := "RECORD ";
    fieldStr : TEXT;
BEGIN
  IF g # NIL THEN RETURN g; END;
  FOR i := 0 TO LAST(t.fields^) DO
    WITH f = t.fields[i] DO
      fieldStr := f.type.declareVar(f.name, TRUE, extensionCode);
      IF f.default # NIL THEN
	fieldStr := fieldStr & " := " & f.default;
      END;
      str := str & fieldStr & ";";
    END;
  END;
  RETURN str & " END";
END ToTextRecord;

PROCEDURE ToCTextRecord (t : Record; name : TEXT; allowAlias: BOOLEAN)
: TEXT =
VAR str : TEXT;
BEGIN
  IF allowAlias AND t.alias # NIL THEN
    (* If NAME is a variable that references itself,
     * ex, TYPE foo=RECORD a:CHAR;  bar : REF foo; END;
     *we have to prefix it with "struct".
     *  Here, we take a conservative position and prefix type occurence
     *  with "struct" .
     *)
    RETURN t.alias & " " & name;
  END;

  (* Add a unique struct name *)
  str := "struct {";
  
  FOR i := 0 TO LAST(t.fields^) DO
    WITH f = t.fields[i] DO
      str := str & f.type.toCText(f.name) & ";";
    END;
  END;
  RETURN str & " } " & name;
END ToCTextRecord;

PROCEDURE ViewableRecord (t : Record) : BOOLEAN =
BEGIN
  FOR i := 0 TO LAST(t.fields^) DO
    IF NOT t.fields[i].type.viewable() THEN
      RETURN FALSE;
    END;
  END;
  RETURN TRUE;
END ViewableRecord;

PROCEDURE NeedSpaceRecord (t : Record) : BOOLEAN =
BEGIN
  FOR i := 0 TO LAST(t.fields^) DO
    IF t.fields[i].type.needSpace() THEN
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END NeedSpaceRecord;

PROCEDURE ExternalizeRecord (t : Record) = 
BEGIN
  FOR i := 0 TO LAST(t.fields^) DO
    t.fields[i].type.externalize();
  END;
END ExternalizeRecord;

(*
 *  Refs
 *)
REVEAL Ref = RefPublic BRANDED OBJECT
OVERRIDES
  viewable := False;
  needSpace := True; (* either in externalized ref or real ref, we need
		       caller *)
  exceptionOnUnpack := SpaceExceptionIfSpaceIsUsed;
  exceptionOnPack := ExceptionOnPackRef;
  toText := ToTextRef;
  toCText := ToCTextRef;
  unpack := UnpackRef;
  pack := PackRef;
  externalize := ExternalizeRef;
END;

PROCEDURE ExceptionOnPackRef (t : RefPublic) : REF ARRAY OF TEXT =
  VAR r : REF ARRAY OF TEXT;
  BEGIN
    IF t.capabilityP THEN
      r := NEW(REF ARRAY OF TEXT, 1);
      r[0] := "ExternalRef.Conflict";
      RETURN r;
    ELSE
      RETURN SpaceExceptionList;
    END;
  END ExceptionOnPackRef;
  
PROCEDURE ExternalizeRef (t : RefPublic) =
BEGIN
  t.capabilityP := TRUE;
  t.allowInlinePackingP := TRUE;
END ExternalizeRef;

PROCEDURE ToTextRef (t : RefPublic; allowAlias, extensionCode : BOOLEAN)
: TEXT =
  VAR g := ToText(t, allowAlias);
    brand := "";
  BEGIN
    IF t.capabilityP AND NOT extensionCode THEN RETURN "Word.T"; END;
    IF g # NIL THEN RETURN g; END;
    IF t.branded THEN
      IF t.brand # NIL THEN
	brand := "BRANDED " & t.brand & " ";
      ELSE 
	brand := "BRANDED ";
      END;
    END;
    RETURN brand & "REF " & t.target.toText(TRUE, extensionCode);
  END ToTextRef;

PROCEDURE ToCTextRef (t : RefPublic; name : TEXT; allowAlias : BOOLEAN)
: TEXT =
VAR g := ToCText(t, name, allowAlias);
BEGIN
  IF t.capabilityP THEN RETURN "long "&name; END;
  IF g # NIL THEN RETURN g; END;
  RETURN t.target.toCText("*" & name);
END ToCTextRef ;



PROCEDURE UnpackRef (t : RefPublic; var : TEXT;
		     addr : ParamAddr.T; wr : IWr.T) =
BEGIN
  IF t.capabilityP THEN
    wr.put("VAR tmp_: Word.T;BEGIN\n");
    addr.read("tmp_", integer, wr);
    wr.f("%s := NARROW(space_.getXrefTbl().internalize(tmp_), %s);\n",
	 var, t.toText());
    wr.put("END;\n");
  ELSE
    IF t.target = NIL THEN
      Msg.Error(t.srcPos, "You can't use opaque references for input.\n");
      wr.put("(*You can't use opaque references for input.*)\n");
      RETURN;
    END;

    wr.put("VAR tmp_ : Word.T;\n", "BEGIN\n");
    wr.indent();
    addr.read("tmp_", integer, wr);
    wr.put("IF tmp_ # 0 THEN ");
    wr.indent();
    wr.put(var, " := NEW(", t.toText(), ");\n");
    (* XXX CAUTION
     * here we rely on fact that TARGET does not share
     * the original type
     *  if REF is the circular pointer. 
     *)
    t.target.unpack(var& "^", ParamAddr.User("tmp_"), wr);
    wr.unindent();
    wr.put("END;\n");
    wr.unindent();
    wr.put("END;\n");
  END;
END UnpackRef;

PROCEDURE PackRef (t : Ref; var: TEXT; addr : ParamAddr.T; wr : IWr.T) =
  BEGIN
    IF t.capabilityP THEN
      wr.f("VAR tmp_ :=space_.getXrefTbl().externalize(%s);\n", var);
      wr.put("BEGIN\n");
      addr.write("tmp_", integer, wr);
      wr.put("END;\n");
    ELSE
      Msg.Error(t.srcPos, "VAR REF "&var&" is not supported.\n");
    END;
  END PackRef;

(*
 *  Objects
 *)
REVEAL Object = ObjectPublic BRANDED OBJECT 
OVERRIDES
  viewable := False;
  toText := ToTextObject;
  toCText := ToCTextObject;
  externalize := ExternalizeRef;
END;

PROCEDURE ToTextObject (t : Object; allowAlias, extensionCode : BOOLEAN)
: TEXT =
VAR g := ToText(t, allowAlias);
  s := "";
BEGIN
  IF t.capabilityP AND NOT extensionCode THEN RETURN "Word.T"; END;
  
  IF g # NIL THEN RETURN g; END;
  IF t.ancestor # NIL THEN
    s := t.ancestor & " ";
  END;
  IF t.branded THEN
    s := s & "BRANDED OBJECT METHODS";
  ELSE 
    s := s & "OBJECT METHODS";
  END;

  (* we don't consider outputing code for extension now *)
  <*ASSERT NOT extensionCode*>
  IF NOT extensionCode THEN
    (* cap_ is used to hold the externalized capability *)
    s := s & "cap_ : Word.T;\n";
  END;
  
  FOR i := 0 TO LAST(t.methods^) DO
    s := s & OutputProcDecl(t.methods[i].name, NARROW(t.methods[i].type, Proc), extensionCode) & ";\n";
  END;
  s := s & " END";
  RETURN s;
END ToTextObject;
  
PROCEDURE ToCTextObject (<*UNUSED*>t : Object;
			 name : TEXT;
			 <*UNUSED*>allowAlias : BOOLEAN)
  : TEXT =
  BEGIN
    (* XXX we need more friendly messages. *)
    RETURN "long " & name;
  END ToCTextObject;

(*
 *  Enums
 *)
REVEAL Enum = EnumPublic BRANDED OBJECT
OVERRIDES
  toText := ToTextEnum;
  toCText := ToCTextEnum;
  viewable := False;
END;


PROCEDURE ToTextEnum (t : Enum; allowAlias : BOOLEAN;
		      <*UNUSED*>extensionCode : BOOLEAN) : TEXT =
VAR g := ToText(t, allowAlias);
    s := "{";
BEGIN
  IF g # NIL THEN RETURN g; END;
  FOR i := 0 TO LAST(t.values^) DO
    IF i > 0 THEN s := s & ", "; END;
    s := s & t.values[i];
  END;
  RETURN s & "}";
END ToTextEnum;

PROCEDURE ToCTextEnum (t : Enum; name : TEXT; allowAlias : BOOLEAN) : TEXT =
VAR g := ToCText(t, name, allowAlias);
BEGIN
  IF g # NIL THEN RETURN g; END;
  RETURN Target.ToCTextOrdinal(t.bitsize) & " " & name;
  (* We ignore enum tags. They should be spit out elsewhere. *)
END ToCTextEnum;

(*
 *  Sets
 *)
REVEAL Set = SetPublic BRANDED OBJECT
OVERRIDES
  toText := ToTextSet;
  toCText := ToCTextSet;
END;

PROCEDURE ToTextSet (t : Set; allowAlias, extensionCode : BOOLEAN) : TEXT =
VAR g := ToText(t, allowAlias);
BEGIN
  IF g # NIL THEN RETURN g; END;
  RETURN "SET OF " & t.range.toText(TRUE, extensionCode);
END ToTextSet;

PROCEDURE ToCTextSet (t : Set; name : TEXT; allowAlias : BOOLEAN) : TEXT =
VAR g := ToCText(t, name, allowAlias);
BEGIN
  IF g # NIL THEN RETURN g; END;
  RETURN "struct { unsigned char b["& Fmt.Int(BitsToBytes(t.bitsize))&
  	"]; } " & name;
END ToCTextSet;

(*
 *  Packed stuff
 *)
REVEAL Packed = PackedPublic BRANDED OBJECT END;

(*
 *  TEXTs
 *)

PROCEDURE UnpackText (<*UNUSED*>t : Opaque; var : TEXT;
		      addr : ParamAddr.T; wr : IWr.T) =
  BEGIN
    wr.put("VAR tmp_: Word.T; BEGIN\n");
    addr.read("tmp_", integer, wr);
    wr.f("Sieg.UnpackTEXT(c_, tmp_, %s);\n", var);
    wr.put("END;\n");
  END UnpackText;

(*
 *  Booleans
 *)
PROCEDURE ToCTextBoolean (<*UNUSED*>t : T; name : TEXT;
			  <*UNUSED*>d : BOOLEAN) : TEXT =
BEGIN
  RETURN "int "&name;
END ToCTextBoolean;
PROCEDURE ToCTextChar (<*UNUSED*>t : T; name : TEXT;
			 <*UNUSED*>d : BOOLEAN) : TEXT =
BEGIN
  RETURN "char "&name;
END ToCTextChar;
PROCEDURE ToCTextReal (<*UNUSED*>t : T; name : TEXT;
		       <*UNUSED*>d : BOOLEAN) : TEXT =
BEGIN
  RETURN "float "&name;
END ToCTextReal;
PROCEDURE ToCTextLongreal (<*UNUSED*>t : T; name : TEXT;
			   <*UNUSED*>d : BOOLEAN) : TEXT =
BEGIN
  RETURN "double "&name;
END ToCTextLongreal;

PROCEDURE RegisterStandardTypes (table : TypeTable.T) =
BEGIN
  EVAL table.put(M3CStdTypes.Char(), char);
  EVAL table.put(M3CStdTypes.Text(), text);
  EVAL table.put(M3CStdTypes.Cardinal(), cardinal);
  EVAL table.put(M3CStdTypes.Boolean(), boolean);
  (* Other instances of standard types such as INTEGER are somehow not
   * shared. Therefore, we write an explicit typecase statement to
   * catch them. (see packType below).
   *)
END RegisterStandardTypes;					  

(* Output the prototype definition for the procedure. 
 * We are essentially uncompiling P into the original texutual
 * form here.
 *)
PROCEDURE OutputProcDecl (name : TEXT; p : Proc; extensionCode : BOOLEAN)
: TEXT =
VAR
  s := "PROCEDURE " & name & "(";
  j := 0;
BEGIN 
  FOR i := 0 TO LAST(p.params^) DO
    WITH f = p.params[i] DO
      IF f.type.extensionOnly AND extensionCode = FALSE THEN
	(* ignore this *)
      ELSE 
	IF j > 0 THEN s := s & "; "; END;
	INC(j);
	CASE f.mode OF
	| Mode.Value =>
	| Mode.InOut => s := s & "VAR ";
	| Mode.Out => s := s & "VAR ";
	| Mode.In => s := s & "READONLY ";
	ELSE
	  Msg.Fatal(p.srcPos, "???xx I screwed up something!");
	END;
	
	s := s & f.type.declareVar(f.name, TRUE, FALSE);
	IF f.default # NIL THEN
	  s := s & " := " & f.default;
	END;
      END;
    END;
  END;
  IF p.retType # NIL THEN
    s := s & ") : " & p.retType.toText(TRUE, extensionCode);
  ELSE
    s := s & ")";
  END;
  RETURN s;
END OutputProcDecl;

BEGIN
  (* Create an instance of types.... *)
  integer := NEW(ViewableOrd, name := "INTEGER", allowInlinePackingP := TRUE);
  integer.bitsize := BITSIZE(INTEGER);
  integer.align := BITSIZE(INTEGER);

  cardinal := NEW(Subrange,
		  name := "CARDINAL",
		  base := integer,
		  allowInlinePackingP := TRUE,
		  min := 0, max := LAST(CARDINAL));
  cardinal.bitsize := BITSIZE(CARDINAL);
  cardinal.align := BITSIZE(CARDINAL);
  
  boolean := NEW(Enum,
		 toCText := ToCTextBoolean,
		 allowInlinePackingP := TRUE,
		 name := "BOOLEAN");
  boolean.values := NEW(REF ARRAY OF TEXT, 2);
  boolean.values[0] := "FALSE";
  boolean.values[1] := "TRUE";
  boolean.bitsize := BITSIZE(BOOLEAN);
  boolean.align := BITSIZE(BOOLEAN);

  char :=  NEW(Char, name := "CHAR",
	       (*char is enum, but is repl. complete *)
	       viewable := True,
	       needSpace := False,
	       toCText := ToCTextChar,
	       allowInlinePackingP := TRUE,
	       unpack := UnpackOrdinal);
  char.bitsize := BITSIZE(CHAR);
  char.align := BITSIZE(CHAR);
  
  real := NEW(Real,
	      toCText := ToCTextReal,
	      allowInlinePackingP := TRUE,
	      name := "REAL");
  real.bitsize := BITSIZE(REAL);
  real.align := BITSIZE(REAL);
  
  longreal := NEW(LongReal,
		  toCText := ToCTextLongreal,
		  allowInlinePackingP := TRUE,
		  name := "LONGREAL");
  longreal.bitsize := BITSIZE(LONGREAL);
  longreal.align := BITSIZE(LONGREAL);
  
  extended := NEW(Extended,
		  allowInlinePackingP := TRUE,
		  name :=  "EXTENDED");
  extended.bitsize := BITSIZE(EXTENDED);
  extended.align := BITSIZE(EXTENDED);
  
  refany  := NEW(Ref, name := "REFANY", traced := TRUE);
  root  := NEW(Ref, name := "ROOT", traced := TRUE);
  refany.bitsize := BITSIZE(REFANY);
  refany.align := BITSIZE(REFANY);
  
  null := NEW(Ref, name := "NULL");
  null.bitsize := BITSIZE(REFANY);
  null.align := BITSIZE(REFANY);
  
  text := NEW(Text, name := "TEXT", alias := "TEXT", 
	      revealedPart := refany,
	      unpack := UnpackText);
  text.bitsize := BITSIZE(REFANY);
  text.align := BITSIZE(REFANY);

  SpaceExceptionList := NEW(REF ARRAY OF TEXT, 1);
  SpaceExceptionList[0] := "VMError.E";
  
  
END Type.

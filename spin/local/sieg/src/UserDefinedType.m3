(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 17-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)
MODULE UserDefinedType EXPORTS Type;
IMPORT IWr, Fmt, Msg;
IMPORT ParamAddr;

REVEAL RawWord = RawWordPublic BRANDED OBJECT
OVERRIDES
  declareVar := DeclareVarRawWord;
  toText := ToTextRawWord;
  toCText := ToCTextRawWord;
END;

PROCEDURE DeclareVarRawWord(t : RawWord;
			    name :TEXT;
			    <*UNUSED*>useAlias : BOOLEAN;
			    extensionCode : BOOLEAN) : TEXT =
BEGIN
  IF extensionCode THEN
    RETURN name & " : " & "Word.T";
  ELSE
    RETURN Fmt.F(t.m3ArgTemplate, name);
  END;
END DeclareVarRawWord;
PROCEDURE ToTextRawWord(t : RawWord;
			<*UNUSED*>useAlias : BOOLEAN;
			extensionCode : BOOLEAN) : TEXT =
BEGIN
  IF extensionCode THEN
    RETURN "Word.T";
  ELSE
    Msg.Fatal(t.srcPos, "???????");
    RETURN NIL;
  END;
END ToTextRawWord;

PROCEDURE ToCTextRawWord(t : RawWord;
			 name : TEXT;
			 <*UNUSED*>useAlias : BOOLEAN) : TEXT =
BEGIN
  IF t.cArgTemplate = NIL THEN
    Msg.Error(t.srcPos, "You have to specify C representation in AS");
    RETURN "(*you have to specify C representation*)";
  END;
  
  RETURN Fmt.F(t.cArgTemplate, name);
END ToCTextRawWord;


(*
   User defined type  "CString".
 
  CString is an alias to TEXT in the extension code. However, it is 
   a C-style null-terminated string on the user side. Stub code
   convert from C-string to TEXT. (opposite isn't supported now.)
 *)  
PROCEDURE ToCTextCString(<*UNUSED*>t : T;
			 name : TEXT;
			 <*UNUSED*>useAlias : BOOLEAN) : TEXT =
  BEGIN
    RETURN "char *" & name;
  END ToCTextCString;

PROCEDURE ToTextCString(<*UNUSED*>t : T;
			<*UNUSED*>useAlias : BOOLEAN;
			extensionCode : BOOLEAN) : TEXT =
  BEGIN
    IF extensionCode THEN RETURN "TEXT"; END;
    RETURN "UNTRACED REF ARRAY OF CHAR";
  END ToTextCString;

PROCEDURE UnpackCTEXT (<*UNUSED*>t : Opaque; var : TEXT;
		       addr : ParamAddr.T; wr : IWr.T) =
  BEGIN
    wr.put("VAR tmp_: Word.T; BEGIN\n");
    addr.read("tmp_", integer, wr);
    wr.f("%s := Sieg.UnpackCTEXT(space_, tmp_);\n", var);
    wr.put("END;\n");
  END UnpackCTEXT;


(*
 User defined type "CArrayOfChar".

 CString is an alias to ARRAY OF CHAR in the extension code. However, it is 
 a C-style null-terminated string on the user side. Stub code
 convert from C-string to ARRAY OF CHAR. (opposite isn't supported now.)
 *)

PROCEDURE ToTextCArrayOfChar(t : T;
			     <*UNUSED*>useAlias : BOOLEAN;
			     <*UNUSED*>extensionCode : BOOLEAN) : TEXT =
  BEGIN
    Msg.Fatal(t.srcPos, "CTEXT ARRAY OF CHAR can't be used other than as a parameter");
    RETURN NIL;
  END ToTextCArrayOfChar;

PROCEDURE UnpackCArrayOfChar (<*UNUSED*>t : T; var : TEXT;
			      addr : ParamAddr.T; wr : IWr.T) =
  VAR
    a := var & "_array_";
    len := var & "_len_";
  BEGIN
    wr.put("VAR tmp_: Word.T; BEGIN\n");
    addr.read("tmp_", integer, wr);
    wr.f("Sieg.UnpackCArrayOfChar(space_, tmp_, %s, %s);\n", a, len);
    wr.put("END;\n");
  END UnpackCArrayOfChar;

PROCEDURE PackCArrayOfChar (<*UNUSED*>t : T;
			    <*UNUSED*>var : TEXT;
			    <*UNUSED*>addr : ParamAddr.T;
			    <*UNUSED*>wr : IWr.T) =
  BEGIN
    (* do nothing *)
  END PackCArrayOfChar;

PROCEDURE UnpackNSName (<*UNUSED*>t : T; var : TEXT;
			addr : ParamAddr.T; wr : IWr.T) =
  BEGIN
    wr.put(var & ".str := CharArray.Allocate(128);\n");
    wr.put("VAR tmp_: Word.T; BEGIN\n");
    addr.read("tmp_", integer, wr);
    wr.f("Sieg.UnpackNSName(space_, tmp_, %s);\n", var);
    wr.put("END;\n");
  END UnpackNSName;
  
PROCEDURE DestructNSName (<*UNUSED*>t : T; var : TEXT; wr : IWr.T) =
  BEGIN
    wr.put("CharArray.Free(" & var & ".str);\n");
  END DestructNSName;
  
PROCEDURE ToCTextNSName(<*UNUSED*>t : T;
			 name : TEXT;
			 <*UNUSED*>useAlias : BOOLEAN) : TEXT =
  BEGIN
    RETURN "char *" & name;
  END ToCTextNSName;

PROCEDURE ToTextNSName(<*UNUSED*>t : T;
		       <*UNUSED*>useAlias : BOOLEAN;
		       <*UNUSED*>extensionCode : BOOLEAN) : TEXT =
  BEGIN
    RETURN "NameServer.Name"; 
  END ToTextNSName;
  
PROCEDURE InstanceNameCArrayOfChar (<*UNUSED*>t : T; name : TEXT) : TEXT =
  BEGIN
    RETURN Fmt.F("SUBARRAY(%s_array_, 0, %s_len_)", name, name);
  END InstanceNameCArrayOfChar;
  
PROCEDURE DeclareCArrayOfChar(<*UNUSED*>t : T; name : TEXT;
				<*UNUSED*>useAlias : BOOLEAN;
				extensionCode : BOOLEAN) : TEXT =
  BEGIN
    IF NOT extensionCode THEN
      RETURN name & " : ARRAY OF CHAR";
    ELSE
      RETURN Fmt.F("%s_array_ : ARRAY [0..1023] OF CHAR;\n"
		   & "%s_len_ : INTEGER", name, name);
    END;
  END DeclareCArrayOfChar;
  
 
(*
 Extension Only types.
 *)
PROCEDURE UnpackStrand(<*UNUSED*>t: T; var : TEXT;
		       <*UNUSED*>addr : ParamAddr.T;
		       wr : IWr.T) =
  BEGIN
    wr.put(var & " := strand_;\n");
  END UnpackStrand;

PROCEDURE UnpackSpace(<*UNUSED*>t : T;
		      var : TEXT;
		      <*UNUSED*>addr : ParamAddr.T;
		      wr : IWr.T) =
  BEGIN
    wr.put(var & " := space_;\n");
  END UnpackSpace;

PROCEDURE UnpackState(<*UNUSED*>t : T;
		      <*UNUSED*>var : TEXT;
		      <*UNUSED*>addr : ParamAddr.T;
		      <*UNUSED*>wr : IWr.T) =
  BEGIN
    (* do nothing *)
  END UnpackState;

PROCEDURE InstanceNameState (<*UNUSED*>t : T; <*UNUSED*>name : TEXT) : TEXT =
  BEGIN
    RETURN "s_";
  END InstanceNameState;

BEGIN
  (* Cstring is a special kind of user defined type, because we can't
   *  assign a typename to it. Cstring is an alias to TEXT from the
   * viewpoint of M3 frontend, and we distinguish between TEXT and CString
   * using hardcoded <*CTEXT true/false*> pragma.
   *)
  ctext := NEW(Text, alias := "CTEXT",
	       allowInlinePackingP := TRUE,
	       revealedPart := refany,
	       toText := ToTextCString,
	       toCText := ToCTextCString,
	       viewable := False,
	       unpack := UnpackCTEXT);
  ctext.bitsize := BITSIZE(REFANY);
  ctext.align := BITSIZE(REFANY);
  nsname := NEW(Text, alias := "CNSNAME",
	       allowInlinePackingP := TRUE,
		revealedPart := refany,
		toText := ToTextNSName,
		toCText := ToCTextNSName,
		viewable := False,
		destruct := DestructNSName,
		unpack := UnpackNSName);
  nsname.bitsize := BITSIZE(REFANY);
  nsname.align := BITSIZE(REFANY);

  carrayOfChar := NEW(OpenArray, alias := "CARRAYOFCHAR",
		      allowInlinePackingP := TRUE,
		      instanceName := InstanceNameCArrayOfChar,
		      declareVar := DeclareCArrayOfChar,
		      toText := ToTextCArrayOfChar,
		      toCText := ToCTextCString,
		      viewable := False,
		      unpack := UnpackCArrayOfChar,
		      pack := PackCArrayOfChar);
  carrayOfChar.bitsize := 8;
  carrayOfChar.align := 8;
		      
  strand := NEW(T, alias := "Strand.T",
		allowInlinePackingP := TRUE,
		extensionOnly := TRUE,
		needSpace := True,
		unpack := UnpackStrand);
  space := NEW(T, alias := "Space.T",
	       allowInlinePackingP := TRUE,
	       extensionOnly := TRUE,
	       needSpace := True,
	       unpack := UnpackSpace);
  state := NEW(T, alias := "CPU.SavedState",
	       instanceName := InstanceNameState,
	       allowInlinePackingP := TRUE,
	       unpack := UnpackState,
	       extensionOnly := TRUE,
	       needSpace := True);
		
END UserDefinedType.

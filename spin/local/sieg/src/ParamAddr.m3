(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Introduced temporary variable in code produced by WriteUserAddr.
 *
 * 19-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	made safe.
 * 26-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *	
 *	
 *)
MODULE ParamAddr;
IMPORT Fmt;
IMPORT Type;
IMPORT IWr;
IMPORT Msg;

(* Parameter Addresses *)
TYPE
  UserAddr = T BRANDED OBJECT
    base: TEXT;
    off: INTEGER;
  OVERRIDES
    adr := AdrUserAddr;
    inc := IncUserAddr;
    read := ReadUserAddr;
    write := WriteUserAddr;
    readDyn := ReadDynUserAddr;
    writeDyn := WriteDynUserAddr;
  END;
  
PROCEDURE AdrUserAddr(a: UserAddr): TEXT =
  BEGIN
    IF a.off = 0 THEN
      RETURN a.base;
    ELSE
      RETURN a.base & "+" & Fmt.Int(a.off);
    END;
  END AdrUserAddr;

PROCEDURE IncUserAddr(a: UserAddr; i: INTEGER): T =
  BEGIN
    RETURN NEW(UserAddr, base := a.base, off := a.off + i);
  END IncUserAddr;

PROCEDURE ReadUserAddr (u: UserAddr; var: TEXT; a: REFANY; wr: IWr.T) =
  VAR
    type := NARROW(a, Type.T);
  BEGIN
    IF NOT type.viewable() THEN
      Msg.Fatal(Msg.POS_VOID, "ParamAddr.Write : not viewable??");
    END;
    
    wr.f("Translation.Read(space_, %s, VIEW(%s, ARRAY OF CHAR));\n",
	 u.adr(), var);
  END ReadUserAddr;

PROCEDURE ReadDynUserAddr (u: UserAddr; var: TEXT; size: TEXT; wr: IWr.T) =
  BEGIN
    IF size = NIL THEN
      size := "BYTESIZE(" & size & ")";
    END;
    wr.f("Translation.Read(space_, %s, VIEW(%s, ARRAY [1..%s] OF CHAR));\n",
	 u.adr(), var, size);
  END ReadDynUserAddr;

PROCEDURE WriteUserAddr (u: UserAddr; var: TEXT; a: REFANY; wr: IWr.T) =
  VAR type := NARROW(a, Type.T);
  BEGIN
    IF NOT type.viewable() THEN
      Msg.Fatal(Msg.POS_VOID, "ParamAddr.Write : not viewable??");
    END;      
    wr.f("Translation.Write(space_, VIEW(%s, ARRAY OF CHAR), %s);\n",
	 var, u.adr());
  END WriteUserAddr;

PROCEDURE WriteDynUserAddr (u: UserAddr; var: TEXT; size: TEXT; wr: IWr.T) =
  BEGIN
    wr.f("Translation.Write(space_, VIEW(%s, ARRAY [1..%s] OF CHAR), %s);\n",
	 var, size, u.adr());
  END WriteDynUserAddr;


PROCEDURE User (virtAddr: TEXT): T =
BEGIN
  RETURN NEW(UserAddr, base := virtAddr, off := 0);
END User;

TYPE 
  KernelAddr = T BRANDED OBJECT
    base: TEXT;
    off: INTEGER;
  OVERRIDES
    adr := AdrKernelAddr;
    inc := IncKernelAddr;
    read := ReadKernelAddr;
    write := WriteKernelAddr;
    readDyn := ReadDynKernelAddr;
    writeDyn := WriteDynKernelAddr;
  END;

PROCEDURE AdrKernelAddr(a: KernelAddr): TEXT =
BEGIN
  IF a.off = 0 THEN
    RETURN a.base;
  ELSE
    RETURN a.base & "+" & Fmt.Int(a.off)
  END;
END AdrKernelAddr;

PROCEDURE IncKernelAddr(a: KernelAddr; i: INTEGER): T =
BEGIN
  RETURN NEW(KernelAddr, base := a.base, off := a.off + i);
END IncKernelAddr;

PROCEDURE ReadKernelAddr (k: KernelAddr; var: TEXT; a: REFANY; wr: IWr.T) =
  VAR 
    type := NARROW(a, Type.T);
  BEGIN
    IF k.off = 0 THEN
      wr.f("%s := VIEW(%s, %s);", var, k.base, type.toText());
    ELSE 
      wr.f("%s := VIEW(SUBARRAY(%s, %s, BYTESIZE(%s)),%s);",
	   var, 
	   k.base, Fmt.Int(k.off),
	   type.toText(), type.toText());
    END;
  END ReadKernelAddr;

PROCEDURE ReadDynKernelAddr (a: KernelAddr; var: TEXT; size: TEXT; wr: IWr.T) =
  BEGIN
    IF size = NIL THEN
      size := "BYTESIZE(" & var & ")";
    END;
    wr.f("VIEW(%s, ARRAY [1..%s] OF CHAR) := ", var, size);
    wr.f("SUBARRAY(%s, 0, %s);\n", a.adr(), size);
  END ReadDynKernelAddr;

PROCEDURE WriteKernelAddr (k: KernelAddr; var: TEXT; a: REFANY; wr: IWr.T) =
  VAR
    type := NARROW(a, Type.T);
  BEGIN
    IF k.off = 0 THEN
      wr.f("VIEW(%s, %s) := %s;\n", k.base, type.toText(), var);
    ELSE
      wr.f("VIEW(SUBARRAY(%s, %s, BYTESIZE(%s)), %s) := %s;\n",
	   k.base, Fmt.Int(k.off), type.toText(), type.toText(), var);
    END;
  END WriteKernelAddr;
  
PROCEDURE WriteDynKernelAddr (k: KernelAddr; var: TEXT; size: TEXT; wr: IWr.T)=
  BEGIN
    IF size = NIL THEN
      size := "BYTESIZE(" & var & ")";
    END;
    wr.f("SUBARRAY(%s, %s, %s)", k.base, Fmt.Int(k.off), size);
    wr.f(":= VIEW(%s, ARRAY [1..%s] OF CHAR);\n", var, size);
  END WriteDynKernelAddr;

PROCEDURE Kernel (addr: TEXT): T =
  BEGIN
    RETURN NEW(KernelAddr, base := addr, off := 0);
  END Kernel;

BEGIN
END ParamAddr.

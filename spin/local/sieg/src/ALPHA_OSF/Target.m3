(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 26-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	added RegAddr support.
 *	
 * 18-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	spin-6 support.
 *	
 * 03-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)

(* Type pack/unpacking routines for Alpha/SPIN. *)

MODULE Target EXPORTS Target, ParamAddr, Alpha;
IMPORT Fmt, Word;
IMPORT Msg;
IMPORT ParamAddr;
IMPORT Type;
IMPORT IWr;

FROM M3CBackEnd_C_cc IMPORT
ptrA, ptrS, realA, realS, longRealA, longRealS, intA, intS;

TYPE 
  RegAddr = ParamAddr.T BRANDED OBJECT
    reg: [-1 .. 6*8];
  OVERRIDES
    adr := Adr;
    inc := IncRegAddr;
    read := ReadRegAddr;
    write := WriteRegAddr;
    readDyn := ReadDynRegAddr;
    writeDyn := WriteDynRegAddr;
  END;
PROCEDURE Adr(a: RegAddr): TEXT =
BEGIN
  (*Msg.Error("adr not defined.");*)
  RETURN RegName(a.reg);
END Adr;

PROCEDURE IncRegAddr(a: RegAddr; i: INTEGER): ParamAddr.T =
BEGIN
  RETURN NEW(RegAddr, reg := a.reg+i);
END IncRegAddr;

PROCEDURE Reg (reg: INTEGER): ParamAddr.T =
BEGIN
  RETURN NEW(RegAddr, reg := reg);
END Reg;


PROCEDURE RegName (a: INTEGER): TEXT =
BEGIN
  IF a >= 0 THEN 
    RETURN "s_.a" & Fmt.Int(a DIV BYTESIZE(Word.T));
  ELSIF a = -1 THEN
    RETURN "s_.v0";
  ELSE
    Msg.Fatal(Msg.POS_VOID, "invalid reg number??");
    RETURN NIL;
  END;
END RegName;
		   
PROCEDURE ReadRegAddr (r: RegAddr; var: TEXT; a: REFANY; wr: IWr.T) =
  VAR exp: TEXT;
  BEGIN
    WITH type = NARROW(a, Type.T) DO
      TYPECASE type OF
      | Type.ViewableOrd, Type.Subrange =>
	exp := Fmt.F("%s := %s;", var, RegName(r.reg));
      | Type.Ordinal =>
	exp := Fmt.F("%s := VAL(%s, %s);", var, RegName(r.reg), type.toText());
      ELSE
	IF Type.BitsToBytes(type.bitsize) # regSize THEN
	  Msg.Error(type.srcPos, "You can't unpack value other than ",
		    Fmt.Int(regSize), " bytes long.\n");
	END;
	exp := Fmt.F("%s := VIEW(%s, %s);", var, RegName(r.reg), type.toText());
      END;
      wr.put(exp);
    END;
  END ReadRegAddr;

PROCEDURE WriteRegAddr (r: RegAddr; var: TEXT; a: REFANY; wr: IWr.T) =
  VAR type := NARROW(a, Type.T);
  BEGIN
    TYPECASE type OF
    | Type.Ordinal => 
      wr.f("%s := %s;\n", RegName(r.reg), var);
    ELSE
      IF type.bitsize # regSize*8 THEN
	Msg.Error(type.srcPos,
		  "Can't return aggregate value whose size is not",
		  Fmt.Int(regSize), ".\n");
	wr.put("(*ERROR*)");
      END;
      wr.f("%s := VIEW(%s, Word.T);\n", RegName(r.reg), var);
    END;
  END WriteRegAddr;

PROCEDURE ReadDynRegAddr (<*UNUSED*>a: RegAddr;
			  <*UNUSED*>var: TEXT;
			  <*UNUSED*>size: TEXT;
			  <*UNUSED*>wr: IWr.T) =
  BEGIN
    Msg.Fatal(Msg.POS_VOID, "I don't know how to read dyn on reg.\n");
  END ReadDynRegAddr;
  
PROCEDURE WriteDynRegAddr (<*UNUSED*>a: RegAddr;
			   <*UNUSED*>var: TEXT;
			   <*UNUSED*>size: TEXT;
			   <*UNUSED*>wr: IWr.T) =
  BEGIN
    Msg.Fatal(Msg.POS_VOID, "I don't know how to write dyn on reg.\n");
  END WriteDynRegAddr;

PROCEDURE GetReturnReg(): TEXT =
  BEGIN
    RETURN "s_.v0";
  END GetReturnReg;
PROCEDURE GetSyscallIDReg(): TEXT =
  BEGIN
    RETURN "s_.v0";
  END GetSyscallIDReg;

PROCEDURE ToCTextOrdinal(bitsize: CARDINAL): TEXT =
BEGIN
  IF bitsize <= 8 THEN 
    RETURN "char";
  ELSIF bitsize <= 16 THEN
    RETURN "short";
  ELSIF bitsize <= 32 THEN
    RETURN "int";
  ELSIF bitsize <= 64 THEN
    RETURN "long";
  ELSE
    Msg.Fatal(Msg.POS_VOID, "integer size too big!("&Fmt.Int(bitsize)&".\n");
    RETURN NIL;
  END;
END ToCTextOrdinal;
  
PROCEDURE ClearErrorReg (wr: IWr.T) =
  BEGIN
    wr.put("s_.a3 := 0;\n");
  END ClearErrorReg;
  
PROCEDURE SetErrorReg (code: TEXT; wr: IWr.T) =
  BEGIN
    wr.put("s_.a3 := ", code, ";\n");
    wr.put("s_.v0 := s_.a3;\n");
  END SetErrorReg;

PROCEDURE Init() =
BEGIN
    (* XXX Very weird thing is going on in my M3 compiler.
     * M3 compiler thinks himself as a
     * compiler for MIPS, and try to treat integer as 32bits.
     * This is a quick hack to undo it.
     *)
    ptrA := 64;
    ptrS := 64;
    realA := 32;
    realS := 32;
    longRealA := 64;
    longRealS := 64;
    intA := 64;
    intS := 64;
END Init;

BEGIN
    maxRegAddr := 8 * 5;  (* a0 .. a5 *)
    maxSavedRegAddr := 8 * 4;  (* a0 .. a4 *)
    regSize := 8;
END Target.


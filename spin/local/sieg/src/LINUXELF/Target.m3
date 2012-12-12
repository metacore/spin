(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 *
 * 19-Jul-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)

(* Type pack/unpacking routines for IX86/SPIN. *)

MODULE Target EXPORTS Target, ParamAddr;
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
  IF a = -1 THEN
    RETURN "s_.eax";
  ELSE
    RETURN "args_[" & Fmt.Int(a DIV BYTESIZE(Word.T)) & "]";
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
	wr.f("(*ERROR*)");
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

PROCEDURE GetReturnReg (): TEXT =
  BEGIN
    RETURN "s_.eax";
  END GetReturnReg;
  
PROCEDURE ToCTextOrdinal (bitsize: CARDINAL): TEXT =
  BEGIN
    IF bitsize <= 8 THEN 
      RETURN "char";
    ELSIF bitsize <= 16 THEN
      RETURN "short";
    ELSIF bitsize <= 32 THEN
    RETURN "long";
    ELSIF bitsize <= 64 THEN
    RETURN "long long";
    ELSE
      Msg.Fatal(Msg.POS_VOID, "integer size too big!("&Fmt.Int(bitsize)&".\n");
    RETURN NIL;
    END;
  END ToCTextOrdinal;
  
PROCEDURE GetSyscallIDReg (): TEXT =
  BEGIN
    RETURN "s_.eax";
  END GetSyscallIDReg;
  
PROCEDURE ClearErrorReg (wr: IWr.T) =
  BEGIN
    wr.put("s_.eflags := Word.And(s_.eflags, 16_fffffffe);\n");
  END ClearErrorReg;
  
PROCEDURE SetErrorReg (code: TEXT; wr: IWr.T) =
  BEGIN
    wr.put("s_.eflags := Word.Or(s_.eflags, 1);\n");
    wr.put("s_.edx := ", code, ";\n");
    wr.put("s_.eax := s_.edx;\n");
  END SetErrorReg;
  
PROCEDURE Init() =
BEGIN
    ptrA := 32;
    ptrS := 32;
    realA := 32;
    realS := 32;
    longRealA := 32;
    longRealS := 32;
    intA := 32;
    intS := 32;
END Init;

BEGIN
  (* on the Intel, all arguments are passed on the stack *)
    maxRegAddr := 0;
    maxSavedRegAddr := 0;
    regSize := 0;
END Target.


(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 27-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	added object externalization.
 *)
MODULE UserM3;
IMPORT Type;
IMPORT Fmt;
IMPORT UserI3;
IMPORT Module, Declaration, IWr;
IMPORT RegSchedule;
  
VAR
  mwr : IWr.T; (*output handle for FooUser.m3*)
  swr : IWr.T; (*output handle for FooUser.s*)

PROCEDURE EmitLoad(reg : TEXT; size : INTEGER) = 
BEGIN
  CASE size OF
  | 64 =>
    swr.put("\tldq ", reg, ",(", reg, ")\n");
    | 32 =>
    swr.put("\tldl ", reg, ",(", reg, ")\n");
  | 16 =>
    swr.put("\t.set noat\n");
    swr.put("\tlda $28, 2(", reg, ")\n");
    swr.put("\tldq_u $1, 0(", reg, ")\n");
    swr.put("\textqh $1, $28, $1\n");
    swr.put("\tsra $1, 48, ", reg, "\n");
    swr.put("\t.set at\n");
  | 8 =>
    swr.put("\t.set noat\n");
    swr.put("\tlda $28, 1(", reg, ")\n");
    swr.put("\tldq_u $1, 0(", reg, ")\n");
    swr.put("\textqh $1, $28, $1\n");
    swr.put("\tsra $1, 56, ", reg, "\n");
    swr.put("\t.set at\n");
  ELSE
    swr.put("?????\n");
  END;
END EmitLoad;

PROCEDURE EmitStore(reg : TEXT; size, off  : INTEGER; swr : IWr.T) = 
BEGIN
  (* $1 holds the address *)
  swr.put("\tldq $1, ", Fmt.Int(off), "(sp)\n");
    
  CASE size OF
  | 64 =>
    swr.put("\tstq ", reg, ", 0($1)\n");
  | 32 =>
    swr.put("\tstl ", reg, ", 0($1)\n");
  | 16 =>
    swr.put("\tldq_u $28, 0($1)\n");
    swr.put("\tinswl ", reg, ", $1, $2\n");
    swr.put("\tmskwl $28, $1, $28\n");
    swr.put("\tbis $28, $2, $28\n");
    swr.put("\tstq_u $28, 0($1)\n");
  | 8 =>
    swr.put("\tldq_u $28, 0($1)\n");
    swr.put("\tinsbl ", reg, ", $1, $2\n");
    swr.put("\tmskbl $28, $1, $28\n");
    swr.put("\tbis $28, $2, $28\n");
    swr.put("\tstq_u $28, 0($1)\n");
  ELSE
    swr.put("?????\n");
  END;
END EmitStore;

PROCEDURE OutputAsmProc (READONLY m: Module.T; name: TEXT;
			 decl: Declaration.Proc) =
  VAR
    asName := UserI3.ExternalName(m.intf, name, decl.proc);
    (* XXX We actually have to distinguish between names appear in
     *  <* EXTERNAL xxx *> and names appear in object files.
     * However, they seem to be same in OSF/1
     *)
    bufferSize := 0; (* tmp stack size *)
    off: INTEGER; (* stack offset *)
    j: INTEGER;
    proc := decl.proc;
  BEGIN

    (* Count the number of imput arguments, It's passed to NESTED directive *)
    j := 0;
    FOR i := 0 TO LAST(proc.params^) DO
      WITH f = proc.params[i] DO
	IF NOT f.type.extensionOnly THEN
	  INC(j);
	END;
      END;
    END;

    (* First, output the synonyms *)
    FOR i := 0 TO LAST(decl.userSideName) DO
      IF decl.userSideName[i] # NIL THEN
	swr.put("\t.globl ", decl.userSideName[i], "\n");
	swr.put(decl.userSideName[i], ":\n");
      END;
    END;
      
    swr.put("NESTED(", asName,",", Fmt.Int(j) ,",zero)\n");
    swr.put("\tlda v0, "&Fmt.Int(decl.id)&"(zero)\n");

    (* Calculate the number of buffers we need to save call-by-value-result
     locations *)
    FOR i := 0 TO LAST(proc.params^) DO
      WITH f = proc.params[i] DO
	IF NOT f.type.extensionOnly
	  AND f.mode # Type.Mode.Value
	  AND RegSchedule.CalledByValueResult(m, f.type) THEN
	  (* translate call-by-reference into call-by-value *)
	  INC(bufferSize, 8);
	END;
      END;
    END;
  
    IF bufferSize > 0 THEN
      off := 0;
      swr.put("\tlda sp, -", Fmt.Int(bufferSize), "(sp)\n");
      j := 0;
      FOR i := 0 TO LAST(proc.params^) DO
	WITH f = proc.params[i] DO 
	  IF NOT f.type.extensionOnly THEN
	    IF f.mode # Type.Mode.Value
	      AND RegSchedule.CalledByValueResult(m, f.type) THEN
	      (* translate call-by-reference into call-by-value *)
	      swr.put("\tstq a", Fmt.Int(f.inReg DIV 8),
		      ",", Fmt.Int(off), "(sp)\n");
	      IF f.mode = Type.Mode.InOut OR f.mode = Type.Mode.In THEN 
		EmitLoad("a" & Fmt.Int(f.inReg DIV 8), f.type.bitsize);
	      END;
	      INC(off, 8);
	    END;
	    INC(j);
	  END;
	END;
      END;
    END;
  
    swr.put("\tcall_pal PAL_callsys\n");
    IF m.errorFunction # NIL THEN
      (* take a forward branch in case of error *)
      IF bufferSize > 0 THEN
	swr.put("\tbne     a3, ", name, "_error\n");
      ELSE
	swr.put("\tbne     a3, syscall_error\n");
      END;
    END;
  
  
    IF bufferSize > 0 THEN
      off := 0;
      j := 0;
      FOR i := 0 TO LAST(proc.params^) DO
	WITH f = proc.params[i] DO 
	  IF NOT f.type.extensionOnly THEN
	    IF ((f.mode = Type.Mode.Out OR f.mode = Type.Mode.InOut)
		AND f.type.bitsize <= 64) THEN
	      (* translate call-by-reference into call-by-value *)
	      EmitStore("a" & Fmt.Int(f.outReg DIV 8),
			f.type.bitsize, off, swr);
	      INC(off, 8);
	    END;
	    INC(j);
	  END;
	END;
      END;
      swr.put("\tlda sp, ", Fmt.Int(bufferSize), "(sp)\n");
    END;
    swr.put("\tRET\n");
  
    IF bufferSize > 0 AND m.errorFunction # NIL THEN
      (* Here comes the error routine *)
      swr.put(name, "_error:\n");
      swr.put("\tlda sp, ", Fmt.Int(bufferSize), "(sp)\n");
      swr.put("\tjmp zero, syscall_error\n");
    END;
  
    swr.put("\tEND(",asName,")\n");
  END OutputAsmProc;

PROCEDURE IsExternalizedObject (t: Type.T): BOOLEAN =
  BEGIN
    TYPECASE t OF
    | Type.Object(ot) =>
      RETURN ot.capabilityP;
    ELSE
      RETURN FALSE;
    END;
  END IsExternalizedObject;

PROCEDURE OutputM3Proc (<*UNUSED*>READONLY m : Module.T; name : TEXT;
			decl : Declaration.Proc) =
  VAR proc := decl.proc;
  BEGIN
    mwr.put("PROCEDURE ", Type.OutputProcDecl(name, proc, TRUE));
    mwr.put(" =\n");
    
    mwr.put("VAR\n");
    mwr.indent();
    FOR i := 0 TO LAST(proc.params^) DO
      WITH f = proc.params[i] DO
	IF IsExternalizedObject(f.type) THEN 
	  mwr.put(f.name, "_ : Word.T;\n");
	END;
      END;
    END;
    IF proc.retType # NIL THEN
      mwr.put(proc.retType.declareVar("retval_", TRUE, FALSE), ";\n");
    END;
    
    mwr.unindent();
    mwr.put("BEGIN\n");
    mwr.indent();

    IF proc.retType # NIL THEN
      mwr.put("retval_ := ");
    END;
    mwr.put(name & "_(");
    
    FOR i := 0 TO LAST(proc.params^) DO
      IF i > 0 THEN mwr.put(","); END;
      WITH f = proc.params[i] DO
	IF IsExternalizedObject(f.type) THEN 
	  mwr.put(f.name & "_");
	ELSE
	  mwr.put(f.name);
	END;
      END;
    END;
    mwr.put(");\n");
    FOR i := 0 TO LAST(proc.params^) DO
      WITH f = proc.params[i] DO
	IF IsExternalizedObject(f.type) THEN 
	  mwr.put(f.name, " := NEW(", f.type.toText(TRUE, FALSE),
		  ", cap_ := ", f.name, "_);\n");
	END;
      END;
    END;

    IF proc.retType # NIL THEN
      IF IsExternalizedObject(proc.retType) THEN 
	mwr.put("RETURN NEW(", proc.retType.toText(TRUE, FALSE),
		", cap_ := retval_);\n");
      ELSE
	mwr.put("RETURN retval_;\n");
      END;
    END;
    
    mwr.unindent();
    mwr.put("END " & name & ";\n");
  END OutputM3Proc;


PROCEDURE OutputProc (READONLY m : Module.T; decl : Declaration.Proc) =
  BEGIN
    IF NOT decl.proc.needTrampolineCode THEN
      OutputAsmProc(m, decl.name, decl);
    ELSE
      OutputAsmProc(m, decl.name & "_", decl);
      OutputM3Proc(m, decl.name, decl);
    END;
  END OutputProc;

  
PROCEDURE Output (READONLY m : Module.T) =
  BEGIN
    mwr := IWr.OpenWrite(m.intf&"User.m3");
    swr := IWr.OpenWrite(m.intf&"User.s");
    swr.put("#include <alpha/regdef.h>\n#include <alpha/asm.h>\n");
    
    mwr.put("MODULE ", m.intf&"User;\n");
    mwr.put("IMPORT Word;\n");
    
    FOR i := 0 TO m.names.size()-1 DO
      TYPECASE m.names.get(i) OF
      | Declaration.Proc(decl) =>
	OutputProc(m, decl);
      ELSE
      END;
    END;
    
    mwr.put("END ",  m.intf&"User.\n");
    mwr.close();
    
    IF m.errorFunction # NIL THEN
      swr.put("syscall_error:\n");
      swr.put("\tbis a3, a3, a0\n");
      swr.put("\tlda t12, ", m.errorFunction, "\n");
      swr.put("\tjmp zero, (t12)\n");
    END;
    swr.close();
    
  END Output;

BEGIN

END UserM3.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 19-Jul-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
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


PROCEDURE OutputAsmProc (READONLY m : Module.T;  name : TEXT;
			 decl : Declaration.Proc) =
VAR
  asName := UserI3.ExternalName(m.intf, name, decl.proc);
BEGIN

  (* First, output the synonyms *)
  FOR i := 0 TO LAST(decl.userSideName) DO
    IF decl.userSideName[i] # NIL THEN
      swr.put("\t.globl _", decl.userSideName[i], "\n");
      swr.put("_", decl.userSideName[i], ":\n");
    END;
  END;

  swr.put("\t.globl _", asName, "\n");
  swr.put("_", asName, ":\n");

  swr.put("\tmovl $", Fmt.Int(decl.id), ",%eax\n");
  swr.put("\t.byte 0x9a ; .long 0 ; .word 7\n");
  IF m.errorFunction # NIL THEN
    swr.put("\ttestl %eax,%eax\n");
    swr.put("\tjne syscall_error\n");
  END;
   
  swr.put("\tRET\n");
  
END OutputAsmProc;

PROCEDURE ExternalizedObjectP(t : Type.T) : BOOLEAN =
  BEGIN
    TYPECASE t OF
    | Type.Object(ot) =>
      RETURN ot.capabilityP;
    ELSE
      RETURN FALSE;
    END;
  END ExternalizedObjectP;

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
	IF ExternalizedObjectP(f.type) THEN 
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
	IF ExternalizedObjectP(f.type) THEN 
	  mwr.put(f.name & "_");
	ELSE
	  mwr.put(f.name);
	END;
      END;
    END;
    mwr.put(");\n");
    FOR i := 0 TO LAST(proc.params^) DO
      WITH f = proc.params[i] DO
	IF ExternalizedObjectP(f.type) THEN 
	  mwr.put(f.name, " := NEW(", f.type.toText(TRUE, FALSE),
		  ", cap_ := ", f.name, "_);\n");
	END;
      END;
    END;

    IF proc.retType # NIL THEN
      IF ExternalizedObjectP(proc.retType) THEN 
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
      swr.put("\tcall ", m.errorFunction, "\n");
      swr.put("\tret\n");
    END;
    swr.close();
    
  END Output;

BEGIN

END UserM3.







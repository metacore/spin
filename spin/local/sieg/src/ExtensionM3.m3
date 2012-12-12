(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 13-Jun-97  David Becker at the University of Washington
 *      Restored Error.E in Syscall.  Though I hadnt removed it.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Change Skip and Syscall to handle Errno.UnixError exception
 *
 * 22-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to table jump.
 * 10-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	added REF support, and VAR support.
 * 03-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)
MODULE ExtensionM3;
IMPORT Type;
IMPORT IO, Rd, IWr, TextWr;
IMPORT Msg;
IMPORT Text, Fmt, TextRefTbl;
IMPORT Module, Declaration;
IMPORT ExtensionI3;
IMPORT ParamAddr;
IMPORT RegSchedule;
IMPORT Target;

VAR templateRd: Rd.T; (* extension_tmpl.m3 read handle *)

CONST DefaultImports =
  ARRAY OF TEXT {"CPU", "Space", "Trap", "UserSpaceThread",
		 "VMError", "Translation", "NameServer", "CharArray",
		 "Strand", "SpinException", "AddressSpace", 
		 "Errno", "ErrnoDep", "Error",  "ExternalRef",
		 "IO", "Fmt", "Word", "Dispatcher", "Sieg"};

PROCEDURE RaisesList (t: TextRefTbl.T): TEXT =
  VAR
    itr := t.iterate();
    exc : TEXT;
    xxx : REFANY;
    wr := NEW(TextWr.T).init();
    i : CARDINAL := 0;
  BEGIN
    IF t.size() = 0 THEN RETURN ""; END;
    
    IO.Put("RAISES {", wr);
    WHILE itr.next(exc, xxx) DO 
      IF i > 0 THEN IO.Put(", ", wr); END;
      INC(i);
      IO.Put(exc, wr);
    END;
    IO.Put("}", wr);
    RETURN TextWr.ToText(wr);
  END RaisesList;
  
(*
 * Output dispatcher code for a procedure.
 *)
PROCEDURE OutputProc (READONLY m: Module.T;
		      name: TEXT; p: Type.Proc; wr: IWr.T) =
  VAR
    numStackArgs: INTEGER;
  BEGIN
    wr.put("PROCEDURE ", name, "_ (");

    IF NOT p.needSpace THEN
      wr.put("<*UNUSED*>");
    END;
    
    wr.put("strand_: Strand.T; VAR s_: CPU.SavedState) ");

    (* output RAISES list *)
    wr.put(RaisesList(p.raises), "=\n");
    wr.put("VAR\n");
    wr.indent();
    
    (* Declare the parameters *)
    FOR i := 0 TO LAST(p.params^) DO
      WITH t = p.params[i] DO
	wr.put(t.type.declareVar(t.name), ";\n");
	IF t.mode # Type.Mode.Value THEN
	  (* declare the var to remember the address of the var param. *)
	  wr.put(t.name, "_adr: INTEGER;\n");
	END;
      END;
    END;

    (*
       Declare placeholder for the return value.
     *)
    IF p.retType # NIL THEN
      wr.put(p.retType.declareVar("ret_"), ";\n");
    END;

    IF p.needSpace THEN
      wr.put("space_ := UserSpaceThread.GetSpace(strand_);\n");
    END;

    numStackArgs := LAST(p.params^) - Target.maxRegAddr + 1;
    IF numStackArgs > 0 THEN
      <*ASSERT p.needSpace*>
      wr.put("args_: ARRAY [0.." & Fmt.Int(numStackArgs-1) & "] OF Word.T;\n");
    END;

    wr.unindent();
    wr.put("BEGIN\n");
    wr.indent();
  
    IF numStackArgs > 0 THEN
      (* XXX this is x86 specific. *)
      wr.put("Translation.Read(space_, s_.usp+4, ", "VIEW(args_, ARRAY OF CHAR));\n");
    END;
    
    (* Unpack the parameters *)
    FOR i := 0 TO LAST(p.params^) DO
      WITH t = p.params[i] DO
	IF t.mode = Type.Mode.Value
	  OR RegSchedule.CalledByValueResult(m, t.type)
	  OR t.type = Type.nsname THEN
	  (* XXX ugly hack. NSName.T is very special, VAR/READONLY is
	     ignored. *)
	  (* If the VAR parameter size fits a register, they are
	     treated like call-value-return parameter.
	     See also UserM3. *)
	  t.type.unpack(t.name, ParamAddr.Reg(t.inReg), wr);
	ELSE
	  (* remember the address in the variable "x_adr", where "x" is the
	     orig variable name. *)
	  Type.integer.unpack(t.name & "_adr", ParamAddr.Reg(t.inReg), wr);

	  (* Get the value from user space only when param is in or in-out
	     reference. *)
	  IF t.mode # Type.Mode.Out THEN
	    t.type.unpack(t.name, ParamAddr.User(ParamAddr.Reg(t.inReg).adr()), wr);
	  END;
	END;
      END;
    END;

    (* Clear the error code register. Note: We can't move this to
       after the handler is called, because the handler may use
       a3 for a different purpose. (eg, when a signal is raised,
       a3 holds the handler address. *)
    Target.ClearErrorReg(wr);
    
    (* Call the body *)
    IF p.retType # NIL THEN
      wr.put("ret_ := ");
    END;

    wr.put(name, "(");
    
    FOR i := 0 TO LAST(p.params^) DO
      IF i > 0 THEN wr.put(", "); END;
      wr.put(p.params[i].type.instanceName(p.params[i].name));
    END;
    wr.put(");\n");
    
    (* Write back VAR parameters and REF values. *)
    FOR i := 0 TO LAST(p.params^) DO
      WITH t = p.params[i] DO
	IF (t.mode = Type.Mode.Out OR t.mode = Type.Mode.InOut)
	  AND NOT t.type.extensionOnly THEN
	  IF t.type = Type.nsname THEN
	    (* XXX ugly hack. NSName.T is very special, VAR/READONLY is
	       ignored. *)
	  ELSIF RegSchedule.CalledByValueResult(m, t.type) THEN
	    t.type.pack(t.name, ParamAddr.Reg(t.outReg), wr);
	  ELSE
	    t.type.pack(t.name, ParamAddr.User(t.name & "_adr"), wr);
	  END;
	ELSIF ISTYPE(t.type, Type.Ref) THEN
	  TYPECASE t.type OF
	  | Type.Text =>
	    (* text is special. it is ref, but is readonly *)
	  | Type.Ref(r) =>
	    IF NOT r.capabilityP THEN
	      t.type.pack(t.name,
			  ParamAddr.User(ParamAddr.Reg(t.outReg).adr()), wr);
	    END;
	  ELSE
	  END;
	END;
	t.type.destruct(t.name, wr);
      END;
    END;
    
    IF p.retType # NIL THEN
      p.retType.pack("ret_", ParamAddr.Reg(-1), wr);
    END;
    wr.unindent();
    wr.put("END ", name, "_;\n");
  END OutputProc;

PROCEDURE NeedToOutputStub (p: Type.Proc): BOOLEAN =
BEGIN
  IF NUMBER(p.params^) = 2
    AND p.params[0].type = Type.strand
    AND  p.params[1].type = Type.state THEN
    RETURN FALSE;
  ELSE
    RETURN TRUE;
  END;
END NeedToOutputStub; 

PROCEDURE Output (READONLY m: Module.T) =
VAR wr := IWr.OpenWrite(m.intf&"Extension.m3");
  templateFile := ExtensionI3.templateFile & ".m3";
  procDecl: Declaration.Proc;
  needSkipProc := FALSE;
  blah: REFANY;
BEGIN
  templateRd := IO.OpenRead(templateFile);
  IF templateRd = NIL THEN
    Msg.Fatal(Msg.POS_VOID, templateFile, ": can't open.\n");
  ELSE
    Msg.Verbose(Msg.POS_VOID, "opening template file "&templateFile&".\n");
  END;
  
  wr.put(
"(* This file is produced by sieg. Please do not edit this file directly.\n"
	   &"  Instead, kick yasushi@cs. *)\n", 
"MODULE "& m.intf & "Extension;\n");

  (* Internalize all symbols defined in the original i3 files, because
   *  we don't want to prefix interface name to each symbols(we can do this,
   *  but the program will be messy).
   *)
  wr.put("FROM "&m.intf&" IMPORT ");
  FOR i := 0 TO m.names.size()-1 DO
    IF i > 0 THEN wr.put(","); END;
    wr.put(m.names.get(i).name);
  END;
  wr.put(";<*NOWARN*>\n");

  (* Import modules listed in the original i3 file *)
  FOR i := 0 TO m.imports.size()-1 DO
    wr.put("IMPORT ", m.imports.get(i), ";<*NOWARN*>\n");
  END;
  
  (* Import default modules only if they aren't imported already *)
  FOR i := 0 TO LAST(DefaultImports) DO
    VAR found := FALSE;
    BEGIN
      FOR j := 0 TO m.imports.size()-1 DO
	IF Text.Equal(DefaultImports[i], m.imports.get(j)) THEN
	  found := TRUE;
	  EXIT;
	END;
      END;
      IF NOT found THEN
	IF Text.Equal(DefaultImports[i], "Errno") OR
	  Text.Equal(DefaultImports[i], "Error") THEN
	  IF m.errorFunction # NIL THEN
	    wr.put("IMPORT ", DefaultImports[i], ";<*NOWARN*>\n");
	  END;
	ELSE
	  wr.put("IMPORT ", DefaultImports[i], ";<*NOWARN*>\n");
	END;
      END;
    END;
  END;
  ExtensionI3.CopyTemplateTillMarker(templateRd, wr);

  (* Create a jump table *)
  wr.put("CONST table_ = ARRAY [", Fmt.Int(m.minProcID), " .. ",
	 Fmt.Int(m.maxProcID), "] OF PROCEDURE (st: Strand.T; VAR s: CPU.SavedState) ");

  (* output RAISES list *)
  wr.put(RaisesList(m.exceptionList), " {\n");

  FOR i := m.minProcID TO m.maxProcID DO
    IF Module.LookupProc(m, i, procDecl) THEN 
      IF NeedToOutputStub(procDecl.proc) THEN
	wr.put("  ", procDecl.name, "_");
      ELSE 
	wr.put("  ", procDecl.name);
      END;
    ELSE
      (* no procedure that has the number "i".  *)
      wr.put("  Skip_");
      needSkipProc := TRUE;
    END;
    
    IF i # m.maxProcID THEN
      wr.put(",");
    END;
    wr.put("\n");
  END;
  wr.put("};\n");

  ExtensionI3.CopyTemplateTillMarker(templateRd, wr);

  IF needSkipProc THEN
    wr.put("PROCEDURE Skip_(<*UNUSED*>st_: Strand.T; VAR s_: CPU.SavedState) RAISES {Errno.E} =\n");
    wr.indent();
    wr.put("BEGIN\n");
    wr.f("IO.Put(\"skip syscall: \" & Fmt.Int(%s) & \".\\n\");",
	 Target.GetSyscallIDReg());
    wr.put("RAISE Errno.E(ErrnoDep.ENOTSUP);\n");
    wr.put("END Skip_;\n");
    wr.unindent();
  END;
  
  FOR i := 0 TO m.names.size()-1 DO
    WITH genericDecl = m.names.get(i) DO 
      TYPECASE genericDecl OF
      | Declaration.Proc(decl) =>
	IF NeedToOutputStub(decl.proc) THEN
	  OutputProc(m, decl.name, decl.proc, wr);
	END;
      ELSE
      END;
    END;
  END;

  (* Then, output the Syscall itself *)
  wr.put("PROCEDURE Syscall(st_: Strand.T; VAR s_: CPU.SavedState) =\n");
  wr.put("BEGIN\n");
  wr.indent();
  IF m.errorFunction # NIL THEN
    wr.put("TRY\n");
    wr.indent();
    wr.f("table_[%s](st_, s_);\n", Target.GetSyscallIDReg());
    wr.unindent();
    wr.put("EXCEPT\n");
    wr.put("| Errno.E(err) =>\n");
    Target.SetErrorReg("err", wr);
    wr.put("| Error.E(e) =>\n");
    Target.SetErrorReg("e.errno()", wr);
    wr.put("| SpinException.Exception(e) =>\n");
    Target.SetErrorReg("Sieg.SpinExceptionToErrno(e)", wr);
    wr.put("| VMError.E =>\n");
    Target.SetErrorReg("ErrnoDep.EFAULT", wr);
    wr.put("| NameServer.Error =>\n");
    Target.SetErrorReg("ErrnoDep.ENOENT", wr);

    IF m.exceptionList.get("Socket.Error", blah) THEN
      wr.put("| Socket.Error(ec) =>\n");
      Target.SetErrorReg("ec", wr);
    END;
    
    wr.put("END;\n");
  ELSE
    wr.put("table_[", Target.GetSyscallIDReg(), "](st_, s_);\n");
  END;
  IF m.afterHook # NIL THEN
    wr.put(m.afterHook, "(st_, s_);\n");
  END;
  wr.unindent();
  wr.put("END Syscall;\n");
  
  ExtensionI3.CopyTemplateTillMarker(templateRd, wr);
  wr.put("END ",m.intf,"Extension.\n");
  wr.close();
END Output;

BEGIN
END ExtensionM3.

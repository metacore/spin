(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	borrowed from netobj stubgen.
 *)


(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE Main;

(* The stub generator uses the standard tool framework provided by
 "M3ToolFrame".  
 Each interface given on the command line is processed, and
 stubs are generated for all the network objects it defines
 that cn legitimately be marshalled.
*)

IMPORT Process, RTCollector;
IMPORT Stdio, Wr;
IMPORT Pass1;
IMPORT ASTWalk;
IMPORT M3CId, M3CConcTypeSpec;
IMPORT M3Args, M3Context, M3Conventions, M3AST_AS, M3CUnit, M3CFETool, 
       M3ToolFrame;
IMPORT M3AST_all; (* this cannot be omitted; it defines the particular
                     revelations for all the AST nodes *)
IMPORT Text, Msg;
IMPORT Target;

IMPORT UserI3, UserM3, ExtensionM3 ,ExtensionI3, UserCHeader, ExtInfo;
IMPORT RegSchedule;

TYPE
  OutputType = {User, Extension, Info};
  OutputMode = SET OF OutputType;
VAR
  args : M3Args.T; (* command line arg processor *)
  outputMode := OutputMode {OutputType.User, OutputType.Extension, OutputType.Info};
  (*output everything by default*)
  
TYPE ContextClosure = M3Context.Closure OBJECT
    wr: Wr.T;
OVERRIDES callback := VisitUnit;
END;

PROCEDURE Set(context: M3Context.T; cu: M3AST_AS.Compilation_Unit) =
VAR
  intfname := M3CId.ToText(cu.as_root.as_id.lx_symrep);
  domain := M3Args.GetString(args, "DomainName");
  h: Pass1.Handle;
BEGIN

  (* Some cases we dont want to handle, plus making sure we
   deal with generic instantiations. *)
  TYPECASE cu.as_root OF
  | NULL => RETURN
  | M3AST_AS.UNIT_GEN_DEF => 
    (* parsing is ok, but no semantic analysis *)
  | M3AST_AS.UNIT_GEN_INS(ui) =>
    WITH cuIns = ui.sm_ins_comp_unit DO
      IF cuIns # NIL THEN
	(* Do it again on the instantiated module. *)
	Set(context, cuIns);
      END;
    END;
    RETURN;
  | M3AST_AS.UNIT_NORMAL =>
    (* Just ignore built-in pseudo module *)
    IF cu = M3Context.Standard() THEN RETURN END;
  ELSE Msg.Fatal(Msg.POS_VOID, "Run time error -- should not occur");
  END;

  (* No reason to look at MODULEs *)
  TYPECASE cu.as_root OF
  | M3AST_AS.Module, M3AST_AS.Module_gen_ins, M3AST_AS.Module_gen_def =>
    RETURN;
  ELSE (* continue *)
  END;

  h := Pass1.NewHandle(intfname, domain, context);

  M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Entry);
  ASTWalk.VisitNodes(cu, h);
  M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Exit);

  (* Assign registers to each params in each procedure *)
  RegSchedule.Scan(h.m);
    
  IF OutputType.Extension IN outputMode THEN
    ExtensionM3.Output(h.m);
    ExtensionI3.Output(h.m);
  END;
      
  IF OutputType.User IN outputMode THEN
    UserI3.Output(h.m);
    UserM3.Output(h.m);
    IF M3Args.GetFlag(args, "GenerateCHeader") THEN
      UserCHeader.Output(h.m);
    END;
  END;
  IF OutputType.Info IN outputMode THEN
    ExtInfo.Output(h.m);
  END;
	    
END Set;
    
(* This guy is called once for each node in the parse tree. *)
PROCEDURE VisitUnit(cl: ContextClosure;
		    ut: M3CUnit.Type;
		    <*UNUSED*> name: TEXT;
		    cu: M3AST_AS.Compilation_Unit) RAISES {}=
BEGIN
  (* if it is a generic instantiation, get to actual instantiated tree *)
  cu := M3CUnit.ToGenIns(cu, ut); 
  IF M3Conventions.PrimarySource IN cu.fe_status AND
    (M3CUnit.Errors * cu.fe_status = M3CUnit.Status{}) THEN
    Set(cl.context, cu);
  END;
END VisitUnit;


PROCEDURE DoRun(<*UNUSED*> w: M3ToolFrame.Worker; c: M3Context.T;
                <*UNUSED*> compileResult: INTEGER): INTEGER RAISES {}=
VAR
  rc : INTEGER;
  str : TEXT;
BEGIN
  Target.Init();

  (* Parse command line args here. *)
  IF M3Args.Find(args) THEN
    str := M3Args.GetString(args, "ExtensionTemplate");
    IF str # NIL THEN
      ExtensionI3.templateFile := str;
    END;
    str := M3Args.GetString(args, "OutputMode");
    IF str # NIL THEN
      IF Text.Equal(str, "both") THEN
	outputMode := OutputMode {OutputType.User, OutputType.Extension};
      ELSIF Text.Equal(str, "user") THEN
	outputMode := OutputMode {OutputType.User};
      ELSIF Text.Equal(str, "extension") THEN
	outputMode := OutputMode {OutputType.Extension};
      ELSE
	Msg.Fatal(Msg.POS_VOID, "-om [both|user|extension]");
      END;
    END;
    Msg.debuggingP := M3Args.GetFlag(args, "Verbose");
  END;
  
  rc := M3CFETool.CompileInContext(c);
  IF rc >= 0 THEN
    RTCollector.DisableMotion(); 
    (* Don't want copying until fix use of RefTable *)
    (*TypeNames.Preprocess(c);*)
    M3Context.Apply(c, NEW(ContextClosure, wr := Stdio.stdout),
		    findStandard := FALSE); (* ignore 'standard' unit *)
    RTCollector.EnableMotion(); 
  END;
  RETURN rc;
END DoRun;

BEGIN
  args := M3Args.New("sieg", 
		     "Generate stubs for SPIN Extensions",
		     "1-Nov-95", master := TRUE);
  M3Args.RegisterString(args, "ExtensionTemplate",
			"specifies the path of the Extension template file.");
  M3Args.RegisterString(args, "OutputMode", "one of both, user, or extension.");
  M3Args.RegisterString(args, "DomainName", "Name of the domain this extension belongs to.");
  
  M3Args.RegisterFlag(args, "GenerateCHeader",
		      "generate C header file (FooUser.h).");
  M3Args.RegisterFlag(args, "Verbose", "Output debug messages.");

  
  Process.Exit(ABS(M3ToolFrame.Startup(NEW(M3ToolFrame.Worker, work := DoRun),
				       compile := FALSE)));
END Main.



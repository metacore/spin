(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CallExpr.i3                                           *)
(* Last Modified On Wed May  4 07:35:49 PDT 1994 By kalsow     *)
(*      Modified On Fri Aug  3 02:44:12 1990 By muller         *)

INTERFACE CallExpr;

IMPORT CG, Expr, Type;
IMPORT Target;

PROCEDURE New (proc: Expr.T;  args: Expr.List): Expr.T;

(*******************************************************************)

TYPE
  T <: T_;
  T_ = Expr.T OBJECT
         proc   : Expr.T;
         args   : Expr.List;
         tmp    : CG.Val;         (* for use by the Prep methods *)
         align  : CG.Alignment;   (* for use by the Prep methods *)
       END;

TYPE
  MethodList <: REFANY;

TYPE
  Typer       = PROCEDURE (t: T): Type.T;
  Visitor     = PROCEDURE (t: T);
  TypeChecker = PROCEDURE (t: T;  VAR cs: Expr.CheckState);
  Evaluator   = PROCEDURE (t: T): Expr.T;
  Predicate   = PROCEDURE (t: T): BOOLEAN;
  Compiler    = PROCEDURE (t: T);
  CompilerLV  = PROCEDURE (t: T);
  CompilerBR  = PROCEDURE (t: T;  true, false: CG.Label;  freq: CG.Frequency);
  NoteWriter  = PROCEDURE (t: T);
  Bounder     = PROCEDURE (t: T; VAR min, max: Target.Int);

PROCEDURE NewMethodList
  (minArgs      : INTEGER;
   maxArgs      : INTEGER;
   functional   : BOOLEAN;
   keywords     : BOOLEAN;
   strict       : BOOLEAN;
   fixedType    : Type.T;
   typeOf       : Typer;
   need_addr    : Visitor;
   checker      : TypeChecker;
   prep         : Compiler;
   compiler     : Compiler;
   prepLV       : CompilerLV;
   compilerLV   : CompilerLV;
   prepBR       : CompilerBR;
   compilerBR   : CompilerBR;
   evaluator    : Evaluator;
   isWritable   : Predicate;
   isDesignator : Predicate;
   noteWriter   : NoteWriter;
   bounder      : Bounder := NoBoundsInfo): MethodList;

PROCEDURE Is (e: Expr.T): BOOLEAN;

PROCEDURE IsNever        (t: T): BOOLEAN;
PROCEDURE IsAlways       (t: T): BOOLEAN;
PROCEDURE NoValue        (t: T): Expr.T;
PROCEDURE NotAddressable (t: T);
PROCEDURE PrepArgs       (t: T);
PROCEDURE NoLValue       (t: T);
PROCEDURE NotBoolean     (t: T;  true, false: CG.Label;  freq: CG.Frequency);
PROCEDURE PrepNoBranch   (t: T;  true, false: CG.Label;  freq: CG.Frequency);
PROCEDURE NoBranch       (t: T;  true, false: CG.Label;  freq: CG.Frequency);
PROCEDURE NotWritable    (t: T);
PROCEDURE NoBoundsInfo   (t: T; VAR min, max: Target.Int);

END CallExpr.

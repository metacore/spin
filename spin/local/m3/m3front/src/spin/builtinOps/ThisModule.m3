(* File: ThisModule.m3                                             *)
(* Last Modified On Thu Oct 19 1995 By garrett     	           *)

(*
 * HISTORY
 * 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	functional and bounded
 *
 *)

MODULE ThisModule;

IMPORT CG, CallExpr, Expr, Procedure, Target, Module, RTCodeModule;

VAR Z: CallExpr.MethodList;

PROCEDURE Check(<*UNUSED*> ce: CallExpr.T; <*UNUSED*> VAR cs: Expr.CheckState) =
BEGIN
    (* skip *)
END Check;

PROCEDURE Prep (<*UNUSED*> ce: CallExpr.T) =
BEGIN
    (* skip *)
END Prep;

(* Find the current module's ID and load the beginning of its
   descriptor. *)
PROCEDURE Compile (<*UNUSED*> ce: CallExpr.T) =
BEGIN
    CG.Load_addr_of(Module.GlobalData(NIL), 0, Target.Byte);
END Compile;

PROCEDURE Initialize () =
BEGIN
    Z := CallExpr.NewMethodList (0, 0, TRUE, FALSE, TRUE, RTCodeModule.ModuleDesc,
				 NIL, 
				 CallExpr.NotAddressable,  
				 Check,
				 Prep,
				 Compile,
				 CallExpr.NoLValue,
				 CallExpr.NoLValue,
				 CallExpr.NotBoolean,
				 CallExpr.NotBoolean,
				 CallExpr.NoValue, (* no constant folding *)
				 CallExpr.IsNever, (* writable *)
				 CallExpr.IsNever, (* designator *)
				 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("THIS_MODULE", Z, TRUE,
                      isBounded := TRUE,
                      isFunctional := TRUE);
END Initialize;

BEGIN
END ThisModule.

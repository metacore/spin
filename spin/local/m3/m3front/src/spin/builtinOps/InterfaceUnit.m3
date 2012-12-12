(* File: InterfaceUnit.m3                                             *)

(*
 * HISTORY
 * 25-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Add constant folding so we can use INTERFACE_UNIT in an interface
 *	file.
 *
 * 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	functional and bounded
 *
 * 19-Oct-95  Charles Garrett (garrett) at the University of Washington
 *	Created.
 *)

MODULE InterfaceUnit;

IMPORT CG, CallExpr, Expr, Procedure, Target, Module, Error, M3ID, NamedExpr;
IMPORT RTCodeModule, InterfaceExpr;

VAR Z: CallExpr.MethodList;

PROCEDURE Check(ce: CallExpr.T; <*UNUSED*> VAR cs: Expr.CheckState) =
VAR
    e := ce.args[0];         (* The first and only argument *)
    name : M3ID.T;           (* Interface identifier *)
BEGIN
    (* Get identifier from expression *)
    IF NamedExpr.SplitName(e, name) THEN 
	IF Module.GetImportedInterface(name) = NIL THEN
	    Error.Txt("INTERFACE_UNIT", M3ID.ToText(name) & "is not an imported interface");
        END;
    ELSE
	Error.Txt("INTERFACE_UNIT", "Argument must be a name");
    END;
END Check;

PROCEDURE Prep (<*UNUSED*> ce: CallExpr.T) =
BEGIN
    (* skip *)
END Prep;

(* Find the ID of the interface in the argument list and load the 
   beginning of its descriptor. *)
PROCEDURE Compile (ce: CallExpr.T) =
  VAR
    e := ce.args[0];         (* The first and only argument *)
    name : M3ID.T;           (* Interface identifier *)
    interface : Module.T;    (* big data structure describing the interface *)
  BEGIN
    EVAL NamedExpr.SplitName(e, name);

    interface := Module.GetImportedInterface(name);
    CG.Load_addr_of(Module.GlobalData(interface), 0, Target.Byte);
END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR
    e := ce.args[0];         (* The first and only argument *)
    name : M3ID.T;           (* Interface identifier *)
    interface : Module.T;    (* big data structure describing the interface *)
  BEGIN
    EVAL NamedExpr.SplitName(e, name);

    interface := Module.GetImportedInterface(name);
    RETURN InterfaceExpr.New(interface);
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, TRUE, RTCodeModule.InterfaceDesc,
				 NIL, 
				 CallExpr.NotAddressable,  
				 Check,
				 Prep,
				 Compile,
				 CallExpr.NoLValue,
				 CallExpr.NoLValue,
				 CallExpr.NotBoolean,
				 CallExpr.NotBoolean,
				 Fold,
				 CallExpr.IsNever, (* writable *)
				 CallExpr.IsNever, (* designator *)
				 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("INTERFACE_UNIT", Z, TRUE,
                      isBounded := TRUE,
                      isFunctional := TRUE);
  END Initialize;

BEGIN
END InterfaceUnit.

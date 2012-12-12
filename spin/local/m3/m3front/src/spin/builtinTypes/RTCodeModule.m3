(* File: RTCodeModule.m3                           *)
(* Last Modified On Tue Oct 24 1995 By garrett     *)

MODULE RTCodeModule;

IMPORT Addr, Scope, Tipe, Module;

PROCEDURE Initialize () =
VAR 
    zz: Scope.T;  
BEGIN
    M := Module.NewDefn("RTCodeBuiltin", TRUE, NIL);

    (* WARNING: The following list must be in the same order
        as the actual RTCode.i3 file, otherwise the version
        stamps will be messed up! *)
    zz := Scope.Push(Module.ExportScope(M));
    InterfaceDesc := Tipe.DefineBuiltinOpaque("Interface", Addr.T, FALSE);
    ModuleDesc := Tipe.DefineBuiltinOpaque("Module", Addr.T, FALSE);
    Scope.Pop (zz);
END Initialize;

BEGIN
END RTCodeModule.

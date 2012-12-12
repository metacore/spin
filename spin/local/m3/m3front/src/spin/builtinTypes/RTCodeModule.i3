(* File: RTCodeModule.i3                           *)
(* Last Modified On Tue Oct 24 1995 By garrett     *)

INTERFACE RTCodeModule;

IMPORT Module, Type;

VAR 
    M: Module.T;
    ModuleDesc, InterfaceDesc: Type.T; 

PROCEDURE Initialize ();

END RTCodeModule.

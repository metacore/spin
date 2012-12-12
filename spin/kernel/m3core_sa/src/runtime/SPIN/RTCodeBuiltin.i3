(* 10-26-95  Charlie Garrett
	RTCodeBuiltin declares two types which the compiler
	knows about for representing modules and interfaces 
	at runtime. THIS_MODULE() returns a Module, and 
	INTERFACE_UNIT(X) returns an Interface.
*)

INTERFACE RTCodeBuiltin;

TYPE
    Module <: ADDRESS;
    Interface <: ADDRESS;

END RTCodeBuiltin.

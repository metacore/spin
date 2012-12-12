(* This interface is unsafe because it declares external declarations *)

UNSAFE INTERFACE RTMachineExtern;

FROM RTMachine IMPORT State;

<*EXTERNAL "__setjmp" *> PROCEDURE setjmp(VAR s: State): INTEGER;

END RTMachineExtern.
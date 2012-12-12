(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-Oct-95  Charlie Garrett (garrett) at the University of Washington
 *      Created.
 *)

UNSAFE INTERFACE RTMachineExtern;

FROM RTMachine IMPORT State;

<*EXTERNAL "_setjmp" *> PROCEDURE setjmp(VAR s: State): INTEGER;

END RTMachineExtern.



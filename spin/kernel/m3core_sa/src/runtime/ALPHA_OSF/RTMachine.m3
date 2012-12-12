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

UNSAFE MODULE RTMachine;

IMPORT RTMachineExtern;

PROCEDURE SaveState(VAR s: State): INTEGER =
  BEGIN
    RETURN RTMachineExtern.setjmp(s);
  END SaveState;

BEGIN
END RTMachine.

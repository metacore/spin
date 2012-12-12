(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)

UNSAFE MODULE RTMachine;

IMPORT RTMachineExtern;

PROCEDURE SaveState(VAR s: State): INTEGER =
  BEGIN
    RETURN RTMachineExtern.setjmp(s);
  END SaveState;

BEGIN
END RTMachine.

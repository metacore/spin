(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * RTOSMachine.m3
 *
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	The C procedure for Debugger takes an argument so I added one
 *	here.  For backwards compatibility with old M3 code I gave it
 *	a default value.  Because external procedures cannot take
 *	default values, I split the procedure into an M3 procedure
 *	and an external procedure.
 *
 *)

UNSAFE MODULE RTOSMachine;

PROCEDURE Debugger(msg: ADDRESS := NIL) =
  BEGIN
    DebuggerExternal(msg);
  END Debugger;

BEGIN
END RTOSMachine.

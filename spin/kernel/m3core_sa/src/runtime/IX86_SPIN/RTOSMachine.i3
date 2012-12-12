(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * RTOSMachine.i3
 *
 * This interface contains the few low level systems services and data 
 * structures that are exported by the kernel and needed by the m3 standalone 
 * runtime.  Some of the definitions in here are duplicates of those found in 
 * interfaces over in sys land, but putting them here allows us to have a 
 * simpler build procedure (at the risk of some redundancy).
 *
 * HISTORY
 * 19-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Added time of day.
 *
 * 19-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Added cycle counter ops.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Removed OKToAllocAtSPL variable
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	The C procedure for Debugger takes an argument so I added one
 *	here.  For backwards compatibility with old M3 code I gave it
 *	a default value.  Because external procedures cannot take
 *	default values, I split the procedure into an M3 procedure
 *	and an external procedure.
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 * This module is UNSAFE because it includes EXTERNALS.
 *)

UNSAFE INTERFACE RTOSMachine;
IMPORT Word;

TYPE InterruptClass = {
  Low,
  SoftClock,
  IO,
  Clock,
  High
};

TYPE InterruptLevel = Word.T;

<*EXTERNAL*> PROCEDURE IsLow(mask: InterruptLevel) : BOOLEAN;
<*EXTERNAL*> PROCEDURE IsHigh(mask: InterruptLevel) : BOOLEAN;
<*EXTERNAL RTSetInterruptMask*> PROCEDURE SetInterruptMask(class: InterruptClass)
                                            : InterruptLevel;
<*EXTERNAL RTRestoreInterruptMask*> PROCEDURE RestoreInterruptMask(mask: InterruptLevel);

PROCEDURE Debugger(msg: ADDRESS := NIL);

<* EXTERNAL Debugger *> PROCEDURE DebuggerExternal(msg: ADDRESS);

<* EXTERNAL *> PROCEDURE AddPCBypass(p1, p2: ADDRESS; nArgs: INTEGER): ADDRESS;


PROCEDURE CycleCounter(): Word.T;
PROCEDURE CycleToMicrosec(cycles: Word.T) : Word.T;
PROCEDURE CycleMinus(stop,start: Word.T): Word.T;
PROCEDURE TimeOfDay(): Word.T; (* returns time of day converted to us *)
END RTOSMachine.




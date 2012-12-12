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
 * 15-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Added interface to cycle counter ops.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added halt.
 *
 * 26-Oct-95 Brian Bershad (bershad) at the University of Washington
 *	Moved qpapa here. Gave it a proper english name.
 *
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Machine-dependent structure definitions.
 *      These definitions MUST match those in the kernel machine directory.
 *	We really need to import directly from the kernel interface area,
 *	but it's not clear how to do that just yet.
 *
 * This module is UNSAFE because it includes EXTERNALS.
 *)

UNSAFE INTERFACE RTOSMachine;
IMPORT Word;

TYPE InterruptLevel = {
  Low,
  SoftClock,
  SPL_2, SPL_3, 
  IO,
  Clock,
  SPL_6,
  High
};

TYPE InterruptClass = InterruptLevel; (* XX interruptclass is obsolete!! *)
  
<*EXTERNAL*> PROCEDURE IsLow(mask: InterruptLevel) : BOOLEAN;
<*EXTERNAL*> PROCEDURE IsHigh(mask: InterruptLevel) : BOOLEAN;
<*EXTERNAL SetInterruptMask*> PROCEDURE SetInterruptMask(class: InterruptLevel)
                                            : InterruptLevel;
<*EXTERNAL RestoreInterruptMask*> PROCEDURE RestoreInterruptMask(mask: InterruptLevel);

<* EXTERNAL *> PROCEDURE Debugger();
<* EXTERNAL *> VAR OKToAllocAtSPL: BOOLEAN;

<* EXTERNAL *> PROCEDURE AddPCBypass(p1, p2: ADDRESS; nArgs: INTEGER): ADDRESS;

<* EXTERNAL halt *> PROCEDURE Halt();

<* EXTERNAL *>
VAR
  refcount_start  : ADDRESS;
  refcount_end    : ADDRESS;
  irefcount_start : ADDRESS;
  irefcount_end   : ADDRESS;

PROCEDURE Yield();


PROCEDURE CycleCounter(): Word.T;
PROCEDURE CycleToMicrosec(cycles: Word.T) : Word.T;
PROCEDURE CycleMinus(stop,start: Word.T): Word.T;
PROCEDURE TimeOfDay(): Word.T; (* returns time of day converted to us *)
END RTOSMachine.




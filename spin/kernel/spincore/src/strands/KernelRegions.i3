(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 28-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Support for special kernel code regions. Namely, restartable
 *	atomic regions and non-preemptible regions.
 *)
INTERFACE KernelRegions;
IMPORT CodeRegions, Word;

VAR
  RASRegions: CodeRegions.T;
  NonPreemptibleRegions: CodeRegions.T;

(*
 * Return true if the PC is somewhere in the SPIN kernel
 *)
PROCEDURE InKernelLand(pc: Word.T) : BOOLEAN;

(*
 * Return true if the PC is in user land
 *)
PROCEDURE InUserLand(pc: Word.T) : BOOLEAN;

END KernelRegions.


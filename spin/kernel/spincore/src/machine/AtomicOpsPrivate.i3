(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 02-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Private interface to set up the atomic operations.
 *)
INTERFACE AtomicOpsPrivate;
IMPORT CodeRegions;

PROCEDURE InitKernelRASRegions(ras: CodeRegions.T);

PROCEDURE Init(verbose: BOOLEAN);

END AtomicOpsPrivate.

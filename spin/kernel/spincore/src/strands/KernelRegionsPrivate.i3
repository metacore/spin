(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Support for special kernel code regions. Namely, restartable
 *	atomic regions and non-preemptible regions.
 *)
INTERFACE KernelRegionsPrivate;

(* Initialize the special kernel regions *)
PROCEDURE Init(verbose: BOOLEAN);

END KernelRegionsPrivate.

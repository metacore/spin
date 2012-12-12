(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 22-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Changed syscall signature to take strand argument explicitly.
 *
 * 04-Nov-94  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Default syscall handlers.
 *) 
INTERFACE CTSyscall;
IMPORT CPU, Strand;

PROCEDURE Syscall(strand: Strand.T; VAR ms: CPU.SavedState);

END CTSyscall.

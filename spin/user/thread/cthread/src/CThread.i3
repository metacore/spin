(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 22-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed the Syscall trap signature.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Cthreads for users, implemented on top of userspace threads.
 *) 
INTERFACE CThread;
IMPORT CPU, Strand, Word;

PROCEDURE Fork(strand: Strand.T; VAR state: CPU.SavedState) : Word.T;

PROCEDURE Exit(strand: Strand.T; VAR state: CPU.SavedState);

PROCEDURE Join(strand: Strand.T; VAR state: CPU.SavedState) : Word.T;

END CThread.

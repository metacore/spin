(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95 Emin Gun Sirer (egs) at the University of Washington
 *	Created. External declarations for kernel threads.
 *)
UNSAFE (* for externals *)
INTERFACE MachineThreadExtern;
IMPORT Thread, Word;

<*EXTERNAL*>PROCEDURE Startup(t: REFANY);

<*EXTERNAL*>PROCEDURE CallContinuation(thread: Thread.T;
                           bodystarter: PROCEDURE(a:Thread.T);
                           newsp: Word.T);

END MachineThreadExtern.

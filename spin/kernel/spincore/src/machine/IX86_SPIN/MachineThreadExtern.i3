(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)
UNSAFE (* for externals *)
INTERFACE MachineThreadExtern;
IMPORT Thread, Word;

<*EXTERNAL*>PROCEDURE Startup(t: REFANY);

<*EXTERNAL*>PROCEDURE CallContinuation(thread: Thread.T;
                           bodystarter: PROCEDURE(a:Thread.T);
                           newsp: Word.T);

(* 
   clk_int is used as a flag so we know when a thread switch 
   is due to a clock int. Declared in machine/IX86_SPIN/Core.S 
*)
<*EXTERNAL*> VAR clk_int: BOOLEAN;
END MachineThreadExtern.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Brought bsd unix interfaces into sphinx from urt
 *)
INTERFACE Sig;
IMPORT BsdSignal;
IMPORT Proc;
IMPORT BSDtty;

PROCEDURE SetDefaultHandlers(VAR x : ARRAY [0..BsdSignal.Max] OF Proc.SigArgs);
  
PROCEDURE SetSigHandler(tty: BSDtty.T);
  
PROCEDURE Check(proc: Proc.T);
(* Send (if any) signal to the process. "proc" MUST be the current
   process, i.e., "proc" must be "Proc.Self()". "proc" is explicitly passed
   only to save some amount of computation. *)

  
END Sig.

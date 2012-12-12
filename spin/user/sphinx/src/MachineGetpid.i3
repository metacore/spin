(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE MachineGetpid;
IMPORT CPU;

PROCEDURE ReturnValues (VAR ss : CPU.SavedState; pid, ppid : INTEGER);
(* getpid returns two values, pid and ppid. This is an arch dependent
 proc to return two values through registers.
 This proc is also used in getgid *)
 
END MachineGetpid.

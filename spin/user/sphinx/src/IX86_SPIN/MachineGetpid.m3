(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE MachineGetpid;
IMPORT CPU;

PROCEDURE ReturnValues (VAR ss : CPU.SavedState; pid, ppid : INTEGER) =
  BEGIN
    ss.eax := pid;
    ss.edx := ppid;
  END ReturnValues;
		      
BEGIN
END MachineGetpid.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE MachineSyscallTrace;
IMPORT Strand;
IMPORT CPU;

TYPE Args =
  RECORD
    syscallNum, retval: INTEGER;
    args: ARRAY [0 .. 7] OF INTEGER;
  END;
  
PROCEDURE GetArgs(strand: Strand.T;
		  READONLY ss: CPU.SavedState;
		  (*OUT*)VAR arg: Args);
  (* Get the arguments to systemcall into "args" *)
  
END MachineSyscallTrace.

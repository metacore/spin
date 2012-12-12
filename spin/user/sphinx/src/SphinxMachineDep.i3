(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE SphinxMachineDep;
IMPORT CPU;
IMPORT Proc;
IMPORT VMError;

PROCEDURE GetpidReturn(VAR ss: CPU.SavedState;
		       pid, ppid: INTEGER);
PROCEDURE SetupSignalContext(proc: Proc.T; signo: INTEGER;
			     args: Proc.SigArgs) RAISES {VMError.E};
PROCEDURE ForkReturn(parentPid: INTEGER;
		     childPid: INTEGER; 
		     VAR parentState: CPU.SavedState;
		     VAR childState: CPU.GeneralRegs);

PROCEDURE Syscall_Return(VAR ss: CPU.SavedState; ret: INTEGER);

END SphinxMachineDep.

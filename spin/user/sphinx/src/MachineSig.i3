(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE MachineSig;
IMPORT Proc;
IMPORT VMError;

PROCEDURE SetupUserContext(proc: Proc.T; signo: INTEGER; sig: Proc.SigArgs)
  	RAISES {VMError.E};
	       
END MachineSig.

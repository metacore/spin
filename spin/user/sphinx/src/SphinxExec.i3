INTERFACE SphinxExec;
IMPORT Strand, CPU, Error, Errno, VMError;

PROCEDURE Execve (us : Strand.T; VAR state : CPU.SavedState)
  RAISES {Errno.E, VMError.E, Error.E};

PROCEDURE ForkReturn(parentPid, childPid : INTEGER; 
  VAR parentState : CPU.SavedState;
  VAR childState : CPU.GeneralRegs);

END SphinxExec.

INTERFACE SyscallTrace;

IMPORT Proc;
IMPORT Dispatcher;

TYPE Output = {Log, Console};
  
PROCEDURE Install(proc: Proc.T := NIL): Dispatcher.Binding;
PROCEDURE Uninstall();
VAR
  output: Output;
END SyscallTrace.

INTERFACE MachineDebugger;
IMPORT Strand, CPU;

PROCEDURE CopySavedUserRegs (    s    : Strand.T;
                             VAR state: CPU.MachineState);

END MachineDebugger.

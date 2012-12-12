MODULE USTFPU;
IMPORT CPU;

PROCEDURE Setup(fpustate: REF CPU.FloatRegs) =
  BEGIN
    CPU.EnableUserFloatOps(TRUE);
    CPU.GetUserFloatRegs(fpustate^);
  END Setup;

BEGIN
END USTFPU.

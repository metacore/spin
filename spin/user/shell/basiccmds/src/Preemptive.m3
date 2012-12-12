(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 4-oct-96  becker at the University of Washington
 *	Added /proc files
 *
 * 04-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Enable/disable preemption.
 *)
MODULE Preemptive;
IMPORT SchedPrivate, IO, ParseParams;
IMPORT InfoFile, Wr, Error;

PROCEDURE Enabled(b: BOOLEAN) : TEXT =
  BEGIN
    IF b = TRUE THEN RETURN "enabled" ELSE RETURN "disabled"; END;
  END Enabled;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    IF pp.keywordPresent("user") THEN
      SchedPrivate.userPreemptive := NOT SchedPrivate.userPreemptive;
    ELSIF pp.keywordPresent("kernel") THEN
      SchedPrivate.kernelPreemptive := NOT SchedPrivate.kernelPreemptive;
    END;
    IO.Put("User preemption "&Enabled(SchedPrivate.userPreemptive)&"\n");
    IO.Put("Kernel preemption "&Enabled(SchedPrivate.kernelPreemptive)&"\n");
    RETURN TRUE;
  END Run;

PROCEDURE Preemption (wr: Wr.T) =
  BEGIN
    Wr.PutText(wr,"User preemption "&Enabled(SchedPrivate.userPreemptive)&"\n");
    Wr.PutText(wr,"Kernel preemption "&Enabled(SchedPrivate.kernelPreemptive)&"\n");
  END Preemption;

BEGIN 
  TRY
     InfoFile.Create("/proc/preemption",Preemption);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("disp procfs files:" & e.message() & "\n");
  END;
END Preemptive.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 4-oct-96  becker at the University of Washington
 *	Added /proc files
 *
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Printing pc value in hex to ensure that the user has entered
 *	right pc.
 *
 * 04-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Shell interface to manipulate kernel preemptible/ras regions.
 *)
MODULE RasControl;

IMPORT KernelRegions, Region, ParseParams, Word;
IMPORT IO, Fmt;
IMPORT InfoFile, Wr, ThreadExtra, Error;

PROCEDURE DumpRegion(regno: INTEGER; READONLY region: Region.T) =
  BEGIN
    IO.Put("Region #");
    IO.Put(Fmt.Int(regno));
    IO.Put(" pc=0x");
    IO.Put(Fmt.Unsigned(region.begin));
    IO.Put(" len=0x");
    IO.Put(Fmt.Unsigned(region.length));
    IO.Put("\n");
  END DumpRegion;

PROCEDURE Dump() =
  BEGIN
    IO.Put("RAS regions:\n");
    KernelRegions.RASRegions.iterate(DumpRegion);
    IO.Put("Non-preemptible regions:\n");
    KernelRegions.NonPreemptibleRegions.iterate(DumpRegion);
  END Dump;

PROCEDURE RegionAdd (pp: ParseParams.T) RAISES {ParseParams.Error}=
  VAR region: Region.T;
  BEGIN
    region.begin := pp.getNextInt();
    region.length := pp.getNextInt();
    KernelRegions.NonPreemptibleRegions.add(region);
  END RegionAdd;

PROCEDURE RegionCheck (pp: ParseParams.T) RAISES {ParseParams.Error} =
  VAR pc: Word.T;
  BEGIN
    pc := pp.getNextInt();
    IF KernelRegions.NonPreemptibleRegions.contains(pc) THEN
      IO.Put("The value ");
      IO.Putx(pc);
      IO.Put(" given is in a non-preemptible range.\n");
    ELSE
      IO.Put("The value ");
      IO.Putx(pc);
      IO.Put(" given is not in a non-preemptible range.\n");
    END;
  END RegionCheck;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      IF pp.keywordPresent("dump") THEN
        Dump();
      ELSIF pp.keywordPresent("np") THEN
        RegionAdd(pp);
      ELSIF pp.keywordPresent("check") THEN
        RegionCheck(pp);
      ELSE
        RAISE ParseParams.Error;
      END;
    EXCEPT
      ParseParams.Error =>
        IO.Put(CommandName & CommandHelp & "\n");
        RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;

PROCEDURE RasRegions (wr: Wr.T) =
  VAR
    realWr := ThreadExtra.SetWrSelf(wr);
  BEGIN
    Dump();
    EVAL ThreadExtra.SetWrSelf(realWr);
  END RasRegions;


BEGIN
  TRY
     InfoFile.Create("/proc/ras_regions",RasRegions);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("gc procfs files:" & e.message() & "\n");
  END;
END RasControl.

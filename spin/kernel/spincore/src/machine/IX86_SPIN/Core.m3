(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Kernel and CPU interfaces
 *
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *) 
UNSAFE MODULE Core;
IMPORT CoreExtern, IO, Region, KernelRegions, Word;

PROCEDURE InitNonPreemptive() =
  VAR region: Region.T;
  BEGIN
    WITH begin = LOOPHOLE(CoreExtern.corebegin, Word.T),
         end   = LOOPHOLE(CoreExtern.coreend, Word.T)
     DO
      region.begin  := begin;
      region.length := end - begin;
    END;
    KernelRegions.NonPreemptibleRegions.add(region);
  END InitNonPreemptive;

PROCEDURE Init(verbose: BOOLEAN) =
  BEGIN
    InitNonPreemptive();
    IF verbose THEN
      IO.Put("Core initialized...\n");
    END;
  END Init;

BEGIN
END Core.

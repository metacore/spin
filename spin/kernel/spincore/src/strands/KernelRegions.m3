(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Sal interfaces
 *
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Name clean up.
 *
 * 28-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Support for special Sal code regions. Namely, restartable
 *	atomic regions and non-preemptible regions.
 *)
UNSAFE (* need to access salextern to find sal bounds *)
MODULE KernelRegions EXPORTS KernelRegions, KernelRegionsPrivate;
IMPORT CodeRegions, AtomicOpsPrivate, SalExtern, Word;
IMPORT IO;

PROCEDURE InKernelLand(pc: Word.T) : BOOLEAN =
  BEGIN
    RETURN Word.GT(pc, LOOPHOLE(SalExtern.salend, Word.T));
  END InKernelLand;

PROCEDURE InUserLand(pc: Word.T) : BOOLEAN =
  BEGIN
    RETURN Word.LT(pc, SalExtern.salbegin);
  END InUserLand;

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN
    RASRegions := CodeRegions.New();
    AtomicOpsPrivate.InitKernelRASRegions(RASRegions);

    NonPreemptibleRegions := CodeRegions.New();
    IF verbose THEN
      IO.Put("Sal code regions initialized...\n");
    END;
  END Init;

BEGIN
END KernelRegions. 

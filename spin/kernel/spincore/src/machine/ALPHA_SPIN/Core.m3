(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Moved the initialization of the Core.s nonpreemtible regions into
 *	this module.
 *
 * 28-Nov-94  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Interface to the interrupt management code.
 *) 
UNSAFE (* to import externals *)
MODULE Core;
IMPORT CoreExtern, CPU, Region, KernelRegions, Word;
IMPORT IO;

PROCEDURE RedirectTraps() =
  BEGIN
    CoreExtern.Wrent(CoreExtern.TRAP_entInt, CPU.TrapType.KNIO);
    CoreExtern.Wrent(CoreExtern.TRAP_entArith, CPU.TrapType.ARITHMETIC);
    CoreExtern.Wrent(CoreExtern.TRAP_entUna, CPU.TrapType.UNALIGNED);
    CoreExtern.Wrent(CoreExtern.TRAP_entSys, CPU.TrapType.SYSCALL);
    CoreExtern.Wrent(CoreExtern.TRAP_entMM, CPU.TrapType.MEMORYMANAGEMENT);
    (*CoreExtern.Wrent(CoreExtern.TRAP_entIF,CPU.TrapType.INSNFAULT);*)
  END RedirectTraps;

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
    RedirectTraps();
    IF verbose THEN
      IO.Put("Core initialized...\n");
    END;
  END Init;

BEGIN
END Core.

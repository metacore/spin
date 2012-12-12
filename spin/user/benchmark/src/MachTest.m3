(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-Jan-96  Stefan Savage (savage) at the University of Washington
 *	Created to test Mach interfaces
 *
 *)
MODULE MachTest;

IMPORT VirtAddr, MachineMem, Mach, IO, Fmt, PhysAddr;

PROCEDURE Test() =
  VAR t: Mach.TaskT;
      regionBegin: VirtAddr.Address := 16_0800000000;
      regionSize : VirtAddr.Size := 10 * MachineMem.PAGESIZE;
  BEGIN

    IO.Put("FreeMem = "&Fmt.Unsigned(PhysAddr.FreeMem())&"\n"); 
    FOR j := 1 TO 2 DO
      EVAL Mach.TaskCreate(NIL, FALSE, t);
      FOR i := 1 TO 1 DO
        IO.Put("FreeMem = "&Fmt.Unsigned(PhysAddr.FreeMem())&"\n"); 
        
        regionBegin := 16_0800000000;
        EVAL Mach.VMAllocate(t, regionBegin, regionSize, FALSE);
        IO.Put("regionBegin = "&Fmt.Unsigned(regionBegin)&"\n"); 
        EVAL Mach.VMAllocate(t, regionBegin, regionSize, TRUE); 
        IO.Put("regionBegin2 = "&Fmt.Unsigned(regionBegin)&"\n"); 
        
        EVAL Mach.VMDeallocate(t, regionBegin, regionSize);  
        EVAL Mach.VMAllocate(t, regionBegin, regionSize, TRUE); 
        IO.Put("regionBegin4 = "&Fmt.Unsigned(regionBegin)&"\n"); 

        regionBegin := 16_0900000000;
        EVAL Mach.VMAllocate(t, regionBegin, regionSize, FALSE);
        IO.Put("regionBegin5 = "&Fmt.Unsigned(regionBegin)&"\n"); 
      END;
      EVAL Mach.TaskTerminate(t);
      IO.Put("FreeMem = "&Fmt.Unsigned(PhysAddr.FreeMem())&"\n");
    END; 
  END Test;

BEGIN
END MachTest.




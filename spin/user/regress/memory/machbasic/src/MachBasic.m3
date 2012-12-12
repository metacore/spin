(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-May-96  Stefan Savage (savage) at the University of Washington
 *	Added a couple tests
 *
 * 09-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *
 *
 *)


(* Note: these tests will fail in the latest SPIN because
   pages are freed asynchronously. We should just get rid of this
   test. -- yaz. *)

   
MODULE MachBasic;
IMPORT IO;
IMPORT VirtAddr, MachineMem, Mach, Fmt, PhysAddr, CPU, Word;

(* BUGBUG: These tests don't catch slow leaks *)

PROCEDURE Test1(): BOOLEAN =
  VAR
    task: Mach.TaskT;
    startMem, endMem: Word.T;
  BEGIN
    IO.Put("Test1.  Sequentially creating and destroying new address spaces\n");
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    FOR i:= 1 TO 50 DO
      EVAL Mach.TaskCreate(NIL, FALSE, task);
      EVAL Mach.TaskTerminate(task);
    END;
    endMem := PhysAddr.FreeMem();
    IO.Put("Ending physical memory = "&Fmt.Int(endMem)&" pages \n");
    IF endMem + 8 < startMem THEN (* 8 is the magic min_pte buffer number *)
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Test1;

PROCEDURE Test2(): BOOLEAN =
  VAR
    task: REF ARRAY OF Mach.TaskT;
    startMem, endMem: Word.T;
  BEGIN
    IO.Put("Test2.  Creating 50 new address spaces and then deleting them\n");
    task := NEW(REF ARRAY OF Mach.TaskT, 50);
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    FOR i:= 0 TO 49 DO
      EVAL Mach.TaskCreate(NIL, FALSE, task[i]);
    END;
    FOR i:= 0 TO 49 DO
      EVAL Mach.TaskTerminate(task[i]);
    END;

    endMem := PhysAddr.FreeMem();
    IO.Put("Ending physical memory = "&Fmt.Int(endMem)&" pages \n");
    IF endMem +8 < startMem THEN (* 8 is the magic min_pte buffer number *)
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Test2;

PROCEDURE Test3(): BOOLEAN =
  VAR
    task: Mach.TaskT;
    startMem, endMem: Word.T;
    regionBegin: VirtAddr.Address := 16_0800000000;
    regionSize : VirtAddr.Size := 10 * CPU.PAGESIZE;
  BEGIN
    IO.Put("Test3.  Simple allocating and deallocating of memory\n");
    startMem := PhysAddr.FreeMem();
    EVAL Mach.TaskCreate(NIL, FALSE, task);
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    FOR i := 1 TO 50 DO
        EVAL Mach.VMAllocate(task, regionBegin, regionSize, TRUE);
        EVAL Mach.VMDeallocate(task, regionBegin, regionSize);
    END;
    EVAL Mach.TaskTerminate(task);
    endMem := PhysAddr.FreeMem();
    IO.Put("Ending physical memory = "&Fmt.Int(endMem)&" pages \n");
    IF endMem +8 < startMem THEN (* 8 is the magic min_pte buffer number *)
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Test3;

PROCEDURE Test4(): BOOLEAN =
  VAR
    task: Mach.TaskT;
    startMem, endMem: Word.T;
    regionBegin: REF ARRAY OF VirtAddr.Address;
    regionSize : VirtAddr.Size := 5 * CPU.PAGESIZE;
  BEGIN
    regionBegin := NEW(REF ARRAY OF VirtAddr.Address, 50);
    IO.Put("Test4.  Allocating and deallocating more memory\n");
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    EVAL Mach.TaskCreate(NIL, FALSE, task);
    FOR i := 0 TO 49 DO
        EVAL Mach.VMAllocate(task, regionBegin[i], regionSize, TRUE);
    END;
    FOR i := 0 TO 49 DO
        EVAL Mach.VMDeallocate(task, regionBegin[i], regionSize);
    END;
    EVAL Mach.TaskTerminate(task);
    endMem := PhysAddr.FreeMem();
    IO.Put("Ending physical memory = "&Fmt.Int(endMem)&" pages \n");
    IF endMem + 8 < startMem THEN (* 8 is the magic min_pte buffer number *)
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Test4;

PROCEDURE Test5(): BOOLEAN =
  VAR
    task: Mach.TaskT;
    startMem, endMem: Word.T;
    regionBegin: REF ARRAY OF VirtAddr.Address;
    regionSize : VirtAddr.Size := 5 * CPU.PAGESIZE;
  BEGIN
    regionBegin := NEW(REF ARRAY OF VirtAddr.Address, 50);
    IO.Put("Test5.  Allocating lots of memory and deallocating it at once\n");
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    EVAL Mach.TaskCreate(NIL, FALSE, task);
    FOR i := 0 TO 49 DO
        EVAL Mach.VMAllocate(task, regionBegin[i], regionSize, TRUE);
    END;
    EVAL Mach.VMDeallocate(task, regionBegin[0], regionSize*50);
    EVAL Mach.TaskTerminate(task);
    endMem := PhysAddr.FreeMem();
    IO.Put("Ending physical memory = "&Fmt.Int(endMem)&" pages \n");
    IF endMem + 8 < startMem THEN (* 8 is the magic min_pte buffer number *)
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Test5;

PROCEDURE Test6(): BOOLEAN =
  VAR
    task: Mach.TaskT;
    startMem, endMem: Word.T;
    regionBegin: REF ARRAY OF VirtAddr.Address;
    regionSize : VirtAddr.Size := 5 * CPU.PAGESIZE;
    regionSize1 : VirtAddr.Size := 2 * CPU.PAGESIZE;
    regionSize2 : VirtAddr.Size := 2 * CPU.PAGESIZE;
    regionSize3 : VirtAddr.Size := 3 * CPU.PAGESIZE;
  BEGIN
    regionBegin := NEW(REF ARRAY OF VirtAddr.Address, 50);
    IO.Put("Test6.  Allocating and deallocating from center of region\n");
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    EVAL Mach.TaskCreate(NIL, FALSE, task);
    FOR i := 0 TO 9 DO
        EVAL Mach.VMAllocate(task, regionBegin[i], regionSize, TRUE);
    END;
    FOR i := 0 TO 9 DO
      EVAL Mach.VMDeallocate(task,regionBegin[i]+regionSize2,
                             regionSize-regionSize2);
    END;
    endMem := PhysAddr.FreeMem();
    IO.Put("1) physical memory = "&Fmt.Int(endMem)&" pages \n");

    FOR i := 0 TO 9 DO
        EVAL Mach.VMAllocate(task, regionBegin[i], regionSize, TRUE);
    END;
    FOR i := 0 TO 9 DO
      EVAL Mach.VMDeallocate(task,regionBegin[i]-regionSize2,
                             regionSize-regionSize2);
    END;
    endMem := PhysAddr.FreeMem();
    IO.Put("2) physical memory = "&Fmt.Int(endMem)&" pages \n");
    FOR i := 0 TO 9 DO
        EVAL Mach.VMAllocate(task, regionBegin[i], regionSize, TRUE);
    END;
    FOR i := 0 TO 9 DO
      EVAL Mach.VMDeallocate(task,regionBegin[i]+regionSize2,
                             regionSize+regionSize2);
    END;
    endMem := PhysAddr.FreeMem();
    IO.Put("3) physical memory = "&Fmt.Int(endMem)&" pages \n");
    EVAL Mach.TaskTerminate(task);
    endMem := PhysAddr.FreeMem();
    IO.Put("Ending physical memory = "&Fmt.Int(endMem)&" pages \n");
    IF endMem + 8 < startMem THEN (* 8 is the magic min_pte buffer number *)
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Test6;

PROCEDURE MachBasic(): BOOLEAN =
  VAR t: Mach.TaskT;
      regionBegin: VirtAddr.Address := 16_0800000000;
      regionSize : VirtAddr.Size := 10 * CPU.PAGESIZE;
  BEGIN
    IF Test1() = FALSE THEN
      RETURN FALSE;
    END;
    IF Test2() = FALSE THEN
      RETURN FALSE;
    END;
    IF Test3() = FALSE THEN
      RETURN FALSE;
    END;
    IF Test4() = FALSE THEN
      RETURN FALSE;
    END;

   IF Test5() = FALSE THEN
      RETURN FALSE;
    END;

   IF Test6() = FALSE THEN
      RETURN FALSE;
    END;

    RETURN TRUE;
    IO.Put("FreeMem = "&Fmt.Unsigned(PhysAddr.FreeMem())&"\n"); 
    FOR j := 1 TO 2 DO
      EVAL Mach.TaskCreate(NIL,FALSE, t);
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
    
    RETURN TRUE;
  END MachBasic;

PROCEDURE Start(<* UNUSED *>i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END MachBasic.

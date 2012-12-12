(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *
 *)

MODULE SpaceBasic;
IMPORT IO, Fmt, Word;
IMPORT VirtAddr, Space, PhysAddr, CPU, VMError;
IMPORT SpaceBasicDep;

<*FATAL VMError.E*> (* quiet the compiler *)

(* BUGBUG: These tests don't catch slow leaks *)

PROCEDURE Test1(): BOOLEAN =
  VAR
    space: Space.T;
    startMem, endMem: Word.T;
  BEGIN
    IO.Put("Test1.  Sequentially creating and destroying new address spaces\n");
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    FOR i:= 1 TO 50 DO
      space := Space.Create();
      Space.Destroy(space);
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
    space: REF ARRAY OF Space.T;
    startMem, endMem: Word.T;
  BEGIN
    IO.Put("Test2.  Creating 50 new address spaces and then deleting them\n");
    space := NEW(REF ARRAY OF Space.T, 50);
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    FOR i:= 0 TO 49 DO
      space[i] := Space.Create();
    END;
    FOR i:= 0 TO 49 DO
      Space.Destroy(space[i]);
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
    space: Space.T;
    startMem, endMem: Word.T;
    regionBegin: VirtAddr.Address := SpaceBasicDep.StartAddr;
    regionSize : VirtAddr.Size := 10 * CPU.PAGESIZE;
  BEGIN
    IO.Put("Test3.  Simple allocating and deallocating of memory\n");
    startMem := PhysAddr.FreeMem();
    space := Space.Create();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    FOR i := 1 TO 50 DO
        Space.Allocate(space, regionBegin, regionSize, TRUE);
        Space.Deallocate(space, regionBegin, regionSize);
    END;
    Space.Destroy(space);
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
    space: Space.T;
    startMem, endMem: Word.T;
    regionBegin: REF ARRAY OF VirtAddr.Address;
    regionSize : VirtAddr.Size := 5 * CPU.PAGESIZE;
  BEGIN
    regionBegin := NEW(REF ARRAY OF VirtAddr.Address, 50);
    FOR i := 0 TO 49 DO
      regionBegin[i] := SpaceBasicDep.StartAddr + i * CPU.PAGESIZE;
    END;
    
    IO.Put("Test4.  Allocating and deallocating more memory\n");
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    space := Space.Create();
    FOR i := 0 TO 49 DO
        Space.Allocate(space, regionBegin[i], regionSize, TRUE);
    END;
    FOR i := 0 TO 49 DO
        Space.Deallocate(space, regionBegin[i], regionSize);
    END;
    Space.Destroy(space);
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
    space: Space.T;
    startMem, endMem: Word.T;
    regionBegin: REF ARRAY OF VirtAddr.Address;
    regionSize : VirtAddr.Size := 5 * CPU.PAGESIZE;
  BEGIN
    regionBegin := NEW(REF ARRAY OF VirtAddr.Address, 50);
    FOR i := 0 TO 49 DO
      regionBegin[i] := SpaceBasicDep.StartAddr + i * CPU.PAGESIZE;
    END;
    
    IO.Put("Test5.  Allocating lots of memory and deallocating it at once\n");
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    space := Space.Create();
    FOR i := 0 TO 49 DO
        Space.Allocate(space, regionBegin[i], regionSize, TRUE);
    END;
    Space.Deallocate(space, regionBegin[0], regionSize*50);
    Space.Destroy(space);
    endMem := PhysAddr.FreeMem();
    IO.Put("Ending physical memory = "&Fmt.Int(endMem)&" pages \n");
    IF endMem + 8 < startMem THEN (* 8 is the magic min_pte buffer number *)
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Test5;

<*UNUSED*>
PROCEDURE Test6(): BOOLEAN =
  VAR
    space: Space.T;
    startMem, endMem: Word.T;
    regionBegin: REF ARRAY OF VirtAddr.Address;
    regionSize : VirtAddr.Size := 5 * CPU.PAGESIZE;
    regionSize1 : VirtAddr.Size := 2 * CPU.PAGESIZE;
  BEGIN
    regionBegin := NEW(REF ARRAY OF VirtAddr.Address, 50);
    FOR i := 0 TO 49 DO
      regionBegin[i] := SpaceBasicDep.StartAddr + i * CPU.PAGESIZE;
    END;

    IO.Put("Test6.  Allocating and deallocating from center of region\n");
    startMem := PhysAddr.FreeMem();
    IO.Put("Starting physical memory = "&Fmt.Int(startMem)&" pages \n");
    space := Space.Create();
    FOR i := 0 TO 9 DO
        Space.Allocate(space, regionBegin[i], regionSize, TRUE);
    END;
    FOR i := 0 TO 9 DO
      Space.Deallocate(space,regionBegin[i]+regionSize1,
                             regionSize-regionSize1);
    END;
    endMem := PhysAddr.FreeMem();
    IO.Put("1) physical memory = "&Fmt.Int(endMem)&" pages \n");

    FOR i := 0 TO 9 DO
        Space.Allocate(space, regionBegin[i], regionSize, TRUE);
    END;
    FOR i := 0 TO 9 DO
      Space.Deallocate(space,regionBegin[i]-regionSize1,
                             regionSize-regionSize1);
    END;
    endMem := PhysAddr.FreeMem();
    IO.Put("2) physical memory = "&Fmt.Int(endMem)&" pages \n");
    FOR i := 0 TO 9 DO
        Space.Allocate(space, regionBegin[i], regionSize, TRUE);
    END;
    FOR i := 0 TO 9 DO
      Space.Deallocate(space,regionBegin[i]+regionSize1,
                             regionSize+regionSize1);
    END;
    endMem := PhysAddr.FreeMem();
    IO.Put("3) physical memory = "&Fmt.Int(endMem)&" pages \n");
    Space.Destroy(space);
    endMem := PhysAddr.FreeMem();
    IO.Put("Ending physical memory = "&Fmt.Int(endMem)&" pages \n");
    IF endMem + 8 < startMem THEN (* 8 is the magic min_pte buffer number *)
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Test6;

PROCEDURE SpaceBasic(): BOOLEAN =
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

    (* I don't know what this should do. 
    IF Test6() = FALSE THEN
      RETURN FALSE;
    END;
     *)
    RETURN TRUE;
  END SpaceBasic;

PROCEDURE Start(<* UNUSED *>i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END SpaceBasic.

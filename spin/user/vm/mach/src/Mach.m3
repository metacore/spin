(*
 * Copyright 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * PURPOSE
 *	To emulate the basic Mach 3.0 interfaces.  The basic parts of
 *      the task_* and vm_* interfaces are covered.
 *	NOTE: This is an _extension_!  Core services should not depend on it.
 *	      Core services should use Translation instead.
 *
 * HISTORY
 * 12-Dec-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

MODULE Mach;
IMPORT VMTypes, AddressSpace, Fmt, IO, MemoryObject,
       PagerObject, DefaultPager, 
       MachInterface, NameServer, Word, MachineMem;

REVEAL 
  TaskT = AddressSpace.T BRANDED OBJECT
    lock: MUTEX;
  END;

  MemoryObjectT = BRANDED OBJECT
  END;

VAR
  currentTask: TaskT;
  currentTaskLock: MUTEX;
  taskNumber: INTEGER := 0;

PROCEDURE TaskSelf(): TaskT = 
  BEGIN
    LOCK currentTaskLock DO
      RETURN currentTask; (* BUGBUG: should lookup to account for preemption *)
    END;
  END TaskSelf;

PROCEDURE TaskCreate(parentTask: TaskT; inheritMemory: BOOLEAN;
  VAR childTask: TaskT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
(*  begin: VMAddressT; *)
(*  size: VMSizeT; *)
  BEGIN
    TRY
      childTask := NEW(TaskT).init(name := "MachTask #"&Fmt.Int(taskNumber));
      childTask.lock := NEW(MUTEX);

      IF inheritMemory = TRUE THEN
        LOCK parentTask.lock DO (* Make sure parentTask exists *)
        END;
      END;
    EXCEPT
    ELSE
      return := KERN_FAILURE;
    END;
    IF return = KERN_SUCCESS THEN
      INC(taskNumber);
    END;
    RETURN return;
  END TaskCreate;

PROCEDURE TaskTerminate(targetTask: TaskT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
  BEGIN
    TRY
      LOCK targetTask.lock DO
        targetTask.destroy();
        (* RECLAIM(targetTask); *)
      END;
      EXCEPT
    ELSE
      return := KERN_FAILURE;
    END;
    RETURN return;
  END TaskTerminate;

(* kern_invalid_argument if the task is null *)
(* kern_sucess of the address is 0, and set *addr to 0 *)
(* start looking at addr nomatter what.  if less thanmin, start at min,
   if more than max return KERN_NO_SPACE *)
(* kern_no_space if you wrap *)
(* these are for not anywhere *)
(* kern_no_space if violate mask *)
(* set shared *)
(* kern_invalid_address if outsize map range or start >= end *)
(* KERN_NO_SPACE if not anywhere and doesn't file*)
PROCEDURE VMAllocate(targetTask: TaskT; VAR address: VMAddressT;
  size: VMSizeT; anywhere: BOOLEAN): KernReturnT =
  VAR
    mObj: MemoryObject.T;
    pager: PagerObject.T;
    return: KernReturnT := KERN_SUCCESS;
    pageNum: VMTypes.PageNumber;
    numPages: VMTypes.PageCount;
  BEGIN
    TRY
      LOCK targetTask.lock DO
        (* mark protection, inheritance, and protmax *)
        pageNum := MachineMem.BytesToPages(address);
        numPages := MachineMem.BytesToPages(MachineMem.RoundToPage(size));
	pager := DefaultPager.Create(size);
        mObj := NEW(MemoryObject.T).init(numPages, pager);
           
        targetTask.allocate(pageNum, numPages, anywhere);
        targetTask.map(pageNum, numPages,mObj,0);
        address := Word.Times(pageNum, MachineMem.PAGESIZE);
      END;
    EXCEPT
    ELSE 
      return := KERN_FAILURE; 
    END;
    RETURN return;
  END VMAllocate;

PROCEDURE VMDeallocate(targetTask: TaskT; address: VMAddressT; 
  size: VMSizeT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
    pageNum: CARDINAL;
    numPages: CARDINAL;
  BEGIN
    TRY
      LOCK targetTask.lock DO
        pageNum := MachineMem.BytesToPages(address);
        numPages := MachineMem.BytesToPages(MachineMem.RoundToPage(size));
        targetTask.unmap(pageNum, numPages);
      END;
    EXCEPT
    ELSE 
      return := KERN_FAILURE; 
    END;
    RETURN return;
  END VMDeallocate;

PROCEDURE VMProtect(targetTask: TaskT; <*UNUSED*>address: VMAddressT;
  <*UNUSED*>size: VMSizeT; <*UNUSED*>setMaximum: BOOLEAN; 
  <*UNUSED*>protection: VMProtT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
  BEGIN
    TRY
      LOCK targetTask.lock DO
        (* BUGBUG: do, check max etc... *)
        (*        task.protect(addr,size, newProt); *)
      END;
    EXCEPT
    ELSE 
      return := KERN_FAILURE; 
    END;
    RETURN return;
  END VMProtect; 

PROCEDURE VMInherit(targetTask: TaskT; <*UNUSED*>address: VMAddressT;
  <*UNUSED*>size: VMSizeT; inheritance: VMInheritT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
  BEGIN
    IF inheritance IN SET OF [0..2]
      {VM_INHERIT_NONE, VM_INHERIT_COPY,VM_INHERIT_NONE} = FALSE THEN
      RETURN KERN_INVALID_ARGUMENT;
    END;
    TRY
      LOCK targetTask.lock DO
      END;
    EXCEPT
    ELSE 
      return := KERN_FAILURE; 
    END;
    RETURN return;
  END VMInherit; 

PROCEDURE VMCopy(targetTask: TaskT; <*UNUSED*>sourceAddress: VMAddressT;
  <*UNUSED*>count: VMSizeT; <*UNUSED*>destAddress: VMAddressT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
  BEGIN
    TRY
      LOCK targetTask.lock DO
        (* BUGBUG: do, check max etc... *)
        (* copy *)
      END;
    EXCEPT
    ELSE 
      return := KERN_FAILURE; 
    END;
    RETURN return;
  END VMCopy; 

PROCEDURE VMRead(targetTask: TaskT; <*UNUSED*>address: VMAddressT;
  <*UNUSED*>size: VMSizeT; <*UNUSED*>VAR data: VMAddressT; <*UNUSED*>VAR dataCount:
  VMSizeT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
  BEGIN
    TRY
      LOCK targetTask.lock DO
        TRY
(*          AddressSpace.CopyData(targetTask, address, size, TaskSelf(), data); *)
        EXCEPT
        ELSE
          return := KERN_FAILURE;
        END;
        (* BUGBUG: do, check max etc... *)
        (* read *)
      END;
    EXCEPT
    ELSE 
      return := KERN_FAILURE; 
    END;
    RETURN return;
  END VMRead; 

PROCEDURE VMWrite(targetTask: TaskT; <*UNUSED*>address: VMAddressT;
  <*UNUSED*>size: VMSizeT; <*UNUSED*>data: VMAddressT; <*UNUSED*>dataCount:
  VMSizeT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
  BEGIN
    TRY
      LOCK targetTask.lock DO
        (* BUGBUG: do, check max etc... *)
(*        AddressSpace.CopyData(TaskSelf(), address, size, targetTask, data); *)
        (* write *)
      END;
    EXCEPT
    ELSE 
      return := KERN_FAILURE; 
    END;
    RETURN return;
  END VMWrite;

PROCEDURE VMMap(targetTask: TaskT; <*UNUSED*>VAR address: VMAddressT;
  <*UNUSED*>size: VMSizeT; <*UNUSED*>mask: Word.T; <*UNUSED*>anywhere: BOOLEAN;
  <*UNUSED*>memoryObject: MemoryObjectT; <*UNUSED*>offset: VMOffsetT;
  <*UNUSED*>copy: BOOLEAN; <*UNUSED*>protection: VMProtT; <*UNUSED*>maxProtection: VMProtT;
  <*UNUSED*>inheritance: VMInheritT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
  BEGIN
    TRY
      LOCK targetTask.lock DO
        (* BUGBUG: do, check max etc... *)
        (* map *)
      END;
    EXCEPT
    ELSE 
      return := KERN_FAILURE; 
    END;
    RETURN return;
  END VMMap;

PROCEDURE VMRegion(targetTask: TaskT; <*UNUSED*>VAR address: VMAddressT;
  <*UNUSED*>VAR size: VMSizeT; <*UNUSED*>VAR protection: VMProtT; <*UNUSED*>VAR
  maxProtection: VMProtT; <*UNUSED*>VAR inheritance: VMInheritT;
  <*UNUSED*>VAR shared: BOOLEAN; <*UNUSED*>VAR memoryObject: MemoryObjectT;
  <*UNUSED*>VAR offset: VMOffsetT): KernReturnT =
  VAR
    return: KernReturnT := KERN_SUCCESS;
  BEGIN
    TRY
      LOCK targetTask.lock DO
        (* BUGBUG: do, check max etc... *)
        (* map *)
      END;
    EXCEPT
    ELSE 
      return := KERN_FAILURE; 
    END;
    RETURN return;
  END VMRegion; 

PROCEDURE Equal(<*UNUSED*>task1, task2: TaskT): BOOLEAN RAISES ANY =
  BEGIN
    <* ASSERT FALSE *>
  END Equal;

BEGIN
  currentTask := NIL;
  currentTaskLock := NEW(MUTEX);

  TRY
    EVAL MachInterface.Export();
  EXCEPT
  | NameServer.Error => 
    IO.PutError("Mach: interface export failed\n");
  END;
END Mach.


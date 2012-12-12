(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 * 
 * HISTORY
 * 11-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *
 *)

INTERFACE LibMach;
IMPORT Mach;


<* PRAGMA INTERFACE_PROC_BASE *>

<* INTERFACE_PROC_BASE 10000 *>

CONST Brand = "LibMach";

TYPE
  TaskT = Mach.TaskT;
(*  MemoryObjectT = Mach.MemoryObjectT; *)
  VMAddressT = Mach.VMAddressT;
  VMSizeT = Mach.VMSizeT;
  VMOffsetT = Mach.VMOffsetT;
  VMProtT = Mach.VMProtT;
  VMInheritT = Mach.VMInheritT;
  KernReturnT = Mach.KernReturnT;

PROCEDURE TaskSelf(): TaskT;

PROCEDURE TaskCreate(parentTask : TaskT;
  inheritMemory: BOOLEAN; VAR childTask: TaskT): KernReturnT;

PROCEDURE TaskTerminate(targetTask: TaskT): KernReturnT;

PROCEDURE VMAllocate(targetTask: TaskT; VAR address: VMAddressT;
  size: VMSizeT; anywhere: BOOLEAN): KernReturnT;

PROCEDURE VMDeallocate(targetTask: TaskT; address: VMAddressT;
  size: VMSizeT): KernReturnT;

PROCEDURE VMProtect(targetTask: TaskT; address: VMAddressT;
  size: VMSizeT; setMaximum: BOOLEAN;
  newProtection: VMProtT): KernReturnT;

(* PROCEDURE VMInherit(targetTask: TaskT; address: VMAddressT;
  size: VMSizeT; inheritance: VMInheritT): KernReturnT;
*)
(*PROCEDURE VMCopy(targetTask: TaskT; sourceAddress: VMAddressT;
  count: VMSizeT; destAddress: VMAddressT): KernReturnT;

PROCEDURE VMRead(targetTask: TaskT; address: VMAddressT;
  size: VMSizeT; VAR data: VMAddressT; VAR dataCount:
  VMSizeT): KernReturnT;  

PROCEDURE VMWrite(task: TaskT; address: VMAddressT; data:
  VMAddressT; dataCount: VMSizeT): KernReturnT;  
*)
(*
PROCEDURE VMMap(targetTask: TaskT; VAR address: VMAddressT;
  size: VMSizeT; mask: Word.T; anywhere: BOOLEAN;
  memoryObject: MemoryObjectT; offset: VMOffsetT;
  copy: BOOLEAN; curProtection: VMProtT;
  maxProtection: VMProtT; inheritance: VMInheritT): KernReturnT;

PROCEDURE VMRegion(targetTask: TaskT; VAR address: VMAddressT;
  VAR size: VMSizeT; VAR protection: VMProtT; VAR
  maxProtection: VMProtT; VAR inheritance: VMInheritT;
  VAR shared: BOOLEAN; VAR memoryObject: MemoryObjectT;
  VAR offset: VMOffsetT): KernReturnT;
*)
(*
 * need to add threads code, and maybe device?
 *)

END LibMach.


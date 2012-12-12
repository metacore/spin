(*
 * Copyright 1995,1996 University of Washington
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

(* The "Mach" interface supports a subset of the Mach 3.0 microkernel 
   interfaces. *)

INTERFACE Mach;

IMPORT VirtAddr, Word;

TYPE
  TaskT <: REFANY;
  MemoryObjectT <: REFANY;
  VMAddressT = VirtAddr.Address;
  VMSizeT = VirtAddr.Size;
  VMOffsetT = VirtAddr.Address;
  VMProtT = Word.T;
  VMInheritT = Word.T;
  KernReturnT = INTEGER;
  

CONST
  VM_INHERIT_SHARE: VMInheritT = 0;
  VM_INHERIT_COPY: VMInheritT = 1;
  VM_INHERIT_NONE: VMInheritT = 2;
  VM_INHERIT_DEFAULT: VMInheritT = VM_INHERIT_COPY;
  VM_PROT_NONE: VMProtT = 0;
  VM_PROT_READ: VMProtT = 1;
  VM_PROT_WRITE: VMProtT = 2;
  VM_PROT_EXECUTE: VMProtT = 4;
  VM_PROT_DEFAULT: VMProtT = Word.Or(VM_PROT_READ, VM_PROT_WRITE);
  KERN_SUCCESS: KernReturnT = 0;
  KERN_INVALID_ARGUMENT: KernReturnT = 4; 
  KERN_FAILURE: KernReturnT = 5;

  
PROCEDURE TaskSelf(): TaskT;
(* Returns the identifier for the currently active task. *)

PROCEDURE TaskCreate(parentTask: TaskT;inheritMemory: BOOLEAN; VAR
  childTask: TaskT): KernReturnT;
(* Returns a new task identifier.  If the "inheritMemory" argument is "TRUE" 
   then the new task will contain a copy of the memory mapped in 
   "parentTask". *)

PROCEDURE TaskTerminate(targetTask: TaskT): KernReturnT;
(* Destroys a task and returns its resources to the system. *)

PROCEDURE VMAllocate(task: TaskT; VAR address: VMAddressT; size:
  VMSizeT; anywhere: BOOLEAN): KernReturnT;
(* Allocates "size" bytes of zero-filled memory in "task" and return
   the address of the new memory in "addr".  If the "anywhere"
   argument is "FALSE" then attempt to allocate the memory at "addr". *)

PROCEDURE VMDeallocate(targetTask: TaskT; address: VMAddressT;
  size: VMSizeT): KernReturnT;
(* Deallocate the range of memory from "addr" to "addr"+"size" in
   "task". *)

PROCEDURE VMProtect(targetTask: TaskT; address: VMAddressT; size:
  VMAddressT; setMaximum: BOOLEAN; protection: VMProtT): KernReturnT;
(* Set the protection of the range of memory from "addr" to "addr"+"size" 
   in "task". *) 

PROCEDURE VMInherit(targetTask: TaskT; address: VMAddressT; size:
  VMSizeT; inheritance: VMInheritT): KernReturnT;

PROCEDURE VMCopy(targetTask: TaskT; sourceAddress: VMAddressT; count:
  VMSizeT; destAddress: VMAddressT): KernReturnT;

PROCEDURE VMRead(targetTask: TaskT; address: VMAddressT; size:
  VMSizeT; VAR data: VMAddressT; VAR dataCount: 
  VMSizeT): KernReturnT;

PROCEDURE VMWrite(targetTask: TaskT; address: VMAddressT; size:
  VMSizeT; data: VMAddressT; dataCount: VMSizeT): KernReturnT;

PROCEDURE VMMap(targetTask: TaskT; VAR address: VMAddressT;
  size: VMSizeT; mask: Word.T; anywhere: BOOLEAN; memoryObject:
  MemoryObjectT; offset: VMOffsetT; copy: BOOLEAN;
  protection: VMProtT; maxProtection: VMProtT;
  inheritance: VMInheritT): KernReturnT;

PROCEDURE VMRegion(targetTask: TaskT; VAR address: VMAddressT;
  VAR size: VMSizeT; VAR protection: VMProtT; VAR
  maxProtection: VMProtT; VAR inheritance: VMInheritT;
  VAR shared: BOOLEAN; VAR memoryObject: MemoryObjectT;
  VAR offset: VMOffsetT): KernReturnT;

(*
 * For List generic
 *)
CONST Brand: TEXT = "Mach";
PROCEDURE Equal(task1, task2: TaskT): BOOLEAN RAISES ANY; 

END Mach.



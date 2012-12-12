(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 * 
 * HISTORY
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 23-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added rendezvous support.
 * 11-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *
 *)

MODULE LibMach;
IMPORT Mach;
IMPORT LibMachInterface, LibMachExtension, Auth, Dispatcher, USyscall;
IMPORT NameServer, Strand, MachineTrap;
IMPORT IO, Fmt, Text;

PROCEDURE TaskSelf(): TaskT =
  BEGIN
    RETURN Mach.TaskSelf();
  END TaskSelf;

PROCEDURE TaskCreate(parentTask : TaskT; inheritMemory: BOOLEAN; VAR
  childTask: TaskT): KernReturnT = 
  BEGIN
    RETURN Mach.TaskCreate(parentTask, inheritMemory, childTask); 
  END TaskCreate;

PROCEDURE TaskTerminate(targetTask: TaskT): KernReturnT =
  BEGIN
    RETURN Mach.TaskTerminate(targetTask);
  END TaskTerminate;

PROCEDURE VMAllocate(targetTask: TaskT; VAR address: VMAddressT;
  size: VMSizeT; anywhere: BOOLEAN): KernReturnT =
  BEGIN
    RETURN Mach.VMAllocate(targetTask, address, size, anywhere);
  END VMAllocate;

PROCEDURE VMDeallocate(targetTask: TaskT; address: VMAddressT;
  size: VMSizeT): KernReturnT =
  BEGIN
    RETURN Mach.VMDeallocate(targetTask, address, size);
  END VMDeallocate;

PROCEDURE VMProtect(targetTask: TaskT; address: VMAddressT;
  size: VMSizeT; setMaximum: BOOLEAN;
  <*UNUSED*>newProtection: VMProtT): KernReturnT =
  BEGIN
    RETURN Mach.VMProtect(targetTask, address, size, setMaximum, Mach.VM_PROT_DEFAULT);
  END VMProtect;

(* PROCEDURE VMInherit(targetTask: TaskT; address: VMAddressT;
  size: VMSizeT; inheritance: VMInheritT): KernReturnT;
*)
(*
PROCEDURE VMCopy(targetTask: TaskT; sourceAddress: VMAddressT;
  count: VMSizeT; destAddress: VMAddressT): KernReturnT =
  BEGIN
    RETURN Mach.VMAllocate(targetTask, sourceAddress, count, destAddress);
  END VMCopy;

PROCEDURE VMRead(targetTask: TaskT; address: VMAddressT;
  size: VMSizeT; VAR data: VMAddressT; VAR dataCount:
  VMSizeT): KernReturnT =  
  BEGIN
    RETURN Mach.VMRead(targetTask, address, size, data, dataCount);
  END VMRead;

PROCEDURE VMWrite(task: TaskT; address: VMAddressT; data:
  VMAddressT; dataCount: VMSizeT): KernReturnT;  

  BEGIN
    RETURN Mach.VMWrite(targetTask, address, data, dataCount);
  END VMWrite;
*)
(* PROCEDURE VMMap(targetTask: TaskT; VAR address: VMAddressT;
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

PROCEDURE RendezvousGuard (simpleName: TEXT;
			   <*UNUSED*>key: REFANY;
			   <*UNUSED*>VAR reply: REFANY) : BOOLEAN =
  BEGIN
    RETURN simpleName # NIL AND Text.Equal(simpleName, Brand);
  END RendezvousGuard;

PROCEDURE Rendezvous(simpleName: TEXT;
		     <*UNUSED*>key: REFANY;
		     <*UNUSED*>VAR reply: REFANY) : USyscall.ErrorCode =
  BEGIN
    IO.Put("libmach rendezvous, just ignoring\n"); 
    RETURN USyscall.Success;
  END Rendezvous;

BEGIN
  TRY
    EVAL LibMachInterface.Export(NEW(Auth.AuthAlways));
    EVAL Dispatcher.Install(MachineTrap.Syscall, NIL, 
			    LibMachExtension.Syscall, 
			    Dispatcher.DefaultOptions,
			    NEW(MachineTrap.AuthKey,
				minProcID := LibMachExtension.MinProcID,
				maxProcID := LibMachExtension.MaxProcID));
    EVAL Dispatcher.Install(USyscall.Rendezvous, RendezvousGuard, Rendezvous,
			    Dispatcher.DefaultOptions, NIL);
    IO.Put("libmach extension ready(but not installed).\n");
  EXCEPT
  | NameServer.Error => IO.Put("libmach failed to export interface.\n");
  | Dispatcher.Error (ec) =>
    IO.Put("libmach could not install rendezvous point: ("
	   & Fmt.Int(ORD(ec)) & "\n");
  END;
END LibMach.


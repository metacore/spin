(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE VMError;

TYPE Code = INTEGER;
  
EXCEPTION E(Code);
CONST
  Success = 0;
  NoAccess = 11;  (* Space.NoAccess *)
  NoSpace = 12;  (* Space.NoSpace *)
  SystemVirtualAddress = 13; (* Translation.Failure *)
  NoResponseFromPager = 14;
  OutOfMemory = 15; (* PhysFreeMem.OutOfMem *)
  CachePageAlreadyExist = 16;
  CachePageNotFound = 17;
  InvalidAddress = 18; (* page faulted on an address that doesn't
			  map anything *)
  InvalidTag = 19; (* tag is nil in PhysAddr.Allocate *)
  StaleFrame = 20; (* invalid PhysAddr.T is used *)
  StalePhysAddr = StaleFrame;
  
  PrivilegedOperation = 21;
PROCEDURE Message(code: Code): TEXT;
  
END VMError.

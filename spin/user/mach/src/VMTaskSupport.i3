(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 14-Dec-95  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)

(* "VMTaskSupport" interface -- dynamically linked extension *)

(* The "VMTaskSupport" interface essentially fakes the existence of
   Mach task support in SPIN.  A table is maintained of all user-level
   tasks.  Within this table is a heap pointer indicating the virtual
   address at which to allocate memory in this task.  Also in the table
   is another table of all of the virtual memory allocations for this
   task.  Using these tables, inheritance can be supported by aggressively
   copying. 

   This interface should become obsolete when the Mach task support
   extension is implemented. *)

INTERFACE VMTaskSupport;

IMPORT Space, Word;

EXCEPTION
  BadArg;
  BadBuf;
  BadSpace;
  BadAddr;
  Failure;

PROCEDURE MemUsage();
(* Print to the console some really basic statistics on memory usage. *)
PROCEDURE VMAllocAnywhere(space: Space.T; VAR addr: Word.T; VAR size: Word.T);
(* Allocate a region of virtual memory at an arbitrary location. *)

PROCEDURE RegisterNewSpace(space: Space.T);
(* Register a new space in the heap pointer table. *)

PROCEDURE RegisterMemAlloc(space: Space.T; addr: Word.T; size: Word.T);
(* Register the allocation of memory using Space.Allocate. *)

PROCEDURE RegisterMemDealloc(space: Space.T; addr: Word.T; size: Word.T)
  : BOOLEAN;
(* Register the deallocation of memory using Space.Deallocate. *)

PROCEDURE GetHeapPointer(space: Space.T; size: Word.T; VAR addr: Word.T)
  : BOOLEAN;
(* Get the current heap pointer for a space. *)

PROCEDURE GetRegion(space: Space.T; VAR addr: Word.T; VAR size: Word.T) 
  RAISES {BadSpace,BadAddr};
(* Get the begin address and size for a region in a space. *)

PROCEDURE DupTask(parentspace: Space.T; childspace: Space.T) RAISES {Failure};
(* Duplicate inherited memory in a parent space to a child space. *)

PROCEDURE SetInheritance(space: Space.T; addr: Word.T; size: Word.T;
  inheritance: Word.T);
(* Set inheritance attribute of region of memory.  This could be touchy. *)

PROCEDURE ClearSpace(space: Space.T) RAISES {Failure};
(* Deallocate all the recorded memory in an address space. *)

PROCEDURE RegisterSpaceDestroy(space: Space.T);
(* Record the destruction of a space through Space.Destroy. *)

END VMTaskSupport.


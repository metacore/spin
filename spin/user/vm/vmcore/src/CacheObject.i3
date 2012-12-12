(*
 * Copyright 1995,1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Destroy returns boolean so that you can keep the memobject.
 * 13-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 * html
 *)

(*
   Cache object is a subcomponent of the [MemoryObject].
   It records all the physical page frames currently held by
   the memory object it belongs to.

   It also serves as the policy module for page eviction(bot not now).

   In all the methods, the location is expressed in unit of MMU pages. 
 *)

INTERFACE CacheObject;

IMPORT Word;
IMPORT VMError;
IMPORT PhysAddr;
IMPORT VMTypes;
IMPORT Protection;

TYPE

  T <: TPublic;
  TPublic = OBJECT
    mObj: REFANY;
  METHODS
    destroy();
    (* This method is called when the memory object is no longer mapped
       anywhere. The method should free all the PhysAddrs in it. *)
    
    lookup(offset: VMTypes.PageNumber; type: INTEGER;
	   VAR frame: PhysAddr.T; VAR prot: Protection.T): BOOLEAN;
    (* Find the frame for the location "offset". Returns true if
       found, false otherwise. "type" is one of "Trap.Read",
       "Trap.Write", or "Trap.Execute".
       "frame" and "prot" are output parameters. *)
    
    update(offset: VMTypes.PageNumber; frame: PhysAddr.T) RAISES {VMError.E};
    (* Record the block "b" at the location "offset". *)
    
    invalidate(offset: VMTypes.PageNumber) RAISES {VMError.E};
    (* "invalidate" is called after the content of page "offset" is
       written out to a device. *)
    
    chooseVictim(VAR frame: PhysAddr.T);
    (* Choose a memory page to be paged out. "frame" is the page chosen by
     the vmcore. The cache object is free to override the frame
     (NIL is not allowed, though.)
     in the simplest case, this proc is a nop.
     
     XXX Vino "disaster" paper talks something about the safety
     of this procedure. I ignore all of their concerns.
     *)

    frameCount(): CARDINAL;
    (* Return the # of frames stored in the object. *)

    iterate(): Iterator;
    (* XXX this is not mutexed. 

     Consider these to address atomicitiy problem with iterator
     | lookupDirty(): ARRAY OF CacheBlock.T;
     | flushDirty(): ARRAY OF CacheBlock.T;
    *)
  END;
  
  Iterator = OBJECT
  METHODS
    next(VAR off: VMTypes.PageNumber; VAR frame: PhysAddr.T): BOOLEAN;
  END;
  
CONST Brand = "CacheObject-1.0";

PROCEDURE Equal(co1, co2: T): BOOLEAN;
PROCEDURE Hash(co: T): Word.T;

END CacheObject.

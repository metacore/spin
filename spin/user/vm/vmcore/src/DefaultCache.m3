(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE DefaultCache;
IMPORT DebugOption;
IMPORT Fmt;<*NOWARN*>
IMPORT CacheObject;
IMPORT OffsetPhysAddrTbl;
IMPORT VMTypes;
IMPORT VMError;
IMPORT PhysAddr;
IMPORT Debugger;
IMPORT Protection;
IMPORT IO;

TYPE
  T = CacheObject.T BRANDED OBJECT
    size : VMTypes.PageCount;
    cache: OffsetPhysAddrTbl.T;
    lock: MUTEX;
  OVERRIDES
    destroy := Destroy;
    lookup := Lookup;
    update := Update;
    invalidate := Invalidate;
    frameCount := FrameCount;
    iterate := Iterate;
  END;

PROCEDURE Create (size: VMTypes.PageCount): CacheObject.T =
  BEGIN
    RETURN NEW(T, 
	       size := size,
	       cache := NEW(OffsetPhysAddrTbl.Default).init(),
	       lock := NEW(MUTEX));
  END Create;

PROCEDURE Destroy (self: T) =
  VAR
    offset: VMTypes.PageNumber; 
    frame: PhysAddr.T;
  BEGIN
    LOCK self.lock DO
      (* Free any cache specific state *)
      WITH iterate = self.cache.iterate() DO
	WHILE iterate.next(offset, frame) = TRUE DO
	  PhysAddr.Deallocate(frame);
	END;
      END;
      WITH iterate = self.cache.iterate() DO
        WHILE iterate.next(offset, frame) DO
	  EVAL self.cache.delete(offset, frame);
	  (* XXX is it ok to modify table while iterating? *)
        END;
      END;
      self.cache := NIL;
    END;
    self.lock := NIL;
  END Destroy;

PROCEDURE Lookup (self: T; offset: VMTypes.PageNumber;
		  <*UNUSED*>type: INTEGER;
		  VAR frame: PhysAddr.T;
		  VAR prot: Protection.T): BOOLEAN =
  BEGIN
    LOCK self.lock DO
      prot := Protection.All;
      RETURN self.cache.get(offset, frame);
    END;
  END Lookup;
  
PROCEDURE Update (self: T; offset: VMTypes.PageNumber; frame: PhysAddr.T) =
  VAR
    result: BOOLEAN;
    dummy: PhysAddr.T;
  BEGIN
    LOCK self.lock DO
      (* We often make mistake here-- *)
      IF DebugOption.PhysAddr THEN
	IF self.cache.get(offset, dummy) THEN
	  IO.Put("something wrong in cache upd.\n");
	  Debugger.Enter();
	END;
      END;
      result := self.cache.put(offset, frame);
      <*ASSERT result = FALSE*>
    END;
  END Update;

PROCEDURE Invalidate (self: T; offset: VMTypes.PageNumber) RAISES {VMError.E} =
  VAR
    frame: PhysAddr.T;
  BEGIN
    LOCK self.lock DO
      IF NOT self.cache.delete(offset, frame) THEN
	RAISE VMError.E(VMError.CachePageNotFound);
      END;
    END;
  END Invalidate;

PROCEDURE FrameCount (self: T): CARDINAL =
  BEGIN
    LOCK self.lock DO 
      RETURN self.cache.size();
    END;
  END FrameCount;
  
TYPE Iterator = CacheObject.Iterator OBJECT
    itr: OffsetPhysAddrTbl.Iterator;
  OVERRIDES
    next := Next;
  END;
  
(* What happens if it gets changed underneath? *)
PROCEDURE Iterate (self: T): CacheObject.Iterator =
  BEGIN
    RETURN NEW(Iterator, itr := self.cache.iterate());
  END Iterate;

PROCEDURE Next (i: Iterator; VAR off: VMTypes.PageNumber;
		VAR frame: PhysAddr.T): BOOLEAN =
  BEGIN
    RETURN i.itr.next(off, frame);
  END Next;
  
BEGIN
END DefaultCache.

(*
 * Copyright 1995,1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 *)

MODULE CacheObject;
IMPORT Word;
IMPORT PhysAddr;
IMPORT IO;
IMPORT VMTypes;
IMPORT Protection;

TYPE
  REVEAL T = TPublic BRANDED OBJECT
  OVERRIDES
    destroy := Destroy;
    lookup := Lookup;
    chooseVictim := ChooseVictim;
    update := Update;
    invalidate := Invalidate;
    frameCount := FrameCount;
    iterate := Iterate;
  END;

PROCEDURE Destroy(<*UNUSED*>self: T) =
  BEGIN
    IO.Put("CacheObject.Destroy not implemented");
  END Destroy;

PROCEDURE Lookup (<*UNUSED*>self: T; <*UNUSED*>off: VMTypes.PageNumber;
		  <*UNUSED*>type: INTEGER;
		  <*UNUSED*>VAR frame: PhysAddr.T;
		  <*UNUSED*>VAR prot: Protection.T): BOOLEAN =
  BEGIN
    IO.Put("CacheObject.Lookup not implemented");
    RETURN FALSE;
  END Lookup;
  
PROCEDURE ChooseVictim (<*UNUSED*>self: T;
			<*UNUSED*>VAR frame: PhysAddr.T) =
  BEGIN
    (* nop *)
  END ChooseVictim;

PROCEDURE Update (<*UNUSED*>self: T; <*UNUSED*>off: VMTypes.PageNumber;
		  <*UNUSED*>frame: PhysAddr.T) =
  BEGIN
    IO.Put("CacheObject.Update not implemented");
  END Update;

PROCEDURE Invalidate (<*UNUSED*>self: T; <*UNUSED*>off: VMTypes.PageNumber) =
  BEGIN
    IO.Put("CacheObject.Invalidate not implemented");
  END Invalidate;

PROCEDURE FrameCount (<*UNUSED*>self: T): CARDINAL =
  BEGIN
    IO.Put("CacheObject.FrameCount not implemented.\n");
    RETURN 0;
  END FrameCount;
  
(* What happens if it gets changed underneath? *)
PROCEDURE Iterate (<*UNUSED*>self: T) : Iterator =
  BEGIN
    IO.Put("CacheObject.Iterate not implemented");
    RETURN NIL;
  END Iterate;

PROCEDURE Equal(<*UNUSED*>co1, co2: T): BOOLEAN =
  BEGIN
    <* ASSERT FALSE *>
    RETURN FALSE;
  END Equal;

PROCEDURE Hash(<*UNUSED*>co: T): Word.T =
  BEGIN
    <* ASSERT FALSE *>
    RETURN 0;
  END Hash;

BEGIN
END CacheObject.






(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 14-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE PagerObject;
IMPORT IO, Debugger;
IMPORT MemoryObject, MemoryObjectRep;
IMPORT PhysAddr;
IMPORT Protection;

REVEAL T = Public BRANDED OBJECT
OVERRIDES
  init := Init;
  destroy := Destroy;
  pageIn := PageIn;
  pageOut := PageOut;
  pageMapNotify := PageMapNotify;
  mapNotify := MapNotify;
  unmapNotify := UnmapNotify;
END;

PROCEDURE Init (s: T): T =
  BEGIN
    RETURN s;
  END Init;
  
PROCEDURE Destroy (<*UNUSED*>s : T) =
  BEGIN
  END Destroy;
  
PROCEDURE PageIn(<*UNUSED*>s: T;
		 <*UNUSED*>off: PageCount;
		 <*UNUSED*>type: INTEGER;
		 <*UNUSED*>frame: PhysAddr.T;
		 <*UNUSED*>VAR prot: Protection.T) : ResultCode =
  BEGIN
    IO.Put("pager pagein : not implemented.\n");
    RETURN ResultCode.NotImplemented;
  END PageIn;

PROCEDURE PageOut(<*UNUSED*>s : T;
		  <*UNUSED*>off: PageCount;
		  <*UNUSED*>frame: PhysAddr.T;
		  <*UNUSED*>dirty: BOOLEAN): ResultCode =
  BEGIN
    IO.Put("pager pageout : not implemented.\n");
    Debugger.Enter();
    RETURN ResultCode.NotImplemented;
  END PageOut;

PROCEDURE PageMapNotify(<*UNUSED*>s: T;
			<*UNUSED*>offset: PageNumber;
			<*UNUSED*>space: REFANY;
			<*UNUSED*>virtAddr: PageNumber) =
  BEGIN
  END PageMapNotify;
  
PROCEDURE MapNotify(<*UNUSED*>s: T;
		    <*UNUSED*>from: PageNumber;
		    <*UNUSED*>len: PageCount;
		    <*UNUSED*>space: REFANY;
		    <*UNUSED*>virtAddr: PageNumber) =
  BEGIN
  END MapNotify;
  
PROCEDURE UnmapNotify(t: T;
		      <*UNUSED*>from: PageNumber;
		      <*UNUSED*>len: PageCount;
		      <*UNUSED*>space: REFANY;
		      <*UNUSED*>virtAddr: PageNumber) =
  VAR mObj: MemoryObject.T := t.mObj;
  BEGIN
    IF NOT mObj.isMapped() THEN
      mObj.destroy();
    END;
  END UnmapNotify;
  

BEGIN
END PagerObject.

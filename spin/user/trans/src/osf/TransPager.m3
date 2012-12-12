(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE TransPager;
IMPORT Storage, StorageRep;
IMPORT TransGroup;
IMPORT PagerObject;
IMPORT PhysAddr;
IMPORT BufferPurge;
IMPORT IO;
IMPORT CPU;
IMPORT Umman, Utypes;

REVEAL T = PagerObject.T BRANDED OBJECT
  st: Storage.T;
OVERRIDES
  pageOut := PageOut;
END;
PROCEDURE Create (st: Storage.T): T =
  BEGIN
    RETURN NEW(T, st := st);
  END Create;
  
PROCEDURE GetStorage(t: T): Storage.T =
  BEGIN
    RETURN t.st;
  END GetStorage;

PROCEDURE PageOut (t: T; off: PagerObject.PageNumber;
		   <*UNUSED*>frame: PhysAddr.T;
		   <*UNUSED*>dirty: BOOLEAN): PagerObject.ResultCode =
  VAR
    offs: ARRAY [0..0] OF INTEGER;
  BEGIN
    offs[0] := off;
    IF BufferPurge.PurgePages(t.st, FALSE, offs) THEN
      RETURN PagerObject.ResultCode.Success;
    ELSE
      IO.Put("pageout: already purged.\n");
      RETURN PagerObject.ResultCode.AlreadyPagedOut;
    END;
  END PageOut;

PROCEDURE InvalidateMappingsForSpace(t: T; group: TransGroup.T) =
  VAR mObj := t.st.memObj;
  BEGIN
    IF Umman.mprotect(LOOPHOLE(mObj.base, Utypes.caddr_t),
		      mObj.virtualSize()*CPU.PAGESIZE, 0) < 0 THEN
      IO.Put("invalidate: failed.\n");
    END;
  END InvalidateMappingsForSpace;
BEGIN
END TransPager.

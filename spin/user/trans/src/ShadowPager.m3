(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)
MODULE ShadowPager;
IMPORT Storage, StorageRep;
IMPORT PagerObject;
IMPORT PhysAddr;
IMPORT BufferPurge;
IMPORT IO;

REVEAL
  T = PagerObject.T BRANDED OBJECT
    st: Storage.T;
  OVERRIDES
    pageOut := PageOut;
  END;

PROCEDURE Create (st: Storage.T): PagerObject.T =
  BEGIN
    RETURN NEW(T, st := st);
  END Create;

PROCEDURE PageOut (t: T; off: PagerObject.PageNumber;
		   <*UNUSED*>frame: PhysAddr.T;
		   <*UNUSED*>dirty: BOOLEAN): PagerObject.ResultCode =
  VAR
    offs: ARRAY [0 .. 31] OF INTEGER;
    frames: ARRAY [0 .. 30] OF PhysAddr.T;
    nFrames: CARDINAL;
  BEGIN
    offs[0] := off;
    nFrames := 0;
    nFrames := PhysAddr.GetVictims(t.st.shadowObj, frames);
    FOR i := 0 TO nFrames-1 DO
      offs[i+1] := PhysAddr.GetTag(frames[i]).off;
    END;
    
    IF BufferPurge.PurgePages(t.st, TRUE, SUBARRAY(offs, 0, nFrames+1)) THEN
      RETURN PagerObject.ResultCode.Success;
    ELSE
      IO.Put("pageout: already purged.\n");
      RETURN PagerObject.ResultCode.AlreadyPagedOut;
    END;
  END PageOut;

BEGIN
END ShadowPager.

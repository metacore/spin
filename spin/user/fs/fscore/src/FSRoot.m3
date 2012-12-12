(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE FSRoot;
IMPORT StatFs;

REVEAL T = Public BRANDED OBJECT
  sync := Sync;
  statfs := Statfs;
END;

PROCEDURE Sync (<*UNUSED*>self: T) =
  BEGIN
  END Sync;

PROCEDURE Statfs (<*UNUSED*>self: T; VAR s: StatFs.T) =
  BEGIN
    StatFs.Init(s);
  END Statfs;
  
BEGIN
END FSRoot.

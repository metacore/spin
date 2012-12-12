(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE StatFs;
PROCEDURE Init (VAR s: T) =
  BEGIN
    s.type := 1; (* MOUNT_UFS. see sys/mount.h *)
    s.flags := 0;
    s.fsize := 0;
    s.bsize := 0;
    s.blocks := 0;
    s.bfree := 0;
    s.bavail := 0;
    s.files := 0;
    s.ffree := 0;
  END Init;
  
BEGIN
END StatFs.

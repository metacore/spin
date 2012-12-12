(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 22-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE FileStat;
PROCEDURE Init (VAR t: T) =
  BEGIN
    t.dev := 0;
    t.ino := 2;
    t.mode := 16_777;
    t.nlink := 1;
    t.uid := 0;
    t.gid := 0;
    t.rdev := 0;
    t.size := 0;
    t.atime := 0;
    t.mtime := 0;
    t.ctime := 0;
    t.blksize := 512;
    t.blocks := 0;
    t.flags := 0;
    t.gen := 432;
  END Init;
  
BEGIN
END FileStat.

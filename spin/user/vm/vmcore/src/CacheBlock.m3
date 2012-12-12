(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 13-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

MODULE CacheBlock;

IMPORT CacheBlockRep, PhysAddr;

PROCEDURE Print (self: T): TEXT =
  VAR
    string: TEXT;
  BEGIN
    string := "CacheBlock(";
    string := string&" "&PhysAddr.Print(self.page)&" ";
    string := string&")";
    RETURN string;
  END Print;


BEGIN
END CacheBlock.









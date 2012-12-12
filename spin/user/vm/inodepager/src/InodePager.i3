(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 14-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(*
   Inode pager manages <a="MemoryObject">memory objects</a> that back files.
   Such memory objects are useful when you want to access files using
   usual CPU memory instructions.

   Inode manager does not handle pageout properly; it just throws away
   all the pages.
   
   Inode pager has a table that remembers what object it
   has created, and if the "Create"
   request is issued for the "file" for which the memory object already exists,
   the inode pager just returns the old one. 
   The table entry is deleted as soon as the memory object gets isolated,
   i.e., the memory object is unmapped from all the address spaces.

   Thus, if a process opens a file <em>A</em> , mmaps, does work,
   and terminates, the memory object for <em>A</em> is destroyed when
   the process terminates. When another process mmaps <em>A</em>, another
   memory object is created.

   This table hack will be replaced by generic file caching mechanism in the
   future.
*)

INTERFACE InodePager;
IMPORT MemoryObject;
IMPORT CacheObject;
IMPORT File;
IMPORT NameServer;

PROCEDURE Create(file: File.T; cache: CacheObject.T := NIL;
		 READONLY name: NameServer.Name): MemoryObject.T;
   (* Create a memory object. If "cache" is NIL, then the default cache
      object is created. "name" is used just for debugging. *)
   
END InodePager.

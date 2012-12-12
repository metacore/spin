(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description.
 *
 * HISTORY
 *)

(*
 * This module provides a least-recently-used write-through cache
 * that stacks on top of any other file system.
 * 
 * That's the theory, anyway...
 *
 * Current status:  UNDER DEVELOPMENT
 *)

INTERFACE LruWt;

IMPORT File, Directory, FileSystem, NameServer;

TYPE FileT     <: File.T;
TYPE DirectoryT <: Directory.T;
TYPE FileSystemT <: FileSystem.T;
TYPE IteratorT <: NameServer.Iterator;

END LruWt.

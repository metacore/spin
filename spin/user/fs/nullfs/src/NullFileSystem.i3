(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 16-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE NullFileSystem;
IMPORT File, Directory;
TYPE FileT = File.T;
TYPE DirectoryT <: Directory.T;
END NullFileSystem.

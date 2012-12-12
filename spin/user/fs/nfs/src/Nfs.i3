(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 09-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updatd to new filesystem interface.
 *
 * 01-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

INTERFACE Nfs;
IMPORT File, Directory;
TYPE FileT      <: File.T;
TYPE DirectoryT <: Directory.T;
END Nfs.

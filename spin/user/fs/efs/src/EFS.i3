(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 05-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* EFS is a simple veneer layer for the extent extension. *)
 
INTERFACE EFS;
IMPORT File, Directory;
TYPE FileT <: File.T;
TYPE DirectoryT <: Directory.T;
END EFS.

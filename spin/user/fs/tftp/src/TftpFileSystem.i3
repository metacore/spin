(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 07-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to inherit from FileSystem.{T,MountPoint}. 
 * 22-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use Device.Error.
 *
 * 06-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

(* The canonical way of using this is to either get the Interface
   itself through the nameserver and then use GetRootDevice to get an
   instance of the root device and to then use that to bootstrap
   yourself up to a real file, OR to go through the the device manager
   and get an instance of the root device and to then use that to
   bootstrap yourself up to a real file. *)

INTERFACE TftpFileSystem;
IMPORT File, Directory;
TYPE FileT <: File.T;
TYPE DirectoryT <: Directory.T;
END TftpFileSystem.

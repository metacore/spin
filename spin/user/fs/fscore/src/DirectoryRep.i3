(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE DirectoryRep;
IMPORT Directory;
IMPORT FileSystem;

REVEAL
  Directory.T <: Public;
TYPE
  Public = Directory.Public OBJECT
    attr: Directory.Attr;
    mountInfo: MountInfo; (* "mountInfo" is NIL unless attr is
			     MountPoint or MountShadow. *)
  END;

TYPE
  MountInfo = REF RECORD
    root: Directory.T;
    (* Points to the root of the file system. *)
    shadow: Directory.T;
    (* Points to the directory shadowed by the mounted file system. *)
    fs: FileSystem.T;
  END;

END DirectoryRep.

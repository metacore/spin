(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 16-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

(* "NullFileSystem" implements a memory based file system interface *)

MODULE NullFileSystem;
IMPORT Error, FileId, FileSystem, Directory, IO, NameServer;

(* ------------------- FileId Support ------------------- *)
CONST NULLCHAR = VAL(16_00,CHAR);
VAR FileIdStart := FileId.T{   
       'n',      'u',      'l',      'l',      'f',      's', 
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, 
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, 
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR};

(* ------------------- DIRECTORY SERVICES ------------------- *)

(* The "DirectoryT" defines what a directory looks like for this
   filesystem.  A DirectoryT is a subtype of fscore's Directory.T *)

REVEAL DirectoryT = Directory.T BRANDED OBJECT
  OVERRIDES
    init := DirectoryInit;
    create := Create;
  END;

PROCEDURE DirectoryInit (self: DirectoryT): NameServer.T =
  BEGIN
    self.id := FileId.Inc(FileIdStart);
    RETURN Directory.T.init(self);
  END DirectoryInit;

PROCEDURE Create (self: DirectoryT; 
		  name: NameServer.Name): NameServer.T
  RAISES {NameServer.Error}= 
  VAR new : DirectoryT;
  BEGIN
    new := NEW(DirectoryT).init();
    self.attach(name, new);
    RETURN new;
  END Create;

(* ------------------- FILE SERVICES ------------------- *)
(* nullfs doesn't support files *)

(* ------------------- MOUNT SERVICES ------------------- *)

(* The type "MountT" represents a mount point exported by this file
   system.  The null file system does not maintain additional
   information for mount points, consequently is acts just like a
   regular DiretoryT.  A MountT is a subtype of a DirectoryT. *)

TYPE MountT = DirectoryT;

(* The type "FileSystemT" is used to register the filesystem with
   fscore.  A FileSystemT is a subtype of fscore's FileSystem.T. *)

TYPE FileSystemT = FileSystem.T OBJECT
  OVERRIDES
    (* "create" is called when we create a new mount point *)
    newfs := Newfs;
  END;

PROCEDURE Newfs (<*UNUSED*>self: FileSystemT; 
		 <*UNUSED*>READONLY name: NameServer.Name): Directory.T = 
  BEGIN
    RETURN NEW(MountT).init();
  END Newfs;

BEGIN
  TRY
    FileSystem.Register("nullfs", NEW(FileSystemT));
  EXCEPT
  | Error.E(e) =>
    IO.Put(e.message()&" during NullFileSystem initialization.\n");
  END;
END NullFileSystem.

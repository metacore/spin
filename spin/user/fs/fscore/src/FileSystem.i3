(*
 * Copyright 1994 - 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 26-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed Name representation, and also cleaned up FileSystem.T hierarchy.
 * 21-May-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added mountPath variable so NFS can get to the mount path
 *      without changing 2000 interfaces.
 *
 * 04-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Added sync, mkdir, rmdir, unlink to mountpoint.
 *
 * 05-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Major reconstruction. It now supports any logical file system.
 *
 * 22-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use Device exceptions.
 *)

(* "FileSystem" 
   Abstract interface for a logical file system(tftp, ufs, nfs, ...).
*)

INTERFACE FileSystem;
IMPORT NameServer, Error;
IMPORT Directory;

TYPE T <: Public;
TYPE Public = OBJECT
  METHODS
    newfs(READONLY devName: NameServer.Name): Directory.T RAISES {Error.E};
    (* Creates a new file system on the specified device. *)
    deletefs(root: Directory.T) RAISES {Error.E};
    (* Deletes the file system "root" that was created by "newfs".
       Defaults to nop. *)
  END;

(* Publish the mount path to the file system being mounted *)
(* XXX this hack has to be got rid of.... - yaz *)
TYPE MPT = MUTEX OBJECT t : TEXT; END;
VAR  mountPath : MPT;  

PROCEDURE Mount(fs, dev, directory: TEXT) RAISES {Error.E};
  (* "Mount" attaches "dev" using the "fs" file-system implementation
     to the file tree at "directory", which must already exist.  Each
     filesystem implementation has its own naming convention. *)

PROCEDURE Unmount(directory : TEXT) RAISES {Error.E};
  (* "Unmount" removes a file-system mounted on "directory" *)

PROCEDURE Register(name: TEXT; self: T) RAISES {Error.E};
  (* "Register" a new file-system *)

PROCEDURE Deregister(name: TEXT; VAR fs: T) RAISES {Error.E};
  (* "Deregister" a file-system *)

<*OBSOLETE*>  
PROCEDURE Open(name:TEXT): REFANY RAISES {Error.E};
  (* "Open" returns a file, or directory. "name" must be an absolute
     path. This is obsolete. Use "Lookup" or "LookupName" instead of this.*)

PROCEDURE GetRoot() : Directory.T;
  (* "GetRoot" is the global file-system root directory. *)

PROCEDURE Lookup(dir: NameServer.T := NIL;
		 name: TEXT;
		 dontFollowAlias := FALSE): REFANY RAISES {NameServer.Error};
  (* Looks up the path "name" from the directory "dir". "dir" is used only when
     the first letter of "name" isn't "/", i.e., "name" is relative.
     If "name" is absolute, then "dir" is ignored and the name is traversed
     from the file system root, i.e., "GetRoot()". *)
  
PROCEDURE LookupSoft(dir: NameServer.T := NIL;
		 name: TEXT;
		 dontFollowAlias := FALSE): REFANY RAISES {NameServer.Error};
  (* Similar to "Lookup", but this version returns NIL when "name" does
     not exist. ("Lookup" raises an exception in that case. *)
  
PROCEDURE LookupName(dir: NameServer.T := NIL;
		     READONLY name: NameServer.Name;
		     dontFollowAlias := FALSE): REFANY
  RAISES {NameServer.Error};
  (* Faster version of "Lookup". Use this one instead of "Lookup" if
     you are concerned about performance. *)

<*OBSOLETE*>  
PROCEDURE NewFs(fsname, devName: TEXT) RAISES {Error.E};
<*OBSOLETE*>  
PROCEDURE Sync(fsname,devName: TEXT) RAISES {Error.E};
  
END FileSystem.


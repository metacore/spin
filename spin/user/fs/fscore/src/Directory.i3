(*
 * HISTORY
 * 02-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added Attr and mountPeer.
 * 22-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NameServer.Name representation.
 * 09-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE Directory;
IMPORT FileId;
IMPORT NameServer;
IMPORT File;
IMPORT FileStat;
IMPORT Error;
IMPORT FSRoot;

TYPE
  Attr = File.Attr;
  
  (* "MountPoint" is a root of a mounted file system.
     "MountShadow" is a directory shadowed by a mount point.
     "Ordinary" is either mountpoint nor mountshadow.
  *)

  T <: Public;
  Public = NameServer.Default OBJECT
    id: FileId.T;
  METHODS
    mkfile(name: NameServer.Name): File.T RAISES {Error.E};
    (* Whatever else we want to do with directories.
       This corresponds to "creat" in UNIX.
        XXX Why don't we call it create? really confusing:( -yaz *)
  
    stat(VAR s: FileStat.T) RAISES {Error.E};
    (* Implement "fstat" system call. *)

    root(): FSRoot.T RAISES {Error.E};
  END;

END Directory.

(*
 * HISTORY
 * 09-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE MountDirectoryRep;
IMPORT FileSystem, Directory, MountDirectory;

TYPE T = MountDirectory.TPublic OBJECT
  mountPoint: Directory.T;
  filesystemRoot: Directory.T;
  filesystem: FileSystem.T;
  deviceName: TEXT;             (* textual device name *)
END;

REVEAL MountDirectory.T <: T;
  
END MountDirectoryRep.




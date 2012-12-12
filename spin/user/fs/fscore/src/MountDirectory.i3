(*
 * HISTORY
 * 09-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

(* This is the virtual dirctory that is layered upon the mount point.
 The real mount point(= root of the fs under this) is RootDirectory.T *)
 
INTERFACE MountDirectory;
IMPORT Directory;
TYPE T <: TPublic;
  
TYPE TPublic = Directory.T OBJECT END;
END MountDirectory. 


(*
 * HISTORY
 * 30-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added some comments, marked Inc *OBSOLETE*.
 * 09-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

(* 

   This module defines the FileId type used by all file-system
   providers to distinguish an individual file, and defines the
   operations allowed on the global FileId -> file object table.

   A "FileId.T" is a filesystem dependent identifier used to resolve
   to file objects.
   It must uniquely identify the file, even across system reboot.
   Note: "Inc" procedure has been used to generate a unique file id, but
   this is WRONG; it doesn't guarantee uniqueness across reboot. Thus,
   each file system must come up with unique representation of the file.
   For example, in UNIX FFS, you might use inode number + ino generation
   number. The open problem is how we should distinguish file systems.
   A FileId.T should typically refer to a File.T and
   Directory.T, but may also be used to refer to an arbitrary REF.
   The identifier is a MAXSIZE-byte array of CHAR and is treated as an
   opaque key to the FileId hash table.  *)

INTERFACE FileId;
IMPORT Word;

CONST MAXSIZE = 32;
  (* XXX we should change this to "Size". It's not about maximum, it's
     a fixed value. *)

TYPE T = ALIGNED BITSIZE(INTEGER) FOR ARRAY [0..MAXSIZE-1] OF CHAR;
CONST Brand = "FileId";

PROCEDURE Get(READONLY id: T): REFANY;
  (* "Get" returns the object associated with "id".  Otherwise, it
     returns NIL if such an object doesn't exist. *)

PROCEDURE Delete(READONLY id: T): REFANY;
  (* "Delete" removes the "id" entry from the FileId table and returns
     the object associated with "id". Otherwise, it returns NIL if
     the object doesn't exist. *)

PROCEDURE Put(READONLY id: T; obj: REFANY);
  (* "Put" creates a new entry for "id" and associates it with "obj".
     It an entry already existed for "id", then "Put" will change the 
     mapping to the new "obj". *)

(* generic support *)
PROCEDURE Equal  (READONLY id1,id2: T): BOOLEAN;
PROCEDURE Hash   (READONLY id: T): Word.T;

<*OBSOLETE*>PROCEDURE Inc (VAR id: T) :T;

(* debugging support *)
PROCEDURE Print(READONLY id:T);

END FileId.

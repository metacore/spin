(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 23-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Rewritten using new NS interface.
 * 31-May-97  David Becker at the University of Washington
 *      New salnet interface.  Currently set to use salnfs.
 *
 *  5-oct-96  David Becker (becker) at the University of Washington
 *	Mount tftp at /spin so domain file names match those on
 #	unix keeping gdb happy for domain sweeps.
 *
 *  4-Jun-96 Jan Sanislo (oystr) at the University of Washington
 *	Return an normal error code for ENOTFOUND.  Some upper level
 *	programs count on this behavior.
 *
 *  9-May-96  David Becker (becker) at the University of Washington
 *	retry with bigger buf on ENOSPACE instead of EACCESS(2)
 *
 * 05-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed the "from" parameter in Read() to CARDINAL.
 *
 * 29-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Eliminate needless allocation. Simplify.  Enable stat semantics.
 *
 * 23-Jan-96  David Dion (ddion) at the University of Washington
 *	Increased MaxLen parameter to 2200000 for UNIX server.
 *
 * 07-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to inherit from FileSystem.{T,MountPoint}. 
 *
 * 21-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use Device.Error.
 *	Cleanup. Made the implementation safe again.
 *
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Made the implementation safe.
 *
 * 04-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Rewritten to export DEVICE interface and to rely on FileIO
 *
 * 04-Jun-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created.
 *)

(* "TftpFileSystem" implements a crude file system interface that only
   support open and read.  It currently can only read files that are
   smaller than MaxLen.  It is implemented using the FileSystem object
   interfaces. *)

MODULE TftpFileSystem;
IMPORT Directory, Error, File, FileId, FileStat, FileSystem, IO,
       NameServer;
IMPORT Salnet, Errno, ErrnoDep;
IMPORT NSName;
IMPORT BuildInfo;

VAR mount: Salnet.NfsFileHandle;
VAR mount_point := BuildInfo.GetMountPoint();
VAR mount_pad := BuildInfo.GetMountPad();

(* ------------------- FILEID SUPPORT ------------------- *)

VAR FileIdStart := FileId.T{'t', 'f', 't', 'p', '\000',..};

(* ------------------- DIRECTORY SERVICES ------------------- *)

(* The "DirectoryT" defines what a directory looks like for this
   filesystem.  A DirectoryT is a subtype of fscore's Directory.T *)

REVEAL DirectoryT = Directory.T BRANDED OBJECT
  OVERRIDES
    init := DirectoryInit;
    create := DirectoryCreate;
    lookup := DirectoryLookup;
    attach := DirectoryAttach;
    detach := DirectoryDetach;
    getEntries := DirectoryGetEntries;
  END;

PROCEDURE DirectoryInit(self : DirectoryT): NameServer.T =
  BEGIN
    self.id := FileId.Inc(FileIdStart);
    RETURN Directory.T.init(self);
  END DirectoryInit;

PROCEDURE DirectoryCreate (self: DirectoryT; 
				 name: NameServer.Name): NameServer.T
  RAISES {NameServer.Error} = 
  VAR dir: DirectoryT; 
  BEGIN
    dir := NEW(DirectoryT).init();
    self.attach(name,dir);
    RETURN dir;
  END DirectoryCreate;

PROCEDURE DirectoryLookup(<*UNUSED*> self: DirectoryT; 
			  VAR xname: NameServer.Name; 
			  <*UNUSED*> getalias: BOOLEAN): REFANY
  RAISES {NameServer.Error} = 
  VAR
    entry: FileT;
    name: TEXT;
    
  PROCEDURE FetchIt (self: FileT; path: TEXT): INTEGER =
    CONST
      EzLen = 256 * 1024;        (* what we always try first *)
      OkLen = 1024 * 1024;  
      MaxLen = 4 * 1024 * 1024;  (* Max byte size of files we can transmit. *)
    VAR
      buf  : REF ARRAY OF CHAR;
      len  : CARDINAL;
      bytes: INTEGER;
    VAR
      i    : CARDINAL := 0;
      res  : INTEGER  := 0;
    BEGIN
      LOOP
        CASE i OF
        | 0 => len := EzLen;
        | 1 => len := OkLen;
        | 2 => len := MaxLen;
        ELSE EXIT;
        END;

        buf := NEW(REF ARRAY OF CHAR, len);

        TRY
          Salnet.NfsFetch(Salnet.BootServer(), mount,
		          mount_pad & path, buf^, bytes);
	  (*
          Salnet.TftpFetch(Salnet.BootServer(),mount_point & mount_pad &  
                           path, buf^, bytes);
	   *)
        EXCEPT
        | Errno.E(err) =>
          IF err = ErrnoDep.EFBIG THEN
	    bytes := 0;
	    IO.Put("retrying");
	  ELSE
	    IO.Put("TftpFileSystem.Fetchit "&mount_point &"/" &mount_pad &path &": " &Errno.Fmt(err) &"\n");
            RETURN err;
          END;
        | Salnet.Error(err) =>
            IO.Put("TftpFileSystem.Fetchit "& mount_point &"/" & mount_pad &path &": " &Salnet.FmtError(err) &"\n");
            RETURN err;
        END;

        IF bytes > 0 AND bytes < len THEN
          (* Yuk.  Trim the buffer to exact size. *)
          self.data := NEW(REF ARRAY OF CHAR, bytes);
          self.data^ := SUBARRAY(buf^, 0, bytes);
          RETURN 0;
        END;

        INC(i);
      END;

      IO.Put("TftpFileSystem.Fetchit(" & path & ": tftp failed)\n");
      RETURN res;
    END FetchIt;

  BEGIN
    name := NSName.ToText(xname);
    xname.end := 0; (* empty xname since we are going to eat up the
		       path anyway*)
    
    entry := NEW(FileT);
    CASE FetchIt(entry, name) OF 
    | 0 =>
      RETURN entry;
    ELSE
      RAISE NameServer.Error(NameServer.EC.NameNotFound);
    END;
  END DirectoryLookup;
PROCEDURE DirectoryAttach (<*UNUSED*>self: DirectoryT; 
			   <*UNUSED*>READONLY name: NameServer.Name; 
			   <*UNUSED*>obj: REFANY) = 
  BEGIN
    (* do nothing *)
  END DirectoryAttach;

PROCEDURE DirectoryDetach (<*UNUSED*>self: DirectoryT; 
			   <*UNUSED*>READONLY name: NameServer.Name) =
  BEGIN
    (* do nothing *)
  END DirectoryDetach;

PROCEDURE DirectoryGetEntries (<*UNUSED*>self: DirectoryT;
			       <*UNUSED*>cookie: NameServer.Cookie;
			       <*UNUSED*>VAR ent: ARRAY OF NameServer.Entry): CARDINAL =
  BEGIN
    RETURN 0;
  END DirectoryGetEntries;
  
(* ------------------- FILE SERVICES ------------------- *)

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

REVEAL FileT = File.T BRANDED OBJECT
    (* tftp first downloads the entire file and then serves chunks
       from ram.  We keep data around using a simple data pointer.  We
       may want to improve it by using a collection of fixed sized
       buffers.  *)
    data: REF ARRAY OF CHAR; (* Where the bits really live *)
  OVERRIDES
    read    := Read;
    close   := Close;
    open    := Open;
    stat    := Stat;
  END;

PROCEDURE Read (self: FileT;
		VAR data: ARRAY OF CHAR;
		offset: File.OffsetT): CARDINAL RAISES {Error.E} = 
  BEGIN
    IF NUMBER(data) = 0 THEN RETURN 0; END;
    IF self.data = NIL OR offset > NUMBER(self.data^) THEN
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_NOT_IN_FILE));
    END;
    (* copy into user provided buffer *)
    WITH size = MIN(NUMBER(self.data^)-offset,NUMBER(data)),
         outdata = SUBARRAY(self.data^,offset, size) DO
      SUBARRAY(data,0,size) := outdata;
      RETURN size;
    END;
  END Read;

PROCEDURE Open (self: FileT; <*UNUSED*>mode: INTEGER): File.T =
  BEGIN
    RETURN self;
  END Open;

PROCEDURE Close (self: FileT) =
  BEGIN
    self.data := NIL;
  END Close;

PROCEDURE Stat (self: FileT; VAR stat: FileStat.T) =
  BEGIN
    FileStat.Init(stat);
    IF self.data # NIL THEN
      stat.size := NUMBER(self.data^);
    ELSE
      stat.size := 0;
    END;
  END Stat;

(* ------------------- MOUNT SERVICES ------------------- *)

(* The type "MountT" represents a mount point exported by this file
   system.  The tftp file system does not maintain additional
   information for mount points, consequently is acts just like a
   regular DiretoryT.  A MountT is a subtype of a DirectoryT. *)

TYPE MountT = DirectoryT OBJECT
  tftpServer: TEXT;
END;

TYPE FileSystemT = FileSystem.T OBJECT
  OVERRIDES
    (* "create" is called when we create a new mount point *)
    newfs := Newfs;
  END;

PROCEDURE Newfs (<*UNUSED*>self: FileSystemT; 
		 <*UNUSED*>READONLY name: NameServer.Name): Directory.T =
  VAR new: MountT;
  BEGIN
    new := NEW(MountT).init();
    RETURN new;
  END Newfs;

BEGIN 
  TRY
    FileSystem.Register("tftp", NEW(FileSystemT));
  EXCEPT
  | Error.E(e) =>
    IO.Put(e.message()&" during TftpFileSystem initialization.\n");
  END;

  TRY
    VAR
      name: NameServer.Name;
      dir: Directory.T;
    BEGIN
      dir := FileSystem.GetRoot();
      (* create a directory *)
      name := NSName.FromText("spin");
      EVAL dir.create(name);
      
      FileSystem.Mount("tftp", "daffy", "/spin");
    END;
    Salnet.NfsMount(Salnet.BootServer(), mount_point, mount);
  EXCEPT
  | Errno.E, Salnet.Error, Error.E, NameServer.Error => 
    IO.PutError("TftpFileSystem creating /tftp directory failed.");
  END;

END TftpFileSystem.

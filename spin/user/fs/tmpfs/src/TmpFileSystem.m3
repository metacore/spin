(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 22-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NameServer.Name representation.
 * 4-oct-96  becker at the University of Washington
 *	exported the domains procedures and mkdir /proc
 *
 * 16-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

(* "TmpFileSystem" implements a memory based file system interface *)

MODULE TmpFileSystem;
IMPORT TmpFileSystemInterface;
IMPORT Error, Directory, File, FileId, FileStat, FileSystem, IO,
       NameServer;

(* ------------------- FILEID SUPPORT ------------------- *)

CONST NULLCHAR = VAL(16_00,CHAR);
VAR FileIdStart := FileId.T{   
       't',      'm',      'p',      'f',      's', NULLCHAR,
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
    init := Init;
    (* "create" allocates a new DirectoryT for tmpfs *)
    create := Create;

    (* tmpfs reuses the Directory.T "attach", 
       "detach" and "lookup" methods *)

    (* "mkfile" creates a new FileT for tmpfs *)
    mkfile := Mkfile;
  END;

PROCEDURE Init(self : DirectoryT): NameServer.T =
  BEGIN
    self.id := FileId.Inc(FileIdStart);
    RETURN Directory.T.init(self);
  END Init;

PROCEDURE Create(self: DirectoryT; 
		 name: NameServer.Name) : NameServer.T
  RAISES {NameServer.Error} = 
  VAR dir: DirectoryT;
  BEGIN
    dir := NEW(DirectoryT).init();
    (* create directory in parent directory *)
    self.attach(name, dir);
    (* create self reference in the new directory *)
    NameServer.Attach(dir, ".", dir);
    NameServer.Attach(dir, "..", self);
    RETURN dir;
  END Create;

PROCEDURE Mkfile (self: DirectoryT;
		  name: NameServer.Name): File.T RAISES {Error.E} =
  VAR fp: FileT;
  BEGIN
    fp := NEW(FileT);
    fp.id := FileId.Inc(FileIdStart);
    self.attach(name, fp);
    RETURN fp;
  END Mkfile;

(* ------------------- FILE SERVICES ------------------- *)

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

REVEAL FileT = File.T BRANDED OBJECT
    (* tmpfs is a RAM file system.  We keep data around using a simple
       data pointer.  We may want to improve it by using a collection
       of fixed sized buffers.  *)
    data: REF ARRAY OF CHAR; (* Where the bits really live *)
  OVERRIDES
    read     := methodFileRead;
    write    := methodFileWrite;
    close    := methodFileClose;
    open     := methodFileOpen;
    stat     := methodFileStat;
  END;

PROCEDURE methodFileRead(
    self    : FileT;
    VAR data : ARRAY OF CHAR;
    offset   : File.OffsetT): CARDINAL 
  RAISES {Error.E} = 
  BEGIN
    IF NUMBER(data) = 0 THEN RETURN 0; END;
    IF self.data = NIL OR offset > NUMBER(self.data^) THEN
      RAISE
        Error.E(NEW(File.ErrorT).init(File.FS_NOT_IN_FILE));
    END;
    (* copy into user provided buffer *)
    WITH size = MIN(NUMBER(self.data^)-offset,NUMBER(data)),
         outdata = SUBARRAY(self.data^,offset, size) DO
      SUBARRAY(data,0,size) := outdata;
      RETURN size;
    END;
  END methodFileRead;

PROCEDURE methodFileWrite (
			   self          : FileT;
			   READONLY data : ARRAY OF CHAR;
			   offset        : File.OffsetT; 
			   ): CARDINAL 
  RAISES {Error.E} <*NOWARN*> =
  VAR
    dataSize, bufferSize : CARDINAL;
  BEGIN
    dataSize := NUMBER(data);
    IF self.data = NIL THEN
      self.data := NEW(REF ARRAY OF CHAR, dataSize+offset);
    END;

    (* do we need to extend the buffer ? *)
    bufferSize := NUMBER(self.data^);
    IF offset + dataSize > bufferSize THEN
      VAR
        buffer : REF ARRAY OF CHAR;
      BEGIN
        buffer := self.data;
        self.data := NEW(REF ARRAY OF CHAR,offset + dataSize);
        SUBARRAY(self.data^,0,NUMBER(buffer^)) := buffer^;
      END;
    END;

    SUBARRAY(self.data^,offset,dataSize) := data;
    RETURN NUMBER(data);
  END methodFileWrite;

PROCEDURE methodFileOpen (
    self: FileT;
    <*UNUSED*>mode : INTEGER): File.T
  RAISES { Error.E } <*NOWARN*> =
  VAR
  BEGIN
    (* XXX access check *)
    RETURN self;
  END methodFileOpen;

PROCEDURE methodFileClose (<* UNUSED *> self: FileT) 
  RAISES {Error.E} <*NOWARN*> =
  BEGIN
    (* do nothing *)
  END methodFileClose;

PROCEDURE methodFileStat (self: FileT; VAR stat : FileStat.T) =
  BEGIN
    FileStat.Init(stat);
    IF self.data # NIL THEN 
      stat.size := NUMBER(self.data^);
    ELSE
      stat.size := 0;
    END;
  END methodFileStat;

(* ------------------- MOUNT SERVICES ------------------- *)

(* The type "MountT" represents a mount point exported by this file
   system.  The tmp file system does not maintain additional
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

PROCEDURE Newfs(<*UNUSED*>self: FileSystemT; 
		<*UNUSED*>READONLY name: NameServer.Name): Directory.T =
  VAR dir: MountT;
  BEGIN
    (* create new mount point directory *)
    dir := NEW(MountT).init();
    (* create self reference in new directory *)
    NameServer.Attach(dir, ".", dir);
    NameServer.Attach(dir, "..", NameServer.Root());
    (* XXX we assume that tmpfilesystem is ALWAYS mounted at fs root.
       Thus, ".." points to the ns root. *)
    RETURN dir;
  END Newfs;

BEGIN 
  EVAL TmpFileSystemInterface.Export(NIL);
  TRY
    FileSystem.Register("tmpfs", NEW(FileSystemT));
  EXCEPT
  | Error.E(e) =>
    IO.Put(e.message()&" during TmpFileSystem initialization.\n");
  END;
  TRY 
    (* Make TmpFS the root filesystem *)
    FileSystem.Mount("tmpfs", "bogodevice1", "/");
    
    (* Make /proc dir *)
    NameServer.Attach(FileSystem.GetRoot(), "proc",
		      NEW(NameServer.Default).init());
  EXCEPT
  | Error.E(e) =>
    IO.Put(e.message()&" during TmpFileSystem mount onto /.\n");
  END;
END TmpFileSystem.

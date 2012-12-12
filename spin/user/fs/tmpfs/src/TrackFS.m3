(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

MODULE TrackFS;
IMPORT Error, Directory, File, FileId, FileStat, FileSystem, IO,
       NameServer, Word;
IMPORT Track, TextWr, TextF;
IMPORT Fmt;
IMPORT NSName;

TYPE FileT      <: File.T;
TYPE DirectoryT <: Directory.T;

(* ------------------- FILEID SUPPORT ------------------- *)

CONST NULLCHAR = VAL(16_00,CHAR);
VAR FileIdStart := FileId.T{   
       't',      'r',      'a',      'c',      'k', NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR};

(* ------------------- DIRECTORY SERVICES ------------------- *)

(* The "DirectoryT" defines what a directory looks like for this
   filesystem.  A DirectoryT is a subtype of fscore's Directory.T *)

REVEAL DirectoryT = Directory.T BRANDED OBJECT
    trackTable : Track.ResourceTable;
    parentDir : Directory.T;
    didDots := FALSE;
x:TEXT;
  METHODS
  OVERRIDES
    init := methodDirectoryInit;
    (* "create" allocates a new DirectoryT for tmpfs *)
    create := methodDirectoryCreate;

    (* "mkfile" creates a new FileT for tmpfs *)
    mkfile := Mkfile;
    getEntries := GetEntries;
    lookup := methodLookup;

  END;

PROCEDURE methodDirectoryInit(self : DirectoryT): NameServer.T =
  BEGIN
    self.id := FileId.Inc(FileIdStart);
    RETURN Directory.T.init(self);
  END methodDirectoryInit;

PROCEDURE methodDirectoryCreate(
    self: DirectoryT; 
    name : NameServer.Name) : NameServer.T = 
  VAR dir: DirectoryT;
  BEGIN
    TRY
      dir := NEW(DirectoryT).init();
      (* create directory in parent directory *)
      self.attach(name,dir);
      (* create self reference in new directory *)
      NameServer.Attach(dir, ".", dir);
      (* create parent reference in new directory *)
      NameServer.Attach(dir, "..", self);
    EXCEPT
    | NameServer.Error =>
      IO.Put("TrackFS.DirectoryCreate: NameSever Error\n");
    END;
    RETURN dir;
  END methodDirectoryCreate;

PROCEDURE Mkfile (self: DirectoryT;
		  name: NameServer.Name): File.T RAISES {Error.E} =
  VAR fp : FileT;
  BEGIN
    fp := NEW(FileT);
    fp.id := FileId.Inc(FileIdStart);
    self.attach(name, fp);
    RETURN fp;
  END Mkfile;

PROCEDURE methodLookup(
    self          : DirectoryT; 
    VAR name          : NameServer.Name; 
    getalias      : BOOLEAN): REFANY 
  RAISES {NameServer.Error} = 
  VAR entry      : REFANY;
    topdir : NameServer.Name;
    dir : DirectoryT;
  BEGIN
    IF name.end = 0 THEN RETURN self; END;

    (* strip top dir from path *)
    NameServer.GetComponent(name, topdir);
    
    IF topdir.str[topdir.from] = '.' THEN
      IF topdir.end = topdir.from+2 AND
	topdir.str[topdir.from+1] = '.' THEN
	entry := self.parentDir;
      ELSIF topdir.end = topdir.from+1 THEN
	entry := self;
      ELSE
	entry := self.trackTable.lookup(topdir, getalias);
      END;
    ELSE
      entry := self.trackTable.lookup(topdir, getalias);
    END;
    
    TYPECASE entry OF
    | NULL => 
    | Track.ResourceTable(table) =>
      (* wrap the table in a DirectoryT object *)
      dir := NEW(DirectoryT).init();
      dir.parentDir := self;
      dir.trackTable := table;
      entry := dir;
    | Track.T(track) =>
      (* wrap the track obj in a FileT object *)
      entry := NEW(FileT, track:=track);
    ELSE
    END;
    RETURN entry;
  END methodLookup;

PROCEDURE GetEntries (dir: DirectoryT; offset: Word.T;
		      VAR ent: ARRAY OF NameServer.Entry): CARDINAL =
  VAR
    entries:=0;
  BEGIN
    IF offset=0 AND NUMBER(ent)<2 THEN
      IO.Put("GetEnt offet "&Fmt.Int(offset)&" ent"&Fmt.Int(NUMBER(ent))&" \n");
      RETURN 0;
    END;

    IF dir.didDots THEN
      dir.didDots:=FALSE;
      RETURN 0;
    END;

    entries := dir.trackTable.getEntries(offset,ent);
    IF entries=0 THEN
      ent[entries].name := NSName.FromText(".");
      ent[entries].cookie := offset+entries+1;
      VIEW(ent[entries].id,INTEGER) := offset+entries;
      INC(entries);

      ent[entries].name := NSName.FromText("..");
      ent[entries].cookie := offset+entries+1;
      VIEW(ent[entries].id,INTEGER) := offset+entries;
      INC(entries);

      dir.didDots := TRUE;
    END;

    RETURN entries;
  END GetEntries;
  

(* ------------------- FILE SERVICES ------------------- *)

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

REVEAL FileT = File.T BRANDED OBJECT
    (* tmpfs is a RAM file system.  We keep data around using a simple
       data pointer.  We may want to improve it by using a collection
       of fixed sized buffers.  *)
(*     data  : TEXT := NIL; REF ARRAY OF CHAR; Where the bits really live *)
    data  : TEXT := NIL; (* REF ARRAY OF CHAR; Where the bits really live *)
    track : Track.T; (* the track object we are wrapping *)
  OVERRIDES
    read     := methodFileRead;
    open     := methodFileOpen;
    stat     := methodFileStat;
  END;

PROCEDURE methodFileRead(self    : FileT;
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

PROCEDURE methodFileOpen (
    self: FileT;
    <*UNUSED*>mode : INTEGER): File.T
  RAISES { Error.E } <*NOWARN*> =
  VAR
    wr := TextWr.New();
    f := NEW(FileT);
  BEGIN
    self.track.print(wr);
    f.data := TextWr.ToText(wr);
    f.track := self.track;
    RETURN f;
  END methodFileOpen;

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

(* The type "FileSystemT" is used to register the filesystem with
   fscore.  A FileSystemT is a subtype of fscore's FileSystem.T. *)

TYPE FileSystemT = FileSystem.T OBJECT
x:TEXT;
  OVERRIDES
    (* "create" is called when we create a new mount point *)
    newfs := Newfs;
  END;

PROCEDURE Newfs (<*UNUSED*>self: FileSystemT; 
		 <*UNUSED*>READONLY name: NameServer.Name) : Directory.T = 
  VAR new: DirectoryT;
  BEGIN
    (* create new mount point directory *)
    new := NEW(DirectoryT).init();
    new.trackTable := Track.GlobalTrack();
    new.parentDir := FileSystem.GetRoot(); (* XXX bogus *)
    RETURN new;
  END Newfs;

BEGIN
  VAR name: NameServer.Name;
  BEGIN
    TRY 
      FileSystem.Register("trackfs", NEW(FileSystemT));
      (* Make /track dir *)
      name := NSName.FromText("track");
      EVAL FileSystem.GetRoot().create(name);
      FileSystem.Mount("trackfs", "bogodevice", "/track");
    EXCEPT
    | NameServer.Error =>
      IO.Put("NS error during TrackFS mount onto /.\n");
    | Error.E(e) =>
      IO.Put(e.message()&" during TrackFS initialization.\n");
    END;
  END;
END TrackFS.

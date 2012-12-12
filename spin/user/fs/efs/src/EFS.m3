(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 22-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NameServer.Name representation.
 * 24-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added additional consistency check in ReadMetaData scan to
 *      avoid picking up garbages.
 * 05-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* "EFS" implements an extent based file system *)

MODULE EFS;

IMPORT Disk, FileStat, File, Ctypes, FileSystem, Error, Text,
       Device, Directory, FileId, NameServer, IO, Fmt, EFSInterface, Auth;
IMPORT Word;
IMPORT NSName;
IMPORT Debugger; <*NOWARN*>

VAR FileIdStart := FileId.T{ 'e', 'f', 's', '\000', ..};

TYPE
  VnodeT = RECORD
    mp: MountT;
    pos: CARDINAL;
    size: CARDINAL;
  END;
  FileName = ARRAY [0 .. 22] OF CHAR;

CONST SecSize = 512;
  
(* ------------------- DIRECTORY SERVICES ------------------- *)

(* The "DirectoryT" defines what a directory looks like for this
   filesystem.  A DirectoryT is a subtype of fscore's Directory.T *)

REVEAL DirectoryT = Directory.T BRANDED OBJECT
    mu: MUTEX;
    vnode: VnodeT;
    metaData: ALIGNED 32 FOR MetaDataT;
  METHODS
    init(): DirectoryT := DirectoryInit;
  OVERRIDES
    create := DirectoryCreate;
    (* "create" allocates a new DirectoryT for efs *)
    lookup  := DirectoryLookup;
    attach := DirectoryAttach;
    detach := DirectoryDetach;
    mkfile := DirectoryMkfile;
    getEntries := GetEntries;
  END;

PROCEDURE RoundUp(<*UNUSED*>mp: MountT; v: CARDINAL): CARDINAL =
  BEGIN
    RETURN Word.And(v-1, Word.Not(SecSize-1)) + SecSize;
  END RoundUp;
  
PROCEDURE DirectoryInit(self: DirectoryT): DirectoryT =
  BEGIN
    self.id := FileId.Inc(FileIdStart);
    self.mu := NEW(MUTEX);
    RETURN Directory.T.init(self);
  END DirectoryInit;
  
PROCEDURE MakeEntry (self: DirectoryT;
		     name: NameServer.Name; pos, size: CARDINAL;
		     type: INTEGER): REFANY =
  BEGIN
    CASE type OF 
    | TypeFile =>
      VAR
	fp: FileT;
      BEGIN
	fp := NEW(FileT);
	fp.id := FileId.Inc(FileIdStart);
	fp.vnode.mp := self.vnode.mp;
	fp.vnode.pos  := pos;
	fp.vnode.size := size;
	fp.name := NSName.DeepCopy(name);
	fp.dir := self;
	RETURN fp;
      END;
    | TypeDirectory =>
      VAR 
	dir: DirectoryT;
      BEGIN
	dir            := NEW(DirectoryT).init();
	dir.vnode.mp   := self.vnode.mp;
	dir.vnode.pos  := pos;
	dir.vnode.size := size;
	RETURN dir;
      END;
    ELSE
      <*ASSERT FALSE*>
      RETURN NIL;
    END;
  END MakeEntry;
  
PROCEDURE DirectoryCreate(self: DirectoryT; 
			  name: NameServer.Name): NameServer.T
  RAISES {NameServer.Error} =
  VAR dir: DirectoryT;
      size: CARDINAL := BYTESIZE(dir.metaData);
  BEGIN
    dir    := NEW(DirectoryT).init();
    dir.vnode.pos  := AllocateRegion(self.vnode.mp.dev, size);
    dir.vnode.size := size;
    dir.vnode.mp   := self.vnode.mp;
    dir.metaData   := MetaDataT{'\000', ..};
    WriteMetaData(dir);
    self.attach(name,dir);
    RETURN dir;
  END DirectoryCreate;

PROCEDURE AllocateRegion (disk: Disk.T; size: CARDINAL): INTEGER
    RAISES {Error.E}=
  VAR
    stat: Disk.Stat;
    pos: INTEGER;

  (* Returns TRUE if the region [start..end] is not occupied by
     any files or directories under "dir". "dir" is the start byte offset
     of the directory. *)
  PROCEDURE RegionIsFree (dir, start, end: CARDINAL): BOOLEAN RAISES {Error.E}=
    VAR
      metaData: MetaDataT;
    BEGIN
      IF start >= stat.nSecs * stat.secSize THEN
	(* region past the end of the disk. *)
	RETURN FALSE;
      END;

      (* read the directory contents *)
      EVAL disk.read(metaData, dir);
    
      FOR idx := 0 TO NUMBER(metaData)-DISize BY DISize DO
	WITH inoBuf = SUBARRAY(metaData, idx, DISize),
	  ino = VIEW(inoBuf,DiskInode) DO 
	  IF ValidInode(ino, SecSize) AND ino.size > 0 THEN
	    CASE ino.type OF
	    | TypeFile =>
	      IF start <= ino.pos AND end > ino.pos THEN
		RETURN FALSE;
	      END;
	      IF end >= ino.pos+ino.size AND start < ino.pos+ino.size THEN
		RETURN  FALSE;
	      END;
	    | TypeDirectory =>
	      IF NOT RegionIsFree(ino.pos, start, end) THEN
		RETURN FALSE;
	      END;
	    ELSE
	      <*ASSERT FALSE*>
	    END;
	  END;
	END;
      END;
      RETURN TRUE;
    END RegionIsFree;

  PROCEDURE Sub (dir: CARDINAL): INTEGER RAISES {Error.E} =
    VAR metaData: MetaDataT;
      pos: INTEGER;
    BEGIN
      EVAL disk.read(metaData, dir);
      FOR idx := 0 TO NUMBER(metaData)-DISize BY DISize DO
	WITH inoBuf = SUBARRAY(metaData, idx, DISize),
	  ino = VIEW(inoBuf,DiskInode) DO 
	  IF ValidInode(ino, SecSize) AND ino.pos # 0 THEN
	    (* ino=0 when file is just created and not brked. we have to
	       ignore such file because the block 0 is implicitly used
	       by the root directory.. *)
	    CASE ino.type OF
	    | TypeFile =>
	      pos := ino.pos+ino.size;
	      IF RegionIsFree(0, pos, pos+size) THEN
		RETURN pos;
	      END;
	    | TypeDirectory =>
	      pos := ino.pos+ino.size;
	      IF RegionIsFree(0, pos, pos+size) THEN
		RETURN pos;
	      END;
	      pos := Sub(size);
	      IF pos >= 0 THEN
		RETURN pos;
	      END;
	    ELSE
	      <*ASSERT FALSE*>
	    END;
	  END;
	END;
      END;
      RETURN -1;
    END Sub;
    
  BEGIN
    disk.stat(stat);
    IF RegionIsFree(0, MetaBlockSize, size) THEN
      (* Place right after the root is free. *)
      pos := MetaBlockSize;
    ELSE
      pos := Sub(0);
    END;
    IF pos < 0 THEN
      IO.PutError("EFS.Allocate: " & Fmt.Int(size, 16) & " failed.\n");
      Debugger.Enter();
    ELSE
      IO.Put("EFS.Allocate: " & Fmt.Int(pos, 16)
	     & " .. " & Fmt.Int(pos+size, 16) & " successful.\n");
    END;
    RETURN pos;
  END AllocateRegion;
  
PROCEDURE Lookup (dir: DirectoryT; READONLY name: NameServer.Name): REFANY
  RAISES {NameServer.Error} =
  BEGIN
    (* read in the metaData from device *)
    ReadMetaData(dir);

    FOR idx := 0 TO NUMBER(dir.metaData)-DISize BY DISize DO
      WITH inoBuf = SUBARRAY(dir.metaData, idx, DISize),
	   ino = VIEW(inoBuf,DiskInode) DO 
	IF ValidInode(ino, SecSize) AND CompareName(ino.name, name) THEN
	  RETURN MakeEntry(dir, name, ino.pos, ino.size, ino.type);
	END;
      END;
    END;
    RETURN NIL;
  END Lookup;
  
PROCEDURE DirectoryLookup(self: DirectoryT;
			  VAR name: NameServer.Name;
			  <*UNUSED*>getalias: BOOLEAN): REFANY RAISES {NameServer.Error} = 
  VAR
    entry: REFANY;
    component: NameServer.Name;
  BEGIN
    (* extract a component from the path *)
    NameServer.GetComponent(name, component);
    entry := Lookup(self, component);
    IF entry = NIL THEN
      RAISE NameServer.Error(NameServer.EC.NameNotFound);
    END;
    RETURN entry;
  END DirectoryLookup;

PROCEDURE DirectoryAttach(self: DirectoryT;
			  READONLY name: NameServer.Name;
			  obj: REFANY) =
  BEGIN
    (* update the metadata and write to disk *)
    TYPECASE obj OF 
    | NULL => 
      IO.PutError("EFS.attach: cannot add null file.\n");
    | FileT(file) =>
      IF AddEntry(self, name, file.vnode.pos, file.vnode.size, TypeFile) THEN
        WriteMetaData(self);
      END;
    | DirectoryT(dir) =>
      IF AddEntry(self, name, dir.vnode.pos, dir.vnode.size,
		  TypeDirectory) THEN
        WriteMetaData(self);
      END;
    ELSE
      IO.PutError("EFS.attach cannot attach unknown type file.\n");
    END;
  END DirectoryAttach;

PROCEDURE DirectoryDetach(self: DirectoryT;
			  READONLY name: NameServer.Name) =
  BEGIN
    IF DelEntry(self, name) THEN 
      (* update directory metadata *)
      WriteMetaData(self);
    ELSE
      IO.PutError("EFS.detach file ");
      IO.Put("foo does not exist.\n")
    END;
  END DirectoryDetach;

PROCEDURE DirectoryMkfile(self: DirectoryT;
			  name: NameServer.Name): File.T =
  VAR
    fp: FileT;
  BEGIN
    IF Lookup(self, name) = NIL THEN
      fp := NEW(FileT);
      fp.vnode.pos  := 0;
      fp.vnode.size := 0;
      fp.vnode.mp   := self.vnode.mp;
      fp.name := NSName.DeepCopy(name);
      fp.dir := self;
      fp.id := FileId.Inc(FileIdStart); (* set file id *)
      self.attach(name,fp);
      RETURN fp;
    ELSE
      IO.PutError("EFS.Mkfile: file already exists.\n");
    END;
    RETURN NIL;
  END DirectoryMkfile;
  
PROCEDURE GetEntries (dir: DirectoryT; offset: Word.T;
		      VAR ent: ARRAY OF NameServer.Entry): CARDINAL =
  VAR
    entIdx: CARDINAL := 0;
    name: TEXT;
  BEGIN
    (* read in the metaData from device *)
    ReadMetaData(dir);
    
    FOR idx := offset TO NUMBER(dir.metaData)-DISize BY DISize DO
      WITH inoBuf = SUBARRAY(dir.metaData, idx, DISize),
           ino = VIEW(inoBuf,DiskInode) DO
	name := GetFileNameFromInode(ino);
	IF ValidInode(ino, SecSize) AND name # NIL THEN
	  ent[entIdx].name := NSName.FromText(name);
	  ent[entIdx].cookie := idx + DISize;
	  VIEW(ent[entIdx].id, Ctypes.unsigned_int) := ino.pos;
	  INC(entIdx);
	  IF entIdx > LAST(ent) THEN EXIT; END;
	END;         
      END;
    END;
    RETURN entIdx;
  END GetEntries;

(* ------------------- FILE SERVICES ------------------- *)

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

REVEAL FileT = File.T BRANDED OBJECT
    vnode: VnodeT;
    dir: DirectoryT;
    name: NameServer.Name;
  OVERRIDES
    read     := Read;
    write    := Write;
    stat     := Stat;
    truncate := Truncate;
  END;
  
PROCEDURE MakeSureOffsetIsBlockAligned(val: INTEGER): BOOLEAN =
  BEGIN
    IF val < 0 OR val MOD 512 # 0 THEN
      IO.Put("efs: warning: 16_" & Fmt.Int(val, 16) & " is not aligned.\n");
    END;
    RETURN TRUE;
  END MakeSureOffsetIsBlockAligned;
  
PROCEDURE Read(self: FileT;
	       VAR data: ARRAY OF CHAR;
	       offset: File.OffsetT): CARDINAL RAISES {Error.E} = 
  BEGIN
    <*ASSERT MakeSureOffsetIsBlockAligned(offset)*>
    <*ASSERT MakeSureOffsetIsBlockAligned(NUMBER(data))*>

    IF self.vnode.pos # 0 THEN 
      RETURN self.vnode.mp.dev.read(data, self.vnode.pos + offset);
    END;
    RETURN 0;
  END Read;


PROCEDURE GrowFile (self: FileT; size: CARDINAL) RAISES {Error.E} = 
  VAR
    newPos: CARDINAL;
    newSize: CARDINAL;

    data: REF ARRAY OF CHAR;
    bytes: CARDINAL;
    offset: CARDINAL;

    rsize,wsize: CARDINAL;
  BEGIN
    IO.Put("EFS.GrowFile: " & Fmt.Int(self.vnode.size) & "->"
	   & Fmt.Int(size) & ".\n");
    newSize := size;
    newPos  := AllocateRegion(self.vnode.mp.dev, size);

    (* copy old extent data into new extent *)
    size := self.vnode.size;
    IF size > 0 THEN 
      bytes  := MIN(8192,size);
      data   := NEW(REF ARRAY OF CHAR,bytes);
      offset := 0;

      REPEAT
        WITH d = SUBARRAY(data^,0,bytes) DO
	  TRY
	    rsize := self.vnode.mp.dev.read(d, self.vnode.pos + offset);
	    wsize := self.vnode.mp.dev.write(d, newPos + offset);
	  EXCEPT
	  | Error.E(e) =>
	    IO.PutError("EFS.GrowFile: " & e.message());
	    EXIT;
	  END;
          IF wsize # rsize THEN 
            IO.PutError("EFS.GrowFile: write failure " &
			Fmt.Int(rsize) & "<=>" & Fmt.Int(wsize));
	    EXIT;
          END;
        END;
        INC(offset,bytes);
        DEC(size, bytes);
        bytes := MIN(bytes,size);
      UNTIL size = 0;

    END;

    (* swap old ext/vnode info with new ext/vnode info *)
    self.vnode.size := newSize;
    self.vnode.pos  := newPos;
    EVAL UpdateEntry(self.dir, self.name, self.vnode.pos, self.vnode.size);
    WriteMetaData(self.dir);
  END GrowFile;

PROCEDURE Write (self: FileT;
		 READONLY data: ARRAY OF CHAR;
		 recnum: File.OffsetT): CARDINAL RAISES {Error.E} =
  VAR
    dataSize: CARDINAL;
  BEGIN
    <*ASSERT MakeSureOffsetIsBlockAligned(recnum)*>
    <*ASSERT MakeSureOffsetIsBlockAligned(NUMBER(data))*> 
    dataSize := NUMBER(data);

    (* need to grow the extent *)
    IF recnum + dataSize > self.vnode.size THEN
      GrowFile(self, RoundUp(self.dir, recnum + dataSize));
    END;
    
    (* write out the data *)
    RETURN self.vnode.mp.dev.write(data, self.vnode.pos + recnum);
  END Write;

PROCEDURE Stat (self: FileT; VAR stat: FileStat.T) =
  BEGIN
    FileStat.Init(stat);
    stat.ino := self.vnode.pos;
    stat.size := self.vnode.size;
  END Stat;

PROCEDURE Truncate (self: FileT; newSize: CARDINAL) RAISES {Error.E} =
  BEGIN
    IF self.vnode.size > newSize THEN
      IO.Put("efs.truncate: shrink not supported.\n");
      RAISE Error.E(NEW(Error.T).init(File.FS_NOT_SUPPORTED));
    END;
    GrowFile(self, RoundUp(self.dir, newSize));
  END Truncate;
  

(* ------------------- MOUNT SERVICES ------------------- *)

(* The type "MountT" represents a mount point exported by this file
   system.  The tmp file system does not maintain additional
   information for mount points, consequently is acts just like a
   regular DiretoryT.  A MountT is a subtype of a DirectoryT. *)

TYPE MountT = DirectoryT OBJECT
  dev: Disk.T;
  devName: TEXT;
  size: CARDINAL;
END;


(* The type "FileSystemT" is used to register the filesystem with
   fscore.  A FileSystemT is a subtype of fscore's FileSystem.T. *)

TYPE FileSystemT = FileSystem.T OBJECT
  OVERRIDES
    newfs := Newfs;
    deletefs := Deletefs;
  END;

PROCEDURE Newfs(<*UNUSED*>self: FileSystemT; 
		READONLY name: NameServer.Name): Directory.T RAISES {Error.E} = 
  VAR mp: MountT;
  BEGIN
    mp := NEW(MountT).init();
    mp.vnode.mp := mp;
    (* get the extent device *)
    mp.dev := Device.Lookup(NSName.ToText(name));
    
    (* open the device for filesystem use *)
    mp.dev.open();
    RETURN mp;
  END Newfs;

PROCEDURE Deletefs (<*UNUSED*>self: FileSystemT; root: Directory.T) =
  VAR mp: MountT := root;
  BEGIN
    mp.dev.close();
  END Deletefs;

(* ------------------- LOW LEVEL FS SERVICES ------------------- *)

CONST MetaBlockSize = 4096; (* XXX probably off by one error in M3 *)
TYPE MetaDataT = ARRAY [1..MetaBlockSize] OF CHAR;

CONST
  TypeFile = 16_55;
  TypeDirectory = 16_56;
  TypeVoid = 16_57;
  
CONST DISize = BYTESIZE(DiskInode);
TYPE DiskInode = ALIGNED 32 FOR RECORD
  name: FileName;
  type: Ctypes.unsigned_char;
  pos: Ctypes.unsigned_int;
  size: Ctypes.unsigned_int;
END;

(* Parse the disk inode "ino" and return the file name as a text.
   If inode is not a valid one, this returns NIL. *)
PROCEDURE GetFileNameFromInode (READONLY ino: DiskInode): TEXT =
  VAR
    len: CARDINAL := 0;
  BEGIN
    IF ino.name[0] = '\000' THEN RETURN NIL; END;
    
    WHILE ino.name[len] # '\000' DO
      IF len = LAST(ino.name) THEN
	(* Couldn't find the null char. *)
	RETURN NIL;
      END;
      INC(len);
    END;
    RETURN Text.FromChars(SUBARRAY(ino.name, 0, len));
  END GetFileNameFromInode;

PROCEDURE ValidInode (READONLY ino: DiskInode; secSize: CARDINAL): BOOLEAN =
  BEGIN
    IF ino.type >= TypeFile AND ino.type <= TypeDirectory
      AND ino.pos MOD secSize = 0 
      AND ino.size MOD secSize = 0 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END ValidInode;

PROCEDURE ReadMetaData (dir: DirectoryT) =
  BEGIN
    TRY
      EVAL dir.vnode.mp.dev.read(dir.metaData, dir.vnode.pos);
    EXCEPT
    | Error.E(e) =>
      IO.Put("EFS.ReadMetaData: " & e.message() & ".\n");
    END;
  END ReadMetaData;
  

PROCEDURE WriteMetaData (dir: DirectoryT) = 
  BEGIN
    TRY
      EVAL dir.vnode.mp.dev.write(dir.metaData, dir.vnode.pos);
    EXCEPT
    | Error.E(e) =>
      IO.Put("EFS.WriteMetaData: " & e.message() & ".\n");
    END;
  END WriteMetaData;

PROCEDURE AddEntry(dir: DirectoryT;
		   name: NameServer.Name; 
		   pos, size: CARDINAL; 
		   type: INTEGER): BOOLEAN=
  BEGIN
    ReadMetaData(dir);
    
    FOR idx := 0 TO NUMBER(dir.metaData)-DISize BY DISize DO
      WITH inoBuf = SUBARRAY(dir.metaData, idx, DISize),
           ino = VIEW(inoBuf,DiskInode) DO
	IF NOT ValidInode(ino, SecSize) THEN
	  VAR len := name.end-name.from;
	  BEGIN
            IF len >= NUMBER(ino.name) THEN
              IO.PutError("EFS name to long. Truncating.\n");
            END;
	    SUBARRAY(ino.name, 0, len) := SUBARRAY(name.str^, name.from, len);
            ino.name[len] := '\000';
            ino.pos := pos;
            ino.size := size;
	    ino.type := type;
	    RETURN TRUE;
          END;
        END;
      END;
    END;
    RETURN FALSE;
  END AddEntry;

PROCEDURE DelEntry(dir: DirectoryT; name: NameServer.Name): BOOLEAN =
  BEGIN
    ReadMetaData(dir);
    FOR idx := 0 TO NUMBER(dir.metaData)-DISize BY DISize DO
      WITH inoBuf = SUBARRAY(dir.metaData, idx, DISize),
           ino = VIEW(inoBuf,DiskInode) DO
	IF ValidInode(ino, SecSize) AND CompareName(ino.name, name) THEN
	  ino.name[0] := '\000';
	  ino.pos := 0;
	  ino.size := 0;
	  ino.type := TypeVoid;
	  RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END DelEntry;

PROCEDURE UpdateEntry (dir: DirectoryT; name: NameServer.Name;
		       newPos, newSize: CARDINAL): BOOLEAN =
  BEGIN
    ReadMetaData(dir);
    FOR idx := 0 TO NUMBER(dir.metaData)-DISize BY DISize DO
      WITH inoBuf = SUBARRAY(dir.metaData, idx, DISize),
           ino = VIEW(inoBuf,DiskInode) DO
	IF ValidInode(ino, SecSize) AND CompareName(ino.name, name) THEN
          ino.pos     := newPos;
          ino.size    := newSize;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END UpdateEntry;

PROCEDURE CompareName (READONLY a: FileName; b: NameServer.Name): BOOLEAN =
  VAR len := MIN(BYTESIZE(FileName)-1, b.end-b.from);
  BEGIN
    FOR i := 0 TO len-1 DO
      IF a[i] # b.str[b.from+i] THEN 
        RETURN FALSE; 
      END;
    END;
    IF a[len] # '\000' THEN
      RETURN FALSE;
    END;
    RETURN TRUE;
  END CompareName;

BEGIN
  TRY
    EVAL EFSInterface.Export(NEW(Auth.AuthAlways));
    FileSystem.Register("efs", NEW(FileSystemT));
  EXCEPT
  | Error.E(e) =>
    IO.PutError(e.message()&" during ExtentFileSystem initialization.\n");
  END;
END EFS.

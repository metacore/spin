(*
  Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Oct-97  Tsutomu Owa (owa) at the University of Washington
 *	Hack made by XXX in ReadSuperBlock() to handle partition > 1GB.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Improved performance by reading consecutive blocks at once and
 *	using Gun's buffer support.  A few bug fix.
 *
 * 10-Sep-96  Eric Christoffersen (ericc) at the University of Washington
 *	Write used to call segBuffer write for EACH block to be written.  Write now
 *	calls segBuffer write once if first block isn't block aligned, once each time
 *	the segBuffer is filled up, and once if the last block is unaligned.  Made
 *	noticable speed improvement.  Deleted a bunch of chaff.
 *
 * 14-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Changed sync to only write dirty segBuff data to disk.  Changed mount to work with it.
 *
 * 04-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Added and tested mkdir.  Disabled Read and Write to directory iNodes.
 *
 * 03-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Implemented support for FileSystem.i3 directory operations, Ls works but files
 *	don't yet return stat info.
 *
 * 01-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Re-wrote sync to respond to file system command.
 *
 * 30-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	Gave LFS concept of directories.  Re-Wrote open to use these features.
 *      Deleted a bunch of chaff.
 *
 * 05-Jun-96  Tim Bradley (tbradley) at the University of Washington
 *	Added sync command to write fs state to disk.
 *
 * 03-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *      Read written, tested, works.
 *
 * 02-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *      Writes tested, they work.  Truncation added to write, modified open to
 *      support it.
 *
 * 31-May-96  Eric Christoffersen (ericc) at the University of Washington
 *      Added single block read.  Still untested.
 *
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *      Wrote Write Command.  Currently ignores the 'from' parameter.  
 *      Currently ignores exceptions.  Might currently suck.
 *
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *      Created, open written.
 *)


MODULE LFS EXPORTS LFS, LFSRep;

IMPORT File, FileStat, Directory, FileId;
IMPORT LFSInterface,Auth,NameServer;
IMPORT IO, FileSystem, Error,Fmt,Text,Word;
IMPORT DiskExtents;
IMPORT SegBuffer,Segment;
IMPORT IMap,INode,LFSTypes;
IMPORT INodeInfo;
IMPORT Ctypes;
IMPORT FastBuffer;
IMPORT FSRoot;
IMPORT NSName;

FROM Segment IMPORT DiskAddress;
FROM Segment IMPORT SegSummaryEntry;
FROM LFSTypes IMPORT ShortCard;
IMPORT DirEnt;
IMPORT Cleaner;
IMPORT INodeInfoArray;
FROM INodeInfoArray IMPORT NewINodeInfoArray;
IMPORT LFSSuperBlock, LFSLock;
IMPORT Debugger;

(* CONST DEBUG = FALSE; *)
VAR DEBUG := FALSE;

(* ------------------- Error Msg ------------------- *)
PROCEDURE Raise (msg : TEXT) RAISES {Error.E} =
  VAR
    err := NEW(LFSTypes.LFSErrorT);
  BEGIN
    err.messageE := msg;
    RAISE Error.E(err);
  END Raise;

(* ------------------- FILEID SUPPORT ------------------- *)

CONST NULLCHAR = VAL(16_00,CHAR);
VAR FileIdStart := FileId.T{   
       'l',      'f',      's', NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR};

(* ---------------------- VNODE SUPPORT -------------------------*)
TYPE VnodeT = RECORD
  mu   : MUTEX;
  fp   : T;  (* defined in LFSRep.i3 *)
END;

(* ------------------- DIRECTORY SERVICES ------------------- *)

(* The "DirectoryT" defines what a directory looks like for this
   filesystem.  A DirectoryT is a subtype of fscore's Directory.T *)

REVEAL DirectoryT = Directory.T BRANDED OBJECT
    vnode : VnodeT;
  METHODS
    init(id: FileId.T): DirectoryT := methodDirectoryInit;
    close():= methodDirectoryClose;
  OVERRIDES
    create  := methodDirectoryCreate;
    lookup  := methodDirectoryLookup;
    attach  := methodDirectoryAttach;
    detach  := methodDirectoryDetach;
    mkfile  := methodDirectoryMkfile;
    getEntries:= methodDirectoryGetEntries;
  END;


(* note that someone else needs to set the mp ref! *)
(* initializes a directory object *)
PROCEDURE methodDirectoryInit(
    self : DirectoryT; 
    id  : FileId.T): DirectoryT =
  BEGIN
    self.id := id;
    self.vnode.mu := NEW(MUTEX);
    RETURN Directory.T.init(self);
  END methodDirectoryInit;


(* creates an initialized directory object and attaches it *)
(* XXX.  need to see if lock works! *)
PROCEDURE methodDirectoryCreate(
    self            : DirectoryT; 
    name            : NameServer.Name;
    ) : NameServer.T = 
  BEGIN
    TRY 
      self.vnode.fp.mp.lock.lock();
      RETURN methodDirectoryCreateI(self, name);
    FINALLY
      self.vnode.fp.mp.lock.unlock();
    END;
  END methodDirectoryCreate;

PROCEDURE methodDirectoryCreateI(
    self            : DirectoryT; 
    name            : NameServer.Name;
    ) : NameServer.T = 
  VAR dir: DirectoryT;
      temp:ROOT;
  BEGIN

    dir := NEW(DirectoryT);

    dir := dir.init(FileId.Inc(FileIdStart));
    dir.vnode.fp := NewT(self.vnode.fp.mp);
    TRY 
      temp:= dir.vnode.fp.mp.imap.getNewINode();
    EXCEPT
    | IMap.OutOfINums =>
      IO.PutError("LFS.methodDirectoryCreate.  OutOfINums\n");
      RETURN NIL;
    END;

    dir.vnode.fp.inode := NARROW(temp,INode.T);
    TRY
      dir.vnode.fp.inode.setFileType(LFSTypes.DIR);
    EXCEPT
    | INode.IllegalFileType =>
      IO.PutError("LFS.methodDirectoryCreate. IllegalFileType\n");
      RETURN NIL;
    END;
    TRY
      self.attach(name, dir);
    EXCEPT
    | NameServer.Error =>
      IO.PutError("LFS.methodDirectoryCreate.  NameServerError\n");
      RETURN NIL;
    END;
    dir.close();
    RETURN dir;
  END methodDirectoryCreateI;

(* XXX.  need to see if lock works! *)
PROCEDURE methodDirectoryLookup(
    self          : DirectoryT;
    VAR name      : NameServer.Name;
    getalias      : BOOLEAN): REFANY 
  RAISES {NameServer.Error} = 
  BEGIN
    TRY 
      self.vnode.fp.mp.lock.lock();
      RETURN methodDirectoryLookupI(self, name, getalias);
    FINALLY
      self.vnode.fp.mp.lock.unlock();
    END;
  END methodDirectoryLookup;

PROCEDURE methodDirectoryLookupI(
    self          : DirectoryT;
    VAR name      : NameServer.Name;
    getalias      : BOOLEAN): REFANY 
  RAISES {NameServer.Error} = 
  VAR
    tmp, component : NameServer.Name;
    entry      : REFANY;
    fileINum   : ShortCard;
    tempINode  : INode.T;
    newDir :DirectoryT;
    newFile:FileT;
  BEGIN

    IF DEBUG THEN
      IO.Put("LFS.lookup called. name:"& NSName.ToText(name)&".\n");
    END;

    (* extract a component from the path *)
    NameServer.GetComponent(name, component);
    tmp := component;
    
    TRY
      entry := Directory.T.lookup(self, tmp, getalias);
    EXCEPT
    | NameServer.Error(ec) =>
      IF ec = NameServer.EC.NameNotFound THEN
	(* check if we have it on disk *)
	IF DEBUG THEN IO.Put("LFS.Lookup found a new file.\n"); END;

	(* if the file doesn't exist, raise an exception indicating so *)
	TRY
	  fileINum := self.vnode.fp.inode.getFileINum(NSName.ToText(component));
	EXCEPT
	| INode.FileNotFound, INode.IllegalFileName, INode.NotDirectory  =>
	  RAISE NameServer.Error(NameServer.EC.NameNotFound);
	END;
	(* otherwise attach this new file to the nameserver *)
	TRY
	  tempINode:=NARROW(self.vnode.fp.mp.imap.getINode(fileINum),INode.T);
        EXCEPT
        | IMap.InvalidINum =>
          IO.PutError("LFS.methodDirectoryLookup. got Invalid Inum\n");
	  RAISE NameServer.Error(NameServer.EC.NameNotFound);
        END;
	IF tempINode.isDir()=TRUE THEN
	  newDir:=NEW(DirectoryT).init(FileId.Inc(FileIdStart));
	  newDir.vnode.fp := NewT(self.vnode.fp.mp);
	  newDir.vnode.fp.inode:= tempINode;
	  Directory.T.attach(self, component, newDir);
	  (* now add . and .. to new directory *)
	  VAR tmp: NSName.T;
	  BEGIN
	    tmp :=  NSName.FromText("..");
	    Directory.T.attach(newDir, tmp, self);
	    tmp := NSName.FromText(".");
	    Directory.T.attach(newDir, tmp, newDir);
	  END;
	  entry := newDir;	(* set entry to point to retrieved dir *)
	ELSE
	  newFile:=NEW(FileT).init();
	  newFile.vnode.fp := NewT(self.vnode.fp.mp);
	  newFile.vnode.fp.inode:= tempINode;
	  Directory.T.attach(self, component, newFile);
	  entry := newFile;	(* set entry to point to retrieved file *)
	END;
      ELSE
	RAISE NameServer.Error(ec);
      END;
    END;
    RETURN entry;
  END methodDirectoryLookupI;

(* adds a name and object to a directory *)
(* XXX.  need to see if lock works! *)
PROCEDURE methodDirectoryAttach(
    self : DirectoryT;
    READONLY name : NameServer.Name;
    obj  : REFANY;
  ) =
  BEGIN
    TRY 
      self.vnode.fp.mp.lock.lock();
      methodDirectoryAttachI(self, name, obj);
    FINALLY
      self.vnode.fp.mp.lock.unlock();
    END;
  END methodDirectoryAttach;


PROCEDURE methodDirectoryAttachI(
    self : DirectoryT;
    READONLY name : NameServer.Name;
    obj  : REFANY;
  ) =
  VAR
    dirObject : LFSTypes.DirectoryData;
    nil: BITS 8 FOR [0..255];
  BEGIN

    nil:= 0;

    IF DEBUG THEN
      IO.Put("LFS.methodDirectoryAttach called with name:"&
	     NSName.ToText(name) &".\n");
    END;
    (* self is a directoryT, assumed to 
       and the inode is a directory, this is the parent*)

    (* this method must add the name to the
       directory.  In this case, obj must be an LFSTypes.DirectoryData,
       well set the name in this object, just to make sure.*)
    TYPECASE obj OF
    | FileT=>
      WITH fp = NARROW(obj,FileT),
	   textName = NSName.ToText(name) DO
        TRY
          dirObject.iNum :=fp.vnode.fp.inode.getINum();
	  Text.SetChars(dirObject.name, textName);

          (* gotta add some of that null termination stuff... *)
          dirObject.name[Text.Length(textName)]:=VIEW(nil,CHAR);

	  IF DEBUG THEN
            IO.Put("DirAttach with FileT called with name:"& textName &".\n");
	  END;
	  TRY
            self.vnode.fp.inode.addDirEntry(dirObject);
          EXCEPT
          | INode.FileError =>
            IO.PutError("LFS.methodDirectoryAttach. Inode FileError\n");
          END;
          Directory.T.attach(self,name,obj);
        EXCEPT
        | INode.FileExists =>
        | INode.IllegalFileName =>
        | INode.NotDirectory =>
          IO.Put("Directory error in LFS.DirectoryAttach, operation not performed.\n");
        END;
      END;
      RETURN;
    | DirectoryT=>
      WITH fp = NARROW(obj,DirectoryT) DO
        TRY
          dirObject.iNum :=fp.vnode.fp.inode.getINum();
          Text.SetChars(dirObject.name, NSName.ToText(name));

          (* gotta add some of that null termination stuff... *)
          dirObject.name[Text.Length(NSName.ToText(name))]:=VIEW(nil,CHAR);
	  IF DEBUG THEN
            IO.Put("DirAttach with DirT called with name:"&
		   NSName.ToText(name) & ".\n");
	  END;

          self.vnode.fp.inode.addDirEntry(dirObject);
          Directory.T.attach(self,name,obj);
          
          (* now add . and .. to new directory *)
	  VAR tmp: NSName.T;
	  BEGIN
	    tmp := NSName.FromText("..");
	    Directory.T.attach(fp, tmp, self);
	    tmp := NSName.FromText(".");
	    Directory.T.attach(fp, tmp, fp);
	  END;
        EXCEPT
        | INode.FileExists =>
        | INode.IllegalFileName =>
        | INode.NotDirectory =>
          IO.Put("Directory error in LFS.DirectoryAttach, operation not performed.\n");
        END;
      END;
      RETURN;
    ELSE
      IO.Put("Error in DirectoryAttach: unknown filetype to attach.\n");
    END;

  END methodDirectoryAttachI;


(* unlinks a file/directory from a directory *)
(* file is destroyed if link count is decremented to 0 *)
(* XXX.  need to see if lock works! *)
PROCEDURE methodDirectoryDetach(self: DirectoryT;
				READONLY victimName: NameServer.Name) =
  BEGIN
    TRY 
      self.vnode.fp.mp.lock.lock();
      methodDirectoryDetachI(self, victimName);
    FINALLY
      self.vnode.fp.mp.lock.unlock();
    END;
  END methodDirectoryDetach;

PROCEDURE methodDirectoryDetachI(self: DirectoryT;
				READONLY victimName: NameServer.Name) =
  VAR victimINum: ShortCard;
  BEGIN

    victimINum  := self.vnode.fp.inode.getFileINum(NSName.ToText(victimName));
    
    IF DEBUG THEN
      IO.Put("In delete, victimINum is set to:"&Fmt.Int(victimINum)&".\n");
    END;
    
    TRY
      self.vnode.fp.inode.rmDirEntry(NSName.ToText(victimName), victimINum);
    EXCEPT
    | INode.NonEmpty =>
      IO.PutError(NSName.ToText(victimName) & ": dir Not empty.");
      self.vnode.fp.close();
      RETURN;
    | INode.FileNotFound, INode.IllegalFileName =>
      IO.PutError(NSName.ToText(victimName) & ": Not found.");
      self.vnode.fp.close();
      RETURN;
    | INode.NotDirectory =>
      IO.PutError(NSName.ToText(victimName) & ": Not Directory\n");
      self.vnode.fp.close();
      RETURN;
    END;

    self.vnode.fp.close();

    (* tell the nameserver that the file is gone *)
    Directory.T.detach(self, victimName);
  END methodDirectoryDetachI;

(* makes and attaches a file to a directory *)
(* XXX.  need to see if lock works! *)
PROCEDURE methodDirectoryMkfile (self: DirectoryT;
				 name: NameServer.Name): File.T =
  BEGIN
    TRY 
      self.vnode.fp.mp.lock.lock();
      RETURN methodDirectoryMkfileI(self, name);
    FINALLY
      self.vnode.fp.mp.lock.unlock();
    END;
  END methodDirectoryMkfile;

PROCEDURE methodDirectoryMkfileI(self: DirectoryT;
				 name: NameServer.Name): File.T =
  VAR fp : FileT;
  BEGIN

    IF DEBUG THEN IO.Put("LFS.DirectoryMkfile called.\n"); END;

    (* create a file *)
    fp := NEW(FileT).init();
    fp.id := FileId.Inc(FileIdStart);
    fp.vnode.fp := NewT(self.vnode.fp.mp);
    fp.vnode.fp.inode := NARROW(self.vnode.fp.mp.imap.getNewINode(),INode.T);

    (* create the LFSTypes.Directory object to store in the directory *)
    self.attach(name, fp);

    fp.close();
    RETURN fp;
  END methodDirectoryMkfileI;

(* returns an iterator containing the file list of the given directory *)
(* works by instantiating all the file objects in the directory, attaching them,
   then returning an iterator on self *)
(* XXX.  need to see if lock works! *)
PROCEDURE methodDirectoryGetEntries(
    self: DirectoryT;
    offset: Word.T;
    VAR ent:ARRAY OF NameServer.Entry
    ) : CARDINAL = 
  BEGIN
    TRY 
      self.vnode.fp.mp.lock.lock();
      RETURN methodDirectoryGetEntriesI(self, offset, ent);
    FINALLY
      self.vnode.fp.mp.lock.unlock();
    END;
  END methodDirectoryGetEntries;

PROCEDURE methodDirectoryGetEntriesI(
    self: DirectoryT;
    offset: Word.T;
    VAR ent:ARRAY OF NameServer.Entry
    ) : CARDINAL = 
  VAR
    array: REF ARRAY OF DirEnt.T;
    numFiles: CARDINAL;
    name: TEXT;
    index: CARDINAL;
  BEGIN

LOCK self.vnode.fp.inode.lock DO
    numFiles:=self.vnode.fp.inode.dirSize();

    IF DEBUG THEN
      IO.Put("In lfs.GetEntries:Directory has "&Fmt.Int(numFiles)&" file entries.\n");
      IO.Put("In lfs.GetEntries:offset is:"&Fmt.Int(offset)&".\n");
    END;

    IF offset>=numFiles THEN
      RETURN 0;
    END;

    index:=0;

    array:=NEW(REF ARRAY OF DirEnt.T,numFiles);
    numFiles:=self.vnode.fp.inode.getFileArray(0,array^);
END;

    FOR i:=offset TO numFiles-1 DO
      name:= array[i].name;
      (*self.getComponent(name,component);*)

      ent[index].name:= NSName.FromText(name);

      IF DEBUG THEN IO.Put("added "&name&" to dirlist.\n"); END;

      ent[index].cookie:=i+1;					(* huh? *)
      VIEW(ent[index].id,Ctypes.unsigned_int) := array[i].ino;	(* huh? *)
      INC(index);
      IF index>LAST(ent) THEN EXIT; END;
    END;  (* end loop *)

    RETURN index;

  END methodDirectoryGetEntriesI;

(* XXX.  need to see if lock works! *)
PROCEDURE methodDirectoryClose(self:DirectoryT) =
  BEGIN
    TRY 
      self.vnode.fp.mp.lock.lock();
      methodDirectoryCloseI(self);
    FINALLY
      self.vnode.fp.mp.lock.unlock();
    END;
  END methodDirectoryClose;

PROCEDURE methodDirectoryCloseI(self:DirectoryT) =
  BEGIN
      self.vnode.fp.close();
  END methodDirectoryCloseI;

(* ------------------- FILE SERVICES ------------------- *)

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

REVEAL FileT = FileTPublic BRANDED OBJECT
    vnode : VnodeT;
    mode  : INTEGER;
  METHODS
    init() : FileT := methodFileInit;
  OVERRIDES
    getMP    := getMP;
    root     := Root;
    read     := methodFileRead;
    write    := methodFileWrite;
    close    := methodFileClose;
    open     := methodFileOpen;
    stat     := methodFileStat;
  END;

PROCEDURE Root (self: FileT): FSRoot.T =
  BEGIN
    RETURN NEW(LfsRoot, mp := self.vnode.fp.mp);
  END Root;
  
(* this needs to be exposed to allow the shell access to the LFS.MP object, in order
   to expose various filesystem numbers, as well as allow access to the cleaner *)
PROCEDURE getMP(self:FileT):MP=
  BEGIN
    RETURN self.vnode.fp.mp;
  END getMP;

(* note that someone else needs to set the mp ref in a fileT! *)
(* and also init the inode from the imap *)
PROCEDURE methodFileInit(self: FileT): FileT = 
  BEGIN

    IF DEBUG THEN IO.Put("LFS.FileInit called.\n"); END;

    self.vnode.mu := NEW(MUTEX);
    RETURN self;
  END methodFileInit;

PROCEDURE methodFileRead(self     : FileT;
			 VAR data : ARRAY OF CHAR;
			 offset   : File.OffsetT): CARDINAL RAISES {Error.E}= 
  VAR 
    bytes : CARDINAL;
  BEGIN

  TRY
    self.vnode.fp.mp.lock.lock();
    LOCK self.vnode.fp.inode.lock DO
      bytes := self.vnode.fp.read(offset,data);
    END;
  FINALLY
    self.vnode.fp.mp.lock.unlock();
  END;

  RETURN bytes;
  END methodFileRead;

PROCEDURE methodFileReadRef (
    self      : FileT;
    offset    : File.OffsetT;
    bytes : CARDINAL;  (* how many to read; how many got *)
    VAR data  : REF ARRAY OF CHAR;
    VAR from  : CARDINAL): CARDINAL
  RAISES {Error.E} =
  BEGIN

    IF bytes = 0 THEN RETURN 0; END;

    IF data = NIL THEN
      data := NEW(REF ARRAY OF CHAR, bytes);
    ELSE
      bytes := MIN(bytes, LAST(data^)-from+1);
    END;

  TRY
    self.vnode.fp.mp.lock.lock();
    LOCK self.vnode.fp.inode.lock DO
      bytes := self.vnode.fp.read(offset,SUBARRAY(data^,from,bytes));
    END;
  FINALLY
    self.vnode.fp.mp.lock.unlock();
  END;

  RETURN bytes;
  END methodFileReadRef; 

PROCEDURE methodFileWrite (
    self          : FileT;
    READONLY data : ARRAY OF CHAR;
    offset        : File.OffsetT):CARDINAL
  RAISES {Error.E} <*NOWARN*> =
  VAR
    bytes : CARDINAL;
  BEGIN

  TRY
    self.vnode.fp.mp.lock.lock();
    LOCK self.vnode.fp.inode.lock DO
      bytes := self.vnode.fp.write(offset,data);
    END;
  FINALLY
    self.vnode.fp.mp.lock.unlock();
  END;

  RETURN bytes;
  END methodFileWrite;

PROCEDURE methodFileWriteRef(
    self      : FileT; 
    recnum    : File.OffsetT; 
    bytes : CARDINAL;
    data      : REF ARRAY OF CHAR;
    from      : CARDINAL) :CARDINAL
  RAISES {Error.E} <*NOWARN*> =
  VAR
  BEGIN

  TRY
    self.vnode.fp.mp.lock.lock();
    LOCK self.vnode.fp.inode.lock DO
      bytes := self.vnode.fp.write(recnum, SUBARRAY(data^,from,bytes));    
    END;
  FINALLY
    self.vnode.fp.mp.lock.unlock();
  END;

  RETURN bytes;
  END methodFileWriteRef;

(* given a fileT returns a file.T? *)
PROCEDURE methodFileOpen (self : FileT; <*UNUSED*>mode : INTEGER): File.T
  RAISES { Error.E } <*NOWARN*> =
  BEGIN
    RETURN self;
  END methodFileOpen;

(* flush dirty inode info to segbuffer *)
PROCEDURE methodFileClose(self: FileT) RAISES {Error.E} =
  BEGIN
  TRY
    self.vnode.fp.mp.lock.lock();
    LOCK self.vnode.fp.inode.lock DO
      self.vnode.fp.close();
    END;
  FINALLY
    self.vnode.fp.mp.lock.unlock();
  END;
  END methodFileClose;

PROCEDURE methodFileStat (self: FileT; VAR s : FileStat.T) =
  PROCEDURE StatI() =
    BEGIN
      s.ino := self.vnode.fp.inode.getINum();
      s.size:= self.vnode.fp.inode.byteSize();
      s.atime:= 0;
      s.mtime:= 0;
      s.blksize:=self.vnode.fp.mp.segInfo.bytesInBlock;

      IF s.blksize # 0 AND (s.size+s.blksize-1) > 0 THEN
        s.blocks:= (s.size+s.blksize-1) DIV s.blksize;
      ELSE
        s.blocks:= 0;
      END;

      s.gen := self.vnode.fp.inode.getVersion();

      (*
        s.dev := 0;
        s.mode:= 0;
        s.nlink:=1;
        s.uid := 0;
        s.gid := 0;
        s.rdev:= 0;
        s.space1:=0;
        s.space2:=0;
        s.ctime :=0;
        s.space3:=0;
      *)
    END StatI;

  BEGIN
    TRY
      self.vnode.fp.mp.lock.lock();
      StatI();
    FINALLY
      self.vnode.fp.mp.lock.unlock();
    END;
  END methodFileStat;

(* ------------------- MOUNT SERVICES ------------------- *)

(* The type "MountT" represents a mount point exported by this file
   system. A MountT is a subtype of a DirectoryT, a directoryT has
   an lfs mount point in it, convention is that the mountT holds the
   original lfs mount point, which all directory and file objects then
   are given. *)
TYPE MountT = DirectoryT OBJECT
  (* XXX not used
   * lock     : LFSLock.T;                (* Should be a ReaderWriter lock *)
   *)
    dirPath  : TEXT;                      (* path name to mount point      *)
END;

(* The type "FileSystemT" is used to register the filesystem with
   fscore.  A FileSystemT is a subtype of fscore's FileSystem.T. *)

REVEAL FileSystemT = FileSystem.T BRANDED OBJECT
  mp:MP;
OVERRIDES
  newfs := Newfs;
END;

TYPE LfsRoot = FSRoot.T OBJECT
  mp: MP;
OVERRIDES
  sync := Sync;
END;
  
(* creates and attaches new file system *)
(* causes lfs to mount to disk *)
PROCEDURE Newfs (self : FileSystemT; 
		 READONLY name : NameServer.Name): Directory.T RAISES{Error.E} =
  VAR mp: MountT;
    tmp: NSName.T;
  BEGIN
    IF DEBUG THEN
      IO.Put("LFS.methodFileSystemCreate called.\n");
    END;
    mp := NEW(MountT).init(FileId.Inc(FileIdStart));
    mp.vnode.fp := NewT(Mount(NSName.ToText(name)));

    self.mp := mp.vnode.fp.mp;	(* yes.  registre myself to NS *)
    mp.vnode.fp.inode := NARROW(mp.vnode.fp.mp.imap.getINode(0),INode.T);
    tmp := NSName.FromText("..");
    Directory.T.attach(mp, tmp, mp); (* XXX doesn't work anyway *)
    tmp := NSName.FromText(".");
    Directory.T.attach(mp, tmp, mp);
    RETURN mp;
  END Newfs;

(* ------------------- LOW LEVEL FS SERVICES ------------------- *)


(* ------------------ FileSystem Registration ------------------- *)

PROCEDURE Init(<*UNUSED*>verbose:BOOLEAN) =
  BEGIN

    TRY
      WITH new = NEW(FileSystemT, mp := NIL) DO
        IO.Put("Registering LFS.\n");
        FileSystem.Register("lfs", new);
      END;
      IO.Put("log file system installed\n");
    EXCEPT
    | Error.E(e) =>
      IO.Put("ERROR!");
      IO.PutError(e.message()&" during LogFileSystem initialization.\n");
    END;
  END Init;


(* *********** END OF NAMESERVER STUFF ******************* *)

(* this is the LFSRep.T.   the LFS file pointer object *)
REVEAL T = Public BRANDED OBJECT
OVERRIDES
  read:=Read;
  write:=Write;
  close:=Close;
  getDirEntries:=GetDirEntries;
  stat:=Stat;
END;

(** Mount stuff**)
(* Mounts file system onto a mountpoint*)
PROCEDURE Mount (READONLY devName: TEXT) : MP RAISES{Error.E}=
  VAR
    mp := NEW(MP);
  BEGIN

    mp.segInfo := NEW(Segment.T);
    mp.cleaner := NEW(Cleaner.T);
    mp.lock := NEW(LFSLock.T).init();		(* for synchronize *)

    TRY
      DiskExtents.SetupDiskExtent(mp, devName);
    EXCEPT
      DiskExtents.DiskNotFound => IO.Put("Disk not found\n");
      Raise("device " & devName & "not found");
    END;

    ReadSuperBlock(mp);

    IO.Put("Mount launching cleaner.\n");
    mp.cleaner := mp.cleaner.initialize(mp.segInfo,
					mp.segBuff,
					mp.imap,
					mp.lock);
    mp.cleaner.start();		(* starts cleaner thread *)
    RETURN mp;
  END Mount;

  (************************************************************************)
  (* not yet implemented *)
  (* Doesnt need to do shit *)
  (* we're gunna use sync instead *)
<* UNUSED *>
PROCEDURE Unmount(mp : MP)  =
  BEGIN
    IO.Put("LFS.Unmount was called.\n");
    mp:=NIL;
  END Unmount;
  (************************************************************************)

(*
  moves the segment buffer to contain the last written segment before shutdown,
  updates iMap to represent recent changes.

  writeLocation argument is the current disk writelocation of the segBuffer, ie:
  the last written block that is known to be valid.

   First we get the segment Summary from the current segment
   if this is longer than the blocks we've written, then
   there is further data in the current segment that hasn't been
   incorporated into the filesystem yet.  If the segment descriptor
   describes any new blocks, we move the write pointer forward.

   We scan through the new section of the segment descriptor, if we 
   find any first inode blocks, we tell the imap where the inode is.
   Continue until end of segment descriptor is reached.  Now look at 
   where next segDescA starts, if it starts in this segment then we 
   are done, if it starts at an illegal segment then we also know we 
   are done.
   Otherwise we load the segBuffer with the next indicated segment
   and repeat.

   The is currently a fatal flaw, not enough information is stored with
   inodes to allow correct roll-forward in all cases.  The case is this:

     Someone writes a big file to disk, summary blocks are written to
     enclose all completely written segments.  The file is written
     fine, so is its inode, then we start to write out the updated
     directory data, but lo- we get to the end of the segment before the
     directories final inode block can be written, we write the segment
     out to disk, then write out its segment descriptor block, and while
     searching for the next segment we crash.
     The directory object cannot be recovered from disk, so we don't have an 
     updated directory object to tell us the inode of the file we just
     wrote, so our filesystem has no way to refer to the file we just
     recovered.
     We need a way to recover these unlucky new files by recording the
     inum of the directory file they are to be inserted into.
*)
(* XXX FIXME.  Need to use LFSLock here *)
<* UNUSED *>
PROCEDURE RollForward(mp:MP;freeList: REF ARRAY OF CHAR;initialOffset:CARDINAL)=
  VAR
    end:BOOLEAN := FALSE;
    entries, writePtr,oldWritePtr:CARDINAL;
    nextSegDescALoc, currentAddress:DiskAddress;
    segDescEntries:REF ARRAY OF CHAR;
    epoch, nextEpoch:INTEGER;
    firstLoop:BOOLEAN:=TRUE;
  BEGIN

    segDescEntries := 
        NEW(REF ARRAY OF CHAR,
            mp.segInfo.blocksInSegment* BYTESIZE(SegSummaryEntry));

    nextEpoch := mp.segBuff.getEpoch();

    IO.Put("In rollForward, initialEpoch is:"&Fmt.Int(nextEpoch)&".\n");

    currentAddress.segment := mp.segBuff.getCurrentSegment();
    currentAddress.block   := mp.segBuff.getOffset();
    writePtr := currentAddress.block;

    nextSegDescALoc := currentAddress;

    WHILE NOT end DO

      IF DEBUG THEN
        IO.Put("recovery attempt on segment:"&Fmt.Int(nextSegDescALoc.segment)&".\n");
      END;

      currentAddress.segment := nextSegDescALoc.segment;

      (* get segment summary for current segment *)
      TRY
        entries := SegBuffer.GetSegmentSummary(mp.segInfo,
                                               currentAddress.segment,
                                               segDescEntries,
                                               nextSegDescALoc,
                                               epoch);

        IO.Put("RollForward got segment descriptor with "&Fmt.Int(entries)&" entries.\n");
        IO.Put("CurrentAddress.block:"&Fmt.Int(currentAddress.block)&".\n");
        IO.Put("Latest recovered epoch is:"&Fmt.Int(epoch)&".\n");

      EXCEPT
      | Segment.SegmentOutOfBounds =>
        IO.Put("Error in roll-forward operations, attempt "&
          "to read out of bounds segment.\n");
        IO.Put("segment:"&Fmt.Int(currentAddress.segment)&"\n");
        IO.Put("Aborting RollForward.\n");
        RETURN;
      END;    

      IF entries>currentAddress.block OR firstLoop # TRUE THEN

        IF firstLoop = TRUE THEN
          nextEpoch := epoch;
          firstLoop := FALSE;
        END;

        (* epoch must be valid except on first iteration *)
        (* there also must be valid data entries past the current write pointer *)
        IF epoch # nextEpoch OR entries <= currentAddress.block THEN
          IO.Put("NOT roll further roll-forward permitted.\n");
          end:= TRUE;
        ELSE
          IO.Put("Roll Forward, next epoch matched, continuing roll forward.\n");
          oldWritePtr := currentAddress.block;

          (* scan through the new entries*)
          FOR i:= oldWritePtr TO entries DO

            WITH segDesc = VIEW(SUBARRAY(segDescEntries^,
                                         i*BYTESIZE(SegSummaryEntry),
                                         BYTESIZE(SegSummaryEntry)),
                                SegSummaryEntry) DO

              (* if any are main inode blocks then update the imap with them *)
              IF segDesc.flag = LFSTypes.META AND segDesc.offset = LFSTypes.INODEOFFSET THEN
                currentAddress.block:=i;
                EVAL mp.imap.setLocation(segDesc.iNode, currentAddress);
                IF DEBUG THEN
                  IO.Put("Found inode with iNum:"&Fmt.Int(segDesc.iNode)&" at block:"&Fmt.Int(i)&".\n");
                END;
              END;

            END;      (* end WITH  *)
          END;        (* end FOR   *)

          (* Note that in any case we must roll the segBuffer forward and modify the freelist.
             If there are no recovered iNodes in these segments, the cleaner will get them
             back later.*)
          mp.segBuff.setCurrentSegment(nextSegDescALoc.segment);
          mp.segBuff.setOffset(nextSegDescALoc.block+1);



          (* if the next segDescA is in the current segment, then we know it is
             invalid and our loop ends *)

          (****************)
          (* POSSIBLE BUG *)
          (****************)
          (* not sure if I mean < or <= *)
          IF nextSegDescALoc.segment # currentAddress.segment AND nextSegDescALoc.segment < mp.segInfo.segmentsOnDisk THEN

            (* declare this segment as not free *)
            freeList[initialOffset+currentAddress.segment]:= Segment.DIRTY;

            currentAddress := nextSegDescALoc;
            nextEpoch := epoch+1;

            IO.Put("reached end of rollForward loop, nextEpoch:"&Fmt.Int(nextEpoch)&".\n"); 
          ELSE
            end:=TRUE;
          END;
        END;

      ELSE
        end:= TRUE;
      END;
    END; (* end WHILE *)
  END RollForward;

PROCEDURE PrintDiskAddress(READONLY msg: TEXT; READONLY loc: DiskAddress) =
  BEGIN
    IO.Put(msg);
    IO.Put(" s: " & Fmt.Int(loc.segment) & ", b: " & Fmt.Int(loc.block));
    IO.Put(".\n");
  END PrintDiskAddress;

PROCEDURE ReadSuperBlock(mp : MP) RAISES{Error.E} =
  VAR 
    header : LFSSuperBlock.LFSSBHeader;
    theBuffer : REF ARRAY OF CHAR;
    temproot  : INode.T;
    writeLoc  : DiskAddress;
  BEGIN

    LFSSuperBlock.ReadSBHeader(mp, header);
    (* sanity check for the header *)
    IF LFSSuperBlock.CheckSBHeader(header, mp) THEN
      Raise("device is not LFS filesystem.  Need to format it");
    END;
    
    (* set constant data from the header *)
    mp.segInfo.firstSegmentOffset := header.size;
    mp.segInfo.blocksInSegment := header.blocksInSegment;
    writeLoc := header.writeLoc;	(* segBuff write location *)

    mp.segInfo.segmentsOnDisk :=
	LFSSuperBlock.GetNumberOfSegments(mp.blocksInExtent,
					  mp.segInfo.bytesInBlock,
					  mp.segInfo.blocksInSegment);

    IO.Put("LFS: SuperBlock Info read from device\n");
    IO.Put("    first segment at " & Fmt.Int(header.size) & " blocks\n");
    IO.Put("    " & Fmt.Int(mp.segInfo.bytesInBlock) & " bytes/block\n");
    IO.Put("    " & Fmt.Int(mp.segInfo.blocksInSegment) & " blocks/segment\n");
    IO.Put("    " & Fmt.Int(mp.segInfo.segmentsOnDisk) & " segments/device\n");
    WITH unusedBlocks = mp.blocksInExtent
                        - mp.segInfo.segmentsOnDisk * mp.segInfo.blocksInSegment
		        - mp.segInfo.firstSegmentOffset DO
      IO.Put("    " & Fmt.Int(unusedBlocks) & " blocks unused\n");
    END;
    IO.Put("    " & Fmt.Int(mp.blocksInExtent) & " blocks/device\n");
    PrintDiskAddress("    write location", writeLoc);

    mp.segInfo.blocksForSummary :=
      LFSSuperBlock.RoundUp(mp.segInfo.blocksInSegment *
				BYTESIZE(SegSummaryEntry),
                            mp.segInfo.bytesInBlock);


    (* init segment buffer *)
    TRY
      mp.segBuff := NEW(SegBuffer.Buffer).init(mp.segInfo,writeLoc);
    EXCEPT
    | Segment.NoFreeSegs =>
      Raise("No free segments left on disk!");
    END;

    (* get a new iMap and init it *)
    LFSSuperBlock.ReadSBIMap(mp, theBuffer); 
    mp.imap := IMap.NewIMap(mp.segInfo, mp.segBuff, theBuffer);

    (* at this point we <*could*> attempt to roll-forward *)
    (*RollForward(mp,theBuffer,BYTESIZE(LFSSuperBlock.LFSSBHeader);*)

    (* initialize the freelist with possibly modified buffer *)    
    LFSSuperBlock.ReadSBFreeList(mp, theBuffer); 
    mp.segInfo.segFreeList := NEW(Segment.FreeList).init(theBuffer);

    WITH rootLoc = mp.imap.getLocation(0) DO
      PrintDiskAddress("    Root iNode is at ", rootLoc);
    END;

    (* here we allocate the root inode (number 0) *)
    TRY
      temproot := NARROW(mp.imap.getINode(0),INode.T);
      IO.Put("    Root INode found, no problem.\n");
    EXCEPT
    | IMap.InvalidINum =>
      IO.Put("    Root inode not found, making a new one.\n");
      temproot := NARROW(mp.imap.getNewINode(),INode.T);
      temproot.setbyteSize(0);
    END;


    IF temproot.getINum() # 0 THEN
      IO.Put("ERROR!! ROOT INODE NOT FOUND, EQUALS " & 
        Fmt.Int(temproot.getINum()) & ".\n");
    END;

    (* Total temp hack, if the root iNode isn't a directory, then make it one. *)
    IF temproot.isDir() = FALSE THEN
      temproot.setFileType(LFSTypes.DIR);
      temproot.flush();
    END;
    
    (*NOTE we can just drop the pointer to the root inode since
      mp.imap will hold onto it*)
    
  END ReadSuperBlock;

(* writes file system state to disk *)
PROCEDURE Sync(self: LfsRoot) RAISES {Error.E} =
  VAR
    mp := self.mp;
    imbuffer  : REF ARRAY OF CHAR;
    frbuffer  : REF ARRAY OF CHAR;
    bigbuf    : REF ARRAY OF CHAR;
    writeLoc  : DiskAddress;
    header    : LFSSuperBlock.LFSSBHeader;
  BEGIN
    IF DEBUG THEN IO.Put("LFS.methodFileSystemSync called.\n"); END;

    TRY
      self.mp.lock.lockwhole();	(* lock LFS. block any read/write request  *)
      IO.Put("LFS: Sync started.  File access to LFS is suspended\n");

      (* writes dirty segBuffer blocks to disk, with segDesc stuff *)
      (* changes segBuffer writePtr and currentSegment *)
      writeLoc:=mp.segBuff.flushToDisk();

      (* allocate and set the values for the buffers holding info *)
      imbuffer := mp.imap.flush();	(* get IMap buffer *)   

      mp.segInfo.segFreeList.flush(frbuffer);	(* get freelist *)

      (* now create a buffer that will be the concatentation of:
       * superblock header,  freelist and imap buffers.
       *)
      WITH hdrSize = BYTESIZE(LFSSuperBlock.LFSSBHeader),
	 flSize = NUMBER(frbuffer^),
	 imSize = NUMBER(imbuffer^) DO 
        bigbuf := NEW(REF ARRAY OF CHAR, hdrSize+flSize+imSize);

        header.size := mp.segInfo.firstSegmentOffset;
        header.blocksInSegment := mp.segInfo.blocksInSegment;
        header.writeLoc := writeLoc;

        (* store header *)
        VIEW(SUBARRAY(bigbuf^, 0, hdrSize), LFSSuperBlock.LFSSBHeader):= header;

        (* store freelist *)
        SUBARRAY(bigbuf^, hdrSize, flSize) := frbuffer^;

        (* store imap buffer *)
        SUBARRAY(bigbuf^, hdrSize+flSize, imSize) := imbuffer^;

        (* flush this buffer to disk as the new super block *)
        LFSSuperBlock.WriteSB(mp, bigbuf);
      END;
    FINALLY
      IO.Put("LFS: Sync ended.  File access to LFS is resumed\n");
      self.mp.lock.unlockwhole();
    END;

    IF DEBUG THEN IO.Put("Sync complete.\n"); END;

    (* discard buffers *)
    imbuffer := NIL;
    frbuffer := NIL;
    bigbuf := NIL;
  END Sync;

(*******************************************************)
(* File stuff *)
PROCEDURE NewT (mp : MP): T =
  VAR newfp: T;
  BEGIN
    newfp        := NEW(T);
    newfp.mu     := NEW(MUTEX);
    newfp.mp     := mp;
    RETURN newfp;
  END NewT;

(* ReadFromDiskOrBuffer: this is the file system read block function.
 * Should not rely on getCurrentSegment.  currentSegment may change.
 *)
PROCEDURE ReadFromDiskOrBuffer (
	diskloc : DiskAddress;
	mp : MP; VAR theblock : ARRAY OF CHAR) =
  BEGIN

    TRY 
      EVAL mp.segBuff.read(theblock, diskloc);
    EXCEPT
    | SegBuffer.BlockOutOfBounds => 
      IO.Put("Tried to read block " & Fmt.Int(diskloc.block) &
      " from the segment buffer and failed.\n");
    | Segment.OffsetOutOfBounds, Segment.SegmentOutOfBounds,
      Segment.BadnumBytesValue, Error.E =>
      IO.PutError("Segbuf.read: from segment.  can't read...\n");
    END; 
  END ReadFromDiskOrBuffer;
    
(* stupid single block read *)
PROCEDURE Read (fp         : T;
                offset     : File.OffsetT;
                VAR dest   : ARRAY OF CHAR):CARDINAL RAISES {Error.E} =
  VAR
    amountToRead      : CARDINAL;
    bytesInBlock      : CARDINAL;
    buffer	      : FastBuffer.T;
    blockLocation     : DiskAddress;
    currentFileOffset : CARDINAL;
    tempVar           : CARDINAL;
    amountWritten     : CARDINAL;
    nread             : CARDINAL;
    consecutiveBlocks : CARDINAL;	(* number of consecutive blocks *)
    curLocation       : DiskAddress;
    curOffset	      : CARDINAL;
    flag	      : BOOLEAN;
  BEGIN

    (* first check to see that the file isn't a directory, if it is then read fails *)
    IF fp.inode.isDir() = TRUE THEN
      Raise("Read attempted to directory iNode.");
    END;

    (* then check to see if this file was opened for write only, if yes then deny *)
    IF fp.flags = LFSTypes.OWRONLY THEN
      Raise("Read attempted on file opened for write only.");
    END;

    (**********************)
    (* init stuff *)
    bytesInBlock      := fp.mp.segInfo.bytesInBlock;
    nread             := BYTESIZE(dest);

    (* set the size of amountToRead *)
    WITH i = fp.inode.byteSize()-offset DO
      IF i < 0 THEN
        (* XXX.  better to do Raise("Attempt to read beginning past end of file"); ?*)
        RETURN 0
      END;
      amountToRead      := i;
    END;

    WITH i = MIN(amountToRead, nread) DO
      amountToRead := i;
      nread := i;
    END;

    amountWritten     := 0;
    currentFileOffset := offset;

    (*******************************************************)
    (* First, if first part of read isn't block aligned then
       read in that first part specially *)
    IF currentFileOffset MOD bytesInBlock # 0 THEN
      IF DEBUG THEN IO.Put("read reading in first unaligned block.\n"); END;

      TRY
        blockLocation := fp.inode.getBlockLoc(currentFileOffset DIV bytesInBlock);
      EXCEPT
      | INode.InvalidBlockRequest =>
        Raise("Read failed, iNode returned invalid block request");
      END;

      (* Now, we need a buffer *)
      buffer := FastBuffer.Allocate(bytesInBlock);
      ReadFromDiskOrBuffer(blockLocation, fp.mp, buffer.data^);

      (* tempVar holds where in this block we'll start    *)
      tempVar := currentFileOffset MOD bytesInBlock;
      amountWritten := MIN(bytesInBlock-tempVar, nread);

      (* copy first data into users buffer *)
      SUBARRAY(dest, 0, amountWritten) := SUBARRAY(buffer.data^, tempVar, amountWritten);
      currentFileOffset := currentFileOffset + amountWritten;
      amountToRead := amountToRead - amountWritten;
      FastBuffer.Deallocate(buffer);
    END;

    (* now we can grab blocks as we're block aligned *)
    WHILE amountToRead > 0 DO
      curLocation := GetBlockLocation(fp, currentFileOffset DIV bytesInBlock);
      blockLocation := curLocation;		(* save current pos *)
      curOffset := currentFileOffset;
      consecutiveBlocks := 1;
      flag := TRUE;

      (* see if we reach the end *)
      WHILE (flag AND consecutiveBlocks * bytesInBlock < amountToRead) DO
	INC(curOffset, bytesInBlock);
	curLocation := GetBlockLocation(fp, curOffset DIV bytesInBlock);

	(* see if it's consecutive block *)
	IF ((curLocation.segment = blockLocation.segment) AND
	    (curLocation.block = blockLocation.block + consecutiveBlocks)) THEN
	  (* yes, check next block *)
	  INC(consecutiveBlocks);
	ELSE
	  (* nope, it's not consecutive.  exit while loop *)
	  flag := FALSE;
	END;
      END;

      IF DEBUG THEN
        PrintDiskAddress("LFS.Read: reading " & Fmt.Int(consecutiveBlocks)
	     & " consecutive blocks from ", blockLocation);
      END;

      IF (amountToRead < consecutiveBlocks * bytesInBlock) THEN
	(* the last block is partially filled *)
        ReadFromDiskOrBuffer(blockLocation, fp.mp,
		SUBARRAY(dest, amountWritten,amountToRead));
	amountWritten := amountWritten + amountToRead;
        amountToRead := 0;
      ELSE
	(* blocks that may contain the last block if it's block aligned *)
        ReadFromDiskOrBuffer(blockLocation,fp.mp,
		SUBARRAY(dest, amountWritten, consecutiveBlocks * bytesInBlock));
	amountWritten := amountWritten +  consecutiveBlocks * bytesInBlock;
        amountToRead := amountToRead -  consecutiveBlocks * bytesInBlock;
        currentFileOffset := currentFileOffset+ consecutiveBlocks * bytesInBlock;
      END;
    END;
    
    IF DEBUG THEN
      IO.Put("read thinks it sent "&Fmt.Int(amountWritten)&" chars to dest.\n");
    END;

    RETURN amountWritten;

  END Read; 
  
PROCEDURE GetBlockLocation(fp : T ; block: CARDINAL)
	: DiskAddress RAISES {Error.E} =
  BEGIN
    TRY
      RETURN fp.inode.getBlockLoc(block);
    EXCEPT
    | INode.InvalidBlockRequest => 
      Raise("iNode returned invalid block request for block " & Fmt.Int(block));
    END;
  END GetBlockLocation;

  (* system write call *)
PROCEDURE Write (fp      : T;
                 offset  : File.OffsetT;
                 READONLY data    : ARRAY OF CHAR): CARDINAL RAISES {Error.E} =
  PROCEDURE SetINodeInfo() =
    BEGIN
      iNodeInfo.iNode := iNum;
      iNodeInfo.block := currentBlockNum;
      iNodeInfo.diskAddress := writeLocation;
    END SetINodeInfo;
  PROCEDURE AddINodeInfoArray() =
    BEGIN
      TRY
        EVAL iNodeInfoArray.addElement(iNodeInfo);
      EXCEPT
      (* Well at the moment this exception is never raised
      | INodeInfoArray.INodeInfoArrayError =>
	FastBuffer.Deallocate(buffer);
        Raise("Write error when adding data to iNodeInfoArray");
      *)
      END;
   END AddINodeInfoArray;
  VAR
  
    originalFileSize   : CARDINAL;
    blockLocation      : DiskAddress;
    buffer	       : FastBuffer.T;
    segSumEntry        : SegSummaryEntry;
    bytesInBlock       : CARDINAL;
    currentBlockByteIndex  : CARDINAL;
    currentBlockNum    : CARDINAL;
    amountWritten      : CARDINAL;
    written            : CARDINAL;
    bytesLeftToWrite   : INTEGER;
    writeLocation      : DiskAddress;
    iNum               : CARDINAL;
    iNodeUpdateArrayIndex : CARDINAL;
    iNodeUpdateArray   : REF ARRAY OF INodeInfo.T;
    iNodeInfoArray     : INodeInfoArray.T;
    iNodeInfo          : INodeInfo.T;

    nWrite             : CARDINAL;

  BEGIN

    (* first check to see that the file isn't a directory, if it is then write fails *)
    IF fp.inode.isDir() = TRUE THEN
      Raise("Write attempted to directory iNode.");
    END;

    (* then check to see if this file was opened readonly, if yes then deny *)
    IF Word.And(fp.flags,LFSTypes.ORDONLY) = LFSTypes.ORDONLY THEN
      Raise("Write attempted on file opened for readonly.");
    END;

    (* if we're asked to write starting past the current end of file *)
    IF offset > fp.inode.byteSize() THEN
      Raise("Write offset exceeds file size");
    END;
    (**********************)
    (* init stuff *)
    nWrite := BYTESIZE(data);
    bytesInBlock := fp.mp.segInfo.bytesInBlock;
    iNodeUpdateArrayIndex := 0;

    iNodeInfoArray :=  NewINodeInfoArray(nWrite DIV bytesInBlock + 1);
    
    buffer := FastBuffer.Allocate(bytesInBlock);
    
    bytesLeftToWrite := nWrite;
    
    iNum := fp.inode.getINum();

    amountWritten:=0;

    iNodeInfo := NEW(INodeInfo.T);

    (**********************)
    (* if this write is going to cause the file to grow, then grow it *)
    originalFileSize := fp.inode.byteSize();

    (* if we're open for truncation then fix the new file size *)
    IF Word.And(fp.flags, LFSTypes.OTRUNC) = LFSTypes.OTRUNC THEN
      fp.inode.setbyteSize(offset+nWrite);
    ELSIF offset+nWrite > originalFileSize THEN
      (* else we're not open for truncation *)
      fp.inode.setbyteSize(offset+nWrite);
    END;
        
    currentBlockByteIndex := offset MOD bytesInBlock;
    (* which byte in a files block we will write into *)

    currentBlockNum := offset DIV bytesInBlock;  
    (* block number in file where we'll write *)

    (*******************************************************)
    (* if we're to start writing to the middle of a block then we need to use up the rest
       of the current block before proceding to the next one *)
    IF currentBlockByteIndex # 0 THEN

      blockLocation := GetBlockLocation(fp, currentBlockNum);

      IF DEBUG THEN PrintDiskAddress("Read. first blk at ", blockLocation); END;
      
      ReadFromDiskOrBuffer(blockLocation, fp.mp, buffer.data^);

      amountWritten := (bytesInBlock-currentBlockByteIndex);
      
      IF amountWritten > bytesLeftToWrite THEN
        amountWritten := bytesLeftToWrite;
      END;

      bytesLeftToWrite := bytesLeftToWrite - amountWritten;

      (* fill up the rest of the first block *)
      SUBARRAY(buffer.data^, currentBlockByteIndex, amountWritten):=
          SUBARRAY(data, 0, amountWritten);

      (* and write that block to the lfs write buffer*)
      segSumEntry:=SegSummaryEntry{iNum, currentBlockNum, LFSTypes.DATA};

      TRY
        writeLocation := fp.mp.segBuff.write(buffer.data, segSumEntry, bytesInBlock);
      EXCEPT
        | Segment.NoFreeSegs => IO.Put("Tried to flush segment buffer and got no free segments\n");
	  FastBuffer.Deallocate(buffer);
          Raise("Disk error, no free segments");
      END;

      IF DEBUG THEN
	PrintDiskAddress("Write. wrote partial first block at ", writeLocation);
      END;
      
      (* and add that location to the seg array *)
      SetINodeInfo();
      AddINodeInfoArray();
      
      currentBlockNum := currentBlockNum + 1;
    END;
    (* done eating up the remainder of the first uneven block *)

    (* if we aren't writing a final, fractional block *)
    WHILE bytesLeftToWrite > bytesInBlock DO

      (* iNum doesn't change *)
      (* flag is always 0 as this is data *)
      (* only thing to change is currentBlockNum,
         method can take an initial currentBlockNum and run with that.

         We know that:
           segSumEntry.iNode:= iNum;
           segSumEntry.offset := currentBlockNum;
           segSumEntry.flag := 0;
      *)

      TRY
        writeLocation := 
            fp.mp.segBuff.refWrite(data, 
                                   iNum,                        (* file that owns data we are writing *)
                                   currentBlockNum,             (* offset in file of first block of data, in blocks *)
                                   amountWritten,               (* byteNum in array to start copying *)
                                   written);                    (* number of bytes written to segBuffer from data array *)

	IF DEBUG THEN
	  PrintDiskAddress("refWrite writes " & Fmt.Int(written) & " bytes at ",
		writeLocation);
        END;

      EXCEPT
      | Segment.NoFreeSegs =>
        Raise("Disk error, no free segments");
      END;

      INC(amountWritten,written);

      (* add all write info to iNodeInfoArray, so inode can be updated *)
      SetINodeInfo();

      FOR i := 0 TO (written DIV bytesInBlock)-1 DO
        IF DEBUG THEN PrintDiskAddress("Write. ch", iNodeInfo.diskAddress); END;
	AddINodeInfoArray();
        INC(currentBlockNum,1);
        iNodeInfo.block := currentBlockNum;
        INC(iNodeInfo.diskAddress.block,1);
      END;
      bytesLeftToWrite := bytesLeftToWrite - written;
    END;	(* end of WHILE *)

    (* at this point we only have a partial block remaining, if anything *)
    IF bytesLeftToWrite > 0 THEN

        (* now there are two ways to write the last block, either it is past
	 * the end of the old file, in which case we just write it, or, more
	 * tricky the last block is the first part of an existing file block
	 * on disk.
	 *)
        
        (* if the last block contains old file data...*)
        IF (offset+nWrite)<originalFileSize THEN
          blockLocation := GetBlockLocation(fp, 
                (offset+amountWritten) DIV bytesInBlock);
          
          ReadFromDiskOrBuffer(blockLocation, fp.mp, buffer.data^);
        
          (* fill up the rest of this last block *)
          SUBARRAY(buffer.data^, 0, bytesLeftToWrite):=
              SUBARRAY(data, amountWritten, bytesLeftToWrite);
          
          (* AND write that block to the lfs write buffer *)
          segSumEntry:=SegSummaryEntry{iNum, currentBlockNum, LFSTypes.DATA};

          TRY
            writeLocation := fp.mp.segBuff.write(buffer.data, segSumEntry,bytesInBlock);
          EXCEPT
          | Segment.NoFreeSegs =>
	    FastBuffer.Deallocate(buffer);
            Raise("Disk error, no free segments");
          END;
	  IF DEBUG THEN
	    PrintDiskAddress("Write. last partial block. " &
		Fmt.Int(bytesLeftToWrite) & " bytes at ", writeLocation);
	  END;

          (* and add that location to the seg array *)
	  SetINodeInfo();
	  AddINodeInfoArray();
          
          currentBlockNum := currentBlockNum + 1;
        
        ELSE
          (* else write's last block is completely new *)
	  segSumEntry := SegSummaryEntry{iNum, currentBlockNum, LFSTypes.DATA};
          
          (* right now unwritten parts of the buffer can be dirty *)
          SUBARRAY(buffer.data^, 0, bytesLeftToWrite) :=
          	SUBARRAY(data,amountWritten,bytesLeftToWrite);
          TRY
            writeLocation := fp.mp.segBuff.write(buffer.data, segSumEntry,bytesInBlock);
          EXCEPT
          | Segment.NoFreeSegs =>
	    FastBuffer.Deallocate(buffer);
            Raise("Disk error, no free segments");
          END;
	  IF DEBUG THEN
	    PrintDiskAddress("Write last coml. block " &
		Fmt.Int(bytesLeftToWrite) & " bytes at ", writeLocation);
	  END;

          (* and add that location to the seg array *)
	  SetINodeInfo();
	  AddINodeInfoArray();

        END; (* END of if last block is all new *)
      
        (* update the various counters *)
        currentBlockNum := currentBlockNum + 1;

        amountWritten := amountWritten + bytesLeftToWrite;
        bytesLeftToWrite := 0;

        IF amountWritten # nWrite THEN
          IO.PutError("Write, amount written was "&Fmt.Int(amountWritten)&" not "&
              Fmt.Int(nWrite)&".\n");
        END;

      END; (* end of if writing all complete blocks *)

      (* ok, NOW we tell the inode all about the blocks we copied to disk in it's name *)
      iNodeUpdateArray:= iNodeInfoArray.getArray();
      
      EVAL fp.inode.update(iNodeUpdateArray^,iNodeInfoArray.size());

      FastBuffer.Deallocate(buffer);
      RETURN amountWritten;
    END Write;

PROCEDURE Close (fp: T) =
  BEGIN
    (* if the file was opened for other than readonly, then we'll flush
       the iNode to store changes *)
    IF Word.And(fp.flags,LFSTypes.ORDONLY) # LFSTypes.ORDONLY THEN
      fp.inode.flush();
    END;
  END Close;

(* pos is the place to start reading in the file array, buf is a subarray of
 * the filearray, returns the number of names in the returned list
 *)
PROCEDURE GetDirEntries(fp:T; pos:INTEGER; VAR buf:ARRAY OF DirEnt.T)
	: CARDINAL RAISES {Error.E} =
  BEGIN
    IF DEBUG THEN
      IO.Put("LFS.GetDirEntries called. buf size="&Fmt.Int(NUMBER(buf))&".\n");
    END;
    
    RETURN fp.inode.getFileArray(pos,buf);
    
  END GetDirEntries;

PROCEDURE Stat(fp:T; VAR s: FileStat.T) =
  BEGIN

    s.ino := fp.inode.getINum();
    s.size:= fp.inode.byteSize();
    s.atime:= 0;
    s.mtime:= 0;
    s.blksize:=fp.mp.segInfo.bytesInBlock;

    IF s.blksize # 0 AND (s.size+s.blksize-1) > 0 THEN
      s.blocks:= (s.size+s.blksize-1) DIV s.blksize;
    ELSE
      s.blocks:= 0;
    END;

    s.gen := fp.inode.getVersion();
    
    (* what the fuck are these for? *)
    (* Im not going to mess with them *)
    (*
      s.dev := 0;
      s.mode:= 0;
      s.nlink:=1;
      s.uid := 0;
      s.gid := 0;
      s.rdev:= 0;
      s.space1:=0;
      s.space2:=0;
    s.ctime :=0;
    s.space3:=0;
    *)
  END Stat;

BEGIN
  EVAL LFSInterface.Export(NEW(Auth.AuthAlways));

  Init(TRUE);
END LFS.

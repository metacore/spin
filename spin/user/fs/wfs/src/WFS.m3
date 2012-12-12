(* Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 * 
 * HISTORY
 * 17-Sep-96  becker at the University of Washington
 *	Updated to current File.T method sigs
 *	
 * 23-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	added getDirEntries, and removed dirhandle stuff.
 *
 * 05-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed the "from" parameter in Read() to CARDINAL.
 *
 * 01-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *)

(* "WFS" implements a readonly web server based file system *)

MODULE WFS;

IMPORT Directory, Error, File, FileId, FileStat, FileSystem, IO,
       NameServer, Text, Fmt, Fetch, Errno;

(* ------------------- FILEID SUPPORT ------------------- *)

CONST NULLCHAR = VAL(16_00,CHAR);
VAR FileIdStart := FileId.T{   
       'w',      'f',      's', NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, 
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, 
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, 
  NULLCHAR, NULLCHAR};


  END methodDirectoryCreate;

PROCEDURE methodDirectoryLookup(
    self          : DirectoryT; 
    name                     : NameServer.Name; 
    <*UNUSED*> VAR component : NameServer.Name;
    <*UNUSED*> VAR parent    : NameServer.TBase): REFANY 
  RAISES {NameServer.Error} = 
  VAR entry      : FileT;
  BEGIN
    IO.Put("WFS.Lookup retrieving " & 
      self.mp.hostname & ":" &name & "\n");
    entry := NEW(FileT).init();
    entry.data := Fetch.GetFile(self.mp.hostname,name);
    IF entry.data # NIL THEN
      RETURN entry;
    END;
    RAISE NameServer.Error(NameServer.EC.NameNotFound);
  END methodDirectoryLookup;

PROCEDURE methodDirectoryAttach(
    <*UNUSED*> self : DirectoryT; 
    <*UNUSED*> name : NameServer.Name; 
    <*UNUSED*> obj  : REFANY) 
  RAISES {NameServer.Error} <* NOWARN *>= 
  BEGIN
    (* do nothing *)
  END methodDirectoryAttach;

PROCEDURE methodDirectoryDetach(
    <*UNUSED*> self: DirectoryT; 
    <*UNUSED*> name: NameServer.Name; 
    <*UNUSED*> VAR obj: REFANY) 
  RAISES {NameServer.Error} <* NOWARN *> = 
  BEGIN
    (* do nothing *)
  END methodDirectoryDetach;

PROCEDURE methodDirectoryIterate(<*UNUSED*> self: DirectoryT): 
  NameServer.Iterator = 
  BEGIN
    RETURN NEW(Iterator);
  END methodDirectoryIterate;

TYPE Iterator = NameServer.Iterator OBJECT
  OVERRIDES
    next := methodNext;
    seek := methodSeek;
    reset:= methodReset;
  END;

PROCEDURE methodNext(
    <*UNUSED*> self : Iterator;
    VAR (*OUT*) k: NameServer.Name; 
    VAR (*OUT*) v: REFANY) : BOOLEAN = 
  BEGIN
    k := NIL; v := NIL;
    RETURN FALSE;
  END methodNext;

PROCEDURE methodSeek(
    <*UNUSED*> self: Iterator; 
    <*UNUSED*> READONLY k:NameServer.Name) = 
  BEGIN
  END methodSeek;

PROCEDURE methodReset(<*UNUSED*> self: Iterator) = 
  BEGIN
  END methodReset;

(* ------------------- FILE SERVICES ------------------- *)

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

REVEAL FileT = File.T BRANDED OBJECT

    (* wfs first downloads the entire file and then serves chunks
       from ram.  We keep data around using a simple data pointer.  We
       may want to improve it by using a collection of fixed sized
       buffers.  *)
    mu   : MUTEX;
    data : REF ARRAY OF CHAR;  (* buffer for data block *)
  METHODS
    init(): FileT := methodFileInit;
  OVERRIDES
    readRef := methodFileReadRef;
    read    := methodFileRead;
    close   := methodFileClose;
    open    := methodFileOpen;
    stat    := methodFileStat;
  END;

PROCEDURE methodFileInit(self:FileT):FileT = 
  BEGIN
    self.mu := NEW(MUTEX);
    RETURN self;
  END methodFileInit;


PROCEDURE Open (mp: MP; <*UNUSED*> mode: INTEGER; path: TEXT): File.T
  RAISES {Error.E} =
  VAR fp: T;
  BEGIN
    fp := NewT();
    fp.mp := mp;
  END Open;
  
    

PROCEDURE Read (fp  : T;
                offset: File.OffsetT;
		VAR data: ARRAY OF CHAR): CARDINAL RAISES {Error.E} =
  VAR
      bytes: CARDINAL := NUMBER(data);
  BEGIN
    LOCK fp.mu DO
      IF bytes = 0 THEN RETURN 0; END;
      IF fp.data = NIL THEN
        RAISE
          Error.E(NEW(File.ErrorT).init(File.FS_NOT_IN_FILE));
      END;
      IF offset > BYTESIZE(fp.data^) THEN
        RAISE
          Error.E(NEW(File.ErrorT).init(File.FS_NOT_IN_FILE));
      END;
      IF offset + bytes > BYTESIZE(fp.data^) THEN bytes := BYTESIZE(fp.data^) - offset END;

      IO.Put("(1) WFS: offset ");  IO.PutInt(offset);
      IO.Put("nread ");  IO.PutInt(bytes);
      IO.Put("fp.data size "); IO.PutInt(BYTESIZE(fp.data^));
      IO.Put("\n");

      dprint("Reading " & Fmt.Int(bytes) & " " & Fmt.Int(NUMBER(fp.data^))
        & Fmt.Int(NUMBER(data)) & " \n");
      SUBARRAY(data, 0, bytes) := SUBARRAY(fp.data^, offset, bytes);
    END;
    RETURN bytes;
  END Read;
  

PROCEDURE Stat (fp: T; VAR s: FileStat.T) =
  BEGIN
    LOCK fp.mu DO
      IF fp.data = NIL THEN
        s.size := 0;
        s.mode := 0;
        s.mtime := 0;
        s.atime := 0;
        s.ctime := 0;
      ELSE
        s.size := NUMBER(fp.data^);
        s.mode := 0;
        s.mtime := 0;
        s.atime := 0;
        s.ctime := 0;
      END;
    END;
  END Stat;


PROCEDURE Close (fp: T) =
  BEGIN
    LOCK fp.mu DO fp.data := NIL; END;
  END Close;

PROCEDURE GetDirEntries (<*UNUSED*>fp : T; pos : INTEGER;
			 VAR buf : ARRAY OF DirEnt.T) : CARDINAL RAISES {Error.E} =
  BEGIN
    (*XXX bogus *)
    IF pos # 0 THEN RETURN 0; END;
    buf[0].name := "Hello Mom";
    buf[0].nextPos := 1;
    buf[0].ino := 777;
    RETURN 1;
  END GetDirEntries;




PROCEDURE methodFileReadRef (
    self      : FileT;
    offset    : File.OffsetT;
    VAR bytes : CARDINAL;  (* how many to read; how many got *)
    VAR data  : REF ARRAY OF CHAR;
    VAR from  : CARDINAL            ) RAISES {Error.E} =
  VAR
    databytes : CARDINAL;
  BEGIN
    IF bytes = 0 THEN RETURN; END;
    IF self.data = NIL THEN
      RAISE
        Error.E(NEW(File.ErrorT).init(File.FS_NOT_IN_FILE));
    END;
    IF data = NIL THEN
      data := NEW(REF ARRAY OF CHAR, MIN(bytes, NUMBER(self.data^)));
    END;

    IF from = 0 THEN from := FIRST(data^); END;

    databytes := NUMBER(self.data^);
    IF offset > databytes THEN
      RAISE
        Error.E(NEW(File.ErrorT).init(File.FS_NOT_IN_FILE));
    END;

    bytes := NUMBER(data^);
    IF offset + bytes > databytes THEN bytes := databytes - offset; END;
    SUBARRAY(data^, from, bytes) := SUBARRAY(self.data^, offset, bytes);
  END methodFileReadRef; 

PROCEDURE methodFileRead(
    self     : FileT;
    offset   : File.OffsetT; 
    VAR data : ARRAY OF CHAR): CARDINAL 
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
    self : FileT;
    <*UNUSED*>mode : INTEGER): File.T
  RAISES { Error.E } <*NOWARN*> =
  VAR
  BEGIN
    RETURN self;
  END methodFileOpen;

PROCEDURE methodFileClose (self: FileT) =
  BEGIN
    IF self.data # NIL THEN
      self.data := NIL;
    END;
  END methodFileClose;

PROCEDURE methodFileStat (self : FileT; VAR stat : FileStat.T) =
  BEGIN
    IF self.data # NIL THEN
      stat.size := NUMBER(self.data^);
    ELSE
      stat.size := 0;
    END;
  END methodFileStat;

(* ------------------- MOUNT SERVICES ------------------- *)

(* The type "MountT" represents a mount point exported by this file
   system.  The tftp file system does not maintain additional
   information for mount points, consequently is acts just like a
   regular DiretoryT.  A MountT is a subtype of a DirectoryT. *)

TYPE MountT = DirectoryT OBJECT
    hostname : TEXT;
END;

TYPE FileSystemT = FileSystem.T OBJECT
  OVERRIDES
    (* "create" is called when we create a new mount point *)
    create := methodFileSystemCreate;

    (* "attach" is called when we mount this file system *)
    attach := methodFileSystemAttach;

    (* "detach" is called when we unmount *)
    detach := methodFileSystemDetach;
  END;

PROCEDURE methodFileSystemCreate(
    self           : FileSystemT; 
    name           : NameServer.Name;
    <*UNUSED*>root : NameServer.TBase) : NameServer.TBase = 
  VAR new: MountT;
  BEGIN
    new :=  NEW(MountT).init(FileSystem.GetRoot());
    self.attach(name,new);
    RETURN new;
  END methodFileSystemCreate;

PROCEDURE methodFileSystemAttach(
    self : FileSystemT; 
    name : NameServer.Name; 
    obj  : REFANY) 
  RAISES {NameServer.Error} = 

  PROCEDURE Mount(mp: MountT) = 
    BEGIN
      mp.hostname := name;
    END Mount;

  BEGIN
    TYPECASE obj OF 
    | MountT(mp) => 
      Mount(mp);
      FileSystem.T.attach(self,name,obj);
    ELSE
      (* BUG IF WE GET HERE *)
      <* ASSERT FALSE *>
    END;
  END methodFileSystemAttach; 

PROCEDURE methodFileSystemDetach(
    self: FileSystemT; 
    name: NameServer.Name; 
    VAR obj: REFANY) 
  RAISES {NameServer.Error} = 
  BEGIN
    TYPECASE obj OF 
    | MountT =>
      (* detach mount point from the fs/registry/ufs directory *)    
      FileSystem.T.detach(self,name,obj);

      (* go through unmount procedure for wfs. *)
      (* do nothing *)
    ELSE
      (* BUG IF WE GET HERE *)
      <* ASSERT FALSE *>
    END;
  END methodFileSystemDetach; 

(* ------------------- FileSystem Registration ------------------- *)

PROCEDURE Init(<*UNUSED*>verbose:BOOLEAN) =
  BEGIN
    TRY
      WITH new = NEW(FileSystemT).init(NIL) DO
        FileSystem.Register("wfs",new);
      END;
    EXCEPT
    | Error.E(e) =>
      IO.Put(e.message()&" during TftpFileSystem initialization.\n");
    END;
  END Init;

CONST debug = 1;
PROCEDURE dprint (READONLY s: TEXT) =
  BEGIN
    IF debug # 0 THEN IO.Put(s); END;
  END dprint;


(******************)
BEGIN
  IO.Put("Registering new WFS file system\n");
  TRY
    FileSystem.Register("wfs", NEW(FS));
  EXCEPT
  | Error.E(e) =>
      IO.Put("Error.E " & e.message() & " during WFS initialization.\n");
  END;
END WFS.

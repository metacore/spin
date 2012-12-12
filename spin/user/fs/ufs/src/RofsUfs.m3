(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Dec-97  David Becker at the University of Washington
 *	Fixed Newfs() to actually open the device being mounted.
 *
 * 23-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Rewrote using new NS interface.
 * 04-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Rewrote bogus readdir.
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Removed hack where DirHeader structure was duplicated into this file.
 *
 * 07-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added inode cache. Added stat.
 *
 * 30-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Encased all calls to dprint() in an IF statement that hopefully
 *	will be eliminited when debug is set to FALSE.
 *
 * 05-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed the "from" parameter in Read() to CARDINAL.
 *
 * 13-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made safe.
 *
 * 05-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Renamed from FileIO.m3. Now conforms to FileSystem interface.
 *      Moved outside the kernel.
 *
 * 21-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *      Changed to use the types defined in MachTypes.
 *	Took out references to reader/writer locks which are not used.
 *
 * 20-Dec-95  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to use Device.read instead of readForFileIO.
 *      You can mount multiple devices. Per-handle locks.
 *
 * 02-Dec-95  Yasushi Saito (yasushi) at the University of Washington
 *	Fixed the bug that it can't handle indirect blocks.
 *	NewT and free_fp are subsumed in Open and Close.
 *      Added globalMutex to serialize execution of public procedures.
 *
 * 29-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	Merged read_fs into mount_fs. Removed redundant call to mount_fs.
 *      Eliminated use of M3toC, M3toC2, and changed to use SafeConvert.
 *      This fixes the 24-byte memory-leak.
 *
 * 28-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Took out code that expects to see "/dev/" infront of the device
 *	name for mount.
 *
 * 27-Oct-95  Yasushi Saito (yasushi) at the University of Washington
 *	unmount now works.
 *
 * 20-Oct-95  Yasushi Saito (yasushi) at the University of Washington
 *	Converted into M3 code from C.
 *
 * 02-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	converting over to M3 implementation.
 *
 * 06-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Merged into glue code to interface between Rofs and CAM.
 *
 * 27-Jan-95  Marc Fiuczynski (mef) at the University of Washington
 *	ReadOnly filesystem support for SPIN.
 *)

(* "RofsUfs" implements a file system interface that only support open
   and read on the ufs filesystem used by Digital Unix.  It is
   implemented using the FileSystem object interfaces. *)

MODULE RofsUfs EXPORTS UnixFS;

IMPORT Cache, Ctypes, Device, Disk, (* DirEnt, *) Directory, Error,
       File, FileDefs, FileId, FileStat, FileSystem, Fmt, IO,
       NameServer, Text, UfsFs, Word;
IMPORT Symlink;
IMPORT StatFs;
IMPORT ArrayUtils;
IMPORT FSRoot;
IMPORT NSName;

(* XXX quieting compiler. Must fix with better exception infrastructure. *)
<*FATAL NameServer.Error, Error.E*>

CONST
  BLOCK_SIZE = 512;
  Debug = FALSE;

TYPE VnodeT = RECORD
  mp: MountT;
  buf: REF ARRAY OF CHAR; (* buffer for data block *)
  inode: UfsFs.icommon;     (* copy of on-disk inode *)
  inumber: FileStat.InoT;     (* inode number *)
  bufBlkno: FileDefs.daddr_t;  (* (fileoffset/fragsize) of the data block *)
END;

(* UFS uses the file id field in the following way. *)
TYPE UFSFileID = RECORD
  header: ARRAY [0..3] OF CHAR;
  ino: INTEGER;
END;

TYPE UFSRoot = FSRoot.T OBJECT
  mp: MountT;
OVERRIDES
  statfs := Statfs;
END;

PROCEDURE Statfs (self: UFSRoot; VAR s: StatFs.T) =
  VAR mp := self.mp;
  BEGIN
    s.type := 1; (* MOUNT_UFS. see sys/mount.h *)
    s.flags := 0;
    s.fsize := mp.sb.fs_bsize;
    s.bsize := mp.sb.fs_bsize;
    
    s.blocks := mp.sb.fs_size;
    s.bfree := 0; (* XXX *)
    s.bavail := 0; (* XXX *)
    s.files := 0;
    s.ffree := 0;
    (* XXX I don't know how fsid is used. *)
    Text.SetChars(s.mntonname, "/Mumbo/Jumbo"); (* XXX *)
    Text.SetChars(s.mntfromname,  mp.devName);
  END Statfs;
  
PROCEDURE InoToFileId (ino: INTEGER; VAR fid: FileId.T) =
  BEGIN 
    ArrayUtils.Clear(VIEW(fid, ARRAY OF CHAR));
    WITH uid = VIEW(fid, UFSFileID) DO
      uid.header[0] := 'u';
      uid.header[1] := 'f';
      uid.header[2] := 's';
      uid.ino := ino;
    END;
  END InoToFileId;

  
(* ------------------- DIRECTORY SERVICES ------------------- *)

(* The "DirectoryT" defines what a directory looks like for this
   filesystem.  A DirectoryT is a subtype of fscore's Directory.T *)
TYPE DirectoryT = Directory.T BRANDED OBJECT
    mu: MUTEX;
    vnode: VnodeT;
  OVERRIDES
    init := DirectoryInit;
    lookup := DirectoryLookup;
    getEntries := GetEntries;
    root := DirRoot;
  END;

(* "DirectoryInit" is only visible from within this module *)
PROCEDURE DirectoryInit(self: DirectoryT): NameServer.T =
  BEGIN
    self.mu := NEW(MUTEX);
    self.vnode.bufBlkno := -1;
    RETURN Directory.T.init(self);
  END DirectoryInit; 

(* "SearchDirectory" searches a directory FP for a NAME and return its
 i_number.
 
 Raise NameServer.EC.NameNotFound if file not found. Raise exception
 if a device error happens.
 *)
PROCEDURE SearchDirectory (READONLY name: ARRAY OF CHAR;
			   dir: DirectoryT): FileDefs.ino_t =
  VAR
    buf   : REF ARRAY OF CHAR; (* temp used to hold directory contents. *)
    offset: INTEGER; (* offset within the directory FP *)
    bufStart: INTEGER;
    length: INTEGER; (* length of NAME *)
    bufSize: INTEGER := 0;
    dirSize: CARDINAL := dir.vnode.inode.ic_size.size;
  BEGIN
    length := NUMBER(name);
    
    (* This is important. Directory always contains "." and "..", so
     it't not empty. We can save one trivial empty-directory checking here *)
    <*ASSERT dirSize # 0*>
    
    offset := 0;
    LOOP
      IF bufSize <= 0 THEN
	TRY
	  BufReadFile(dir.vnode, offset, buf, bufStart, bufSize);
	EXCEPT
	| Error.E =>
	  IO.Put("RofsUfs.SearchDirectory bufreadfile failed.\n");
	  RETURN 0;
	END;
	IF bufSize = 0 THEN EXIT; END;
      END;
      
      WITH dp = VIEW(SUBARRAY(buf^, bufStart, BYTESIZE(UfsFs.DirEntry)), UfsFs.DirEntry) DO
	IF dp.d_ino # 0 AND dp.d_namelen = length THEN
	  VAR match: BOOLEAN := TRUE;
	  BEGIN
	    WITH entryName = SUBARRAY(buf^, bufStart+BYTESIZE(UfsFs.DirEntry), length) DO
	      FOR i := 0 TO MIN(length, LAST(entryName)) DO
		IF name[i] # entryName[i] THEN
		  match := FALSE;
		  EXIT;
		END
	      END;
	    END;
	    IF match THEN RETURN dp.d_ino; END;
	  END;
	END;
	INC(offset, dp.d_reclen);
	INC(bufStart, dp.d_reclen);
	DEC(bufSize, dp.d_reclen);
      END;
      IF offset >= dirSize THEN EXIT; END;
    END;
    RETURN 0;
  END SearchDirectory;
  
PROCEDURE UfsLookup (dir: DirectoryT; file: NameServer.Name): REFANY =
  VAR
    inumber: FileDefs.ino_t := 0;
    parent: DirectoryT;
    entry: REFANY := NIL;
  BEGIN
    (* Save directory ino in case we find a symbolic link. *)
    parent := dir;

    WITH fileName = SUBARRAY(file.str^, file.from, file.end-file.from) DO
      (* Get the ino number for the component in current directory. *)
      inumber := SearchDirectory(fileName, dir);
    END;

    IF inumber # 0 THEN 
      entry := vnodeCache.find(dir.vnode.mp, inumber);
      IF entry = NIL THEN
	VAR inode: UfsFs.icommon;
	BEGIN
	  (* Get the object represented by this inode *)
	  FindInode(dir.vnode.mp, inumber, inode);
	  CASE (Word.And(inode.ic_mode, UfsFs.IFMT)) OF
	  | UfsFs.IFCHR => (* character special *)
	    IF Debug THEN 
	      IO.Put("FindInode found a character special inode.\n");
	    END;
	  | UfsFs.IFDIR => (* directory *)
	    IF Debug THEN 
	      IO.Put("FindInode found a directory inode.\n");
	    END;
	    WITH new = NEW(DirectoryT) DO
	      EVAL new.init();
	      new.vnode.mp := dir.vnode.mp;
	      new.vnode.inumber := inumber;
	      new.vnode.inode := inode;
	      InoToFileId(inumber, new.id);
	      vnodeCache.insert(new, new.vnode.mp, new.vnode.inumber);
	      entry := new;
	    END;
	  | UfsFs.IFBLK => (* block special *)
	    IF Debug THEN 
	      IO.Put("FindInode found a block special inode.\n");
	    END;
	    
	  | UfsFs.IFREG => (* regular *)
	    IF Debug THEN 
	      IO.Put("FindInode found a regular inode.\n");
	    END;
	    WITH new = NEW(FileT).init(inumber, dir.vnode.mp) DO
	      new.vnode.inode := inode;
	      vnodeCache.insert(new, new.vnode.mp, new.vnode.inumber);
	      entry := new;
	    END;
	  | UfsFs.IFLNK => (* symbolic link *)
	    IF Debug THEN 
	      IO.Put("FindInode found a symbolic link inode.\n");
	    END;
	    WITH new = NEW(LinkT) DO
	      new.vnode.inode := inode;
	      EVAL new.init(dir, Deref(new.vnode));
	      vnodeCache.insert(new, new.vnode.mp, new.vnode.inumber);
	      entry := new;
	    END;
	  | UfsFs.IFSOCK => (* socket *)
	    IF Debug THEN 
	      IO.Put("FindInode found a socket inode.\n");
	    END;
	  ELSE
	    IO.PutError("RofsUfs FindInode found an unknown type inode.\n");
	  END;
	END;
      END;
    END;
    RETURN entry;
  END UfsLookup;
  
(* "DirectoryLookup" searches file given a pathname "name", and
   returns either a FileT or DirectoryT. *)
PROCEDURE DirectoryLookup (self: DirectoryT;
			   VAR name: NameServer.Name;
			   dontFollowSymlink: BOOLEAN): REFANY
  RAISES { NameServer.Error } = 
  VAR
    entry: REFANY;
    tmp, component: NameServer.Name;
  BEGIN
    (* Extract a component from "name" *)
    NameServer.GetComponent(name, component);
    tmp := component;
    TRY
      (* Check if we already know about this file *)
      entry := Directory.T.lookup(self, tmp, dontFollowSymlink);
    EXCEPT
    | NameServer.Error(ec) =>
      IF ec = NameServer.EC.NameNotFound THEN
	(* check if we have it on disk *)
	entry := UfsLookup(self, component);
	IF entry # NIL THEN 
	  self.attach(component,entry);
	ELSE
	  RAISE NameServer.Error(NameServer.EC.NameNotFound);
	END;
      ELSE
	RAISE NameServer.Error(ec);
      END;
    END;
    RETURN entry;
  END DirectoryLookup;

PROCEDURE GetEntries (dir: DirectoryT; offset: Word.T;
		      VAR ent: ARRAY OF NameServer.Entry): CARDINAL =
  VAR
    buf   : REF ARRAY OF CHAR; (* temp used to hold directory contents. *)
    bufStart: INTEGER;
    bufSize: INTEGER := 0;
    dirSize: CARDINAL := dir.vnode.inode.ic_size.size;
    idx: CARDINAL := 0;
  BEGIN
    
    (* This is important. Directory always contains "." and "..", so
     it't not empty. We can save one trivial empty-directory checking here *)
    <*ASSERT dirSize # 0*>
    
    LOOP
      IF bufSize <= 0 THEN
	(* Refill the buffer if we reached eob. *)
	TRY
	  BufReadFile(dir.vnode, offset, buf, bufStart, bufSize);
	EXCEPT
	| Error.E =>
	  IO.PutError("RofsUfs.SearchDirectory bufreadfile failed.\n");
	  RETURN idx;
	END;
	IF bufSize = 0 THEN EXIT; END;
      END;
        
      WITH dp = VIEW(SUBARRAY(buf^, bufStart, BYTESIZE(UfsFs.DirEntry)),
		     UfsFs.DirEntry) DO
	IF dp.d_ino # 0 THEN
	  VIEW(ent[idx].id, Ctypes.unsigned_int) := dp.d_ino;
	  WITH name = SUBARRAY(buf^, bufStart+BYTESIZE(UfsFs.DirEntry),
			       dp.d_namelen) DO
	    ent[idx].name := NSName.FromArrayChar(name);
	  END;
	  ent[idx].cookie := offset + dp.d_reclen;
	  INC(idx);
	  IF idx >= NUMBER(ent) THEN EXIT; END;
        END;
	INC(offset, dp.d_reclen);
	INC(bufStart, dp.d_reclen);
	DEC(bufSize, dp.d_reclen);
        IF offset >= dirSize THEN EXIT; END;
      END;
      
    END; (* loop *)
    RETURN idx;
  END GetEntries;
  
PROCEDURE DirRoot (self: DirectoryT): FSRoot.T =
  BEGIN
    RETURN NEW(UFSRoot, mp := self.vnode.mp);
  END DirRoot;

(* ------------------- SYMLINK SERVICES ------------------- *)

TYPE LinkT = Symlink.T OBJECT
    vnode  : VnodeT;
  OVERRIDES
    stat := LinkStat;
END;

PROCEDURE LinkStat (self: LinkT; VAR s: FileStat.T) =
  BEGIN
    s.dev     := 0; (*XXX fixme *)
    s.ino     := self.vnode.inumber;
    s.mode    := self.vnode.inode.ic_mode;
    s.nlink   := self.vnode.inode.ic_nlink;
    s.uid     := self.vnode.inode.ic_uid;
    s.gid     := self.vnode.inode.ic_gid;
    s.rdev    := 0;
    s.size    := self.vnode.inode.ic_size.size;
    s.atime   := self.vnode.inode.ic_atime;
    s.mtime   := self.vnode.inode.ic_mtime;
    s.ctime   := self.vnode.inode.ic_ctime;
    s.blksize := BLOCK_SIZE;
    s.blocks  := self.vnode.inode.ic_blocks;
    s.flags   := 0;
    s.gen     := self.vnode.inode.ic_gen;
  END LinkStat;
							
PROCEDURE PathFromChars (READONLY buf: ARRAY OF CHAR): NameServer.Name =
  VAR
    namelen: CARDINAL := 0;
    name: NameServer.Name;
  BEGIN
    (* read file for symbolic link from inode *)
    FOR i := FIRST(buf) TO LAST(buf) DO
      IF buf[namelen] = '\000' THEN  
        EXIT;
      END;
      INC(namelen);
    END;
    WITH symlinkname = SUBARRAY(buf,0,namelen) DO
      name := NSName.FromArrayChar(symlinkname);
    END;
    RETURN name;
  END PathFromChars;

PROCEDURE Deref(VAR vnode: VnodeT): NameServer.Name = 
  BEGIN
    IF (Word.And(vnode.inode.ic_flags, UfsFs.IC_FASTLINK) # 0) THEN
      RETURN PathFromChars(vnode.inode.ic_indirect);
    ELSE
      (* read file for symbolic link from data blocks *)
      VAR
        startOffset, size: INTEGER; (* unused *)
        buf: REF ARRAY OF CHAR;
      BEGIN
        BufReadFile(vnode, 0, buf, startOffset, size);
        RETURN PathFromChars(buf^);
      END;
    END;
  END Deref;

(* ------------------- FILE SERVICES ------------------- *)

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

TYPE FileT = File.T BRANDED OBJECT
    mu      : MUTEX;
    vnode   : VnodeT;
  METHODS
    init(inumber: FileStat.InoT; mp: MountT): FileT := FileInit;
  OVERRIDES
    read := FileRead;
    readRef := ReadRef;
    close := Close;
    open := Open;
    stat := Stat;
    root := FileRoot;
  END;

PROCEDURE FileInit(self: FileT;
			 inumber: FileStat.InoT; 
			 mp: MountT): FileT = 
  BEGIN
    self.mu := NEW(MUTEX);
    self.vnode.bufBlkno := -1;
    self.vnode.inumber  := inumber;
    self.vnode.mp := mp;
    InoToFileId(inumber, self.id);
    RETURN self;
  END FileInit;

PROCEDURE FileRead(self: FileT;
		   VAR data: ARRAY OF CHAR;
		   offset: File.OffsetT): CARDINAL 
  RAISES {Error.E} <*NOWARN*> = 
  VAR
    size   : INTEGER;
    bytes  : CARDINAL;
  VAR
    bytesread: FileDefs.vm_size_t := 0;
  BEGIN

    bytes := NUMBER(data);
    size  := bytes;
    
    LOCK self.mu DO
      TRY
        IF Debug THEN
          IO.Put("read_file: start reading "&Fmt.Int(size)&"bytes.\n");
        END;
        
        WHILE size > 0 DO
          IF Debug THEN
            IO.Put("read_file . remaining="&Fmt.Int(size)&"bytes\n");
          END;

          IF UfsFs.blkoff(self.vnode.mp.sb, offset) = 0
            AND size >= BlockSize(self.vnode, offset) THEN
            
            (* I'm glad we can read into buffer directly.
               Try to read as much into the block as possible. *)
            
            VAR
              (* bytenumber to read *)
              readSize: CARDINAL;

              (* fbn to start read *)
              startFileBlock := UfsFs.lblkno(self.vnode.mp.sb, offset);
              (* fbn to read up to *)
              endFileBlock   := startFileBlock;
              nextFileBlock  := startFileBlock + 1;
              
              (* lbn to read from *)
              startDiskBlock := BlockMap(self.vnode, startFileBlock);

              (* lbn to read up to *)
              endDiskBlock   := startDiskBlock;
            BEGIN
              IF startDiskBlock = 0 THEN
                (* we are already at EOF *)
                RAISE Error.E(NEW(File.ErrorT).init(File.FS_NOT_IN_FILE));
              END;
              
              (* Go as far as block # is contiguous *)
              WHILE nextFileBlock < UfsFs.lblkno(self.vnode.mp.sb, offset + size) DO 
                WITH nextDiskBlock = BlockMap(self.vnode,nextFileBlock) DO 
                  IF nextDiskBlock = 0
                    OR nextDiskBlock # endDiskBlock + self.vnode.mp.sb.fs_frag THEN
                    EXIT;
                  END;
                  endDiskBlock := nextDiskBlock;
                  endFileBlock := nextFileBlock;
                END;
                INC(nextFileBlock);
              END;

              readSize := (endFileBlock - startFileBlock) * self.vnode.mp.sb.fs_bsize
              + BlockSize(self.vnode, endFileBlock);
              

              IF Debug THEN
                IO.Put(Fmt.F("read: trying to read %s file block(s)(%s bytes)\n",
                             Fmt.Int(endFileBlock-startFileBlock+1),
                             Fmt.Int(readSize)));
              END;

              readSize := self.vnode.mp.dev.read(
                                      SUBARRAY(data,bytesread, readSize),
                                      UfsFs.fsbtodb(self.vnode.mp.sb, startDiskBlock)*BLOCK_SIZE);
              readSize := MIN(readSize, self.vnode.inode.ic_size.size - offset);
              
              <*ASSERT readSize > 0*>
              INC(offset, readSize);
              DEC(size, readSize);
              INC(bytesread, readSize);
            END;
          ELSE 
            VAR
              buf    : REF ARRAY OF CHAR;
              bufStart: INTEGER;
              bufSize: INTEGER;
            BEGIN
              BufReadFile(self.vnode, offset, buf, bufStart, bufSize);

              IF Debug THEN
                IO.Put("read_file <- BufReadFile off="&Fmt.Int(offset)&
                  ",start="&Fmt.Int(bufStart)&",size="&Fmt.Int(bufSize)&"\n");
              END;
              
              bufSize := MIN(size, bufSize);
              IF bufSize = 0 THEN EXIT; END;
              
              (* Copy BUF[bufStart..] into DATA[BYTESREAD .. ] *)
              SUBARRAY(data, bytesread, bufSize) :=
                  SUBARRAY(buf^, bufStart, bufSize);
              INC(offset, bufSize);
              DEC(size, bufSize);
              INC(bytesread, bufSize);
            END;
          END;
        END;
      EXCEPT
      | Error.E(e) =>
        bytes := bytesread;

        IF Debug THEN IO.Put("read_file RETURNs "&e.message()&"\n"); END;
        
        CASE e.resultCode() OF 
        | File.FS_NOT_IN_FILE => 
        ELSE

          IF Debug THEN IO.Put("read_file() error: "&e.message()&"\n"); END;
          RAISE Error.E(e);
        END;
      END; (* TRY *)
      bytes := bytesread;
      IF Debug THEN IO.Put("Read, read "&Fmt.Int(bytes)&"bytes\n"); END;
    END; (* LOCK *)
    RETURN bytes;
  END FileRead;

PROCEDURE ReadRef (self: FileT;
		   VAR data: REF ARRAY OF CHAR;
		   bytes: CARDINAL;  (* how many to READONLY; how many got *)
		   offset: File.OffsetT;
		   VAR from: CARDINAL): CARDINAL
  RAISES {Error.E} =
  VAR
    buf    : REF ARRAY OF CHAR;
    size   : INTEGER;
    bufStart: INTEGER;
    bufSize: INTEGER;
  VAR
    bytesread: FileDefs.vm_size_t := 0;
  BEGIN

    IF data = NIL THEN
      data := NEW(REF ARRAY OF CHAR, bytes);
    ELSE 
      bytes := MIN(bytes, LAST(data^)-from+1);
    END;

    size := bytes;
    
    LOCK self.mu DO
      TRY
        IF Debug THEN
          IO.Put("read_file: start reading "&Fmt.Int(size)&"bytes.\n");
        END;
        
        WHILE size > 0 DO
          IF Debug THEN
            IO.Put("read_file . remaining="&Fmt.Int(size)&"bytes\n");
          END;

          IF UfsFs.blkoff(self.vnode.mp.sb, offset) = 0
            AND size >= BlockSize(self.vnode, offset) THEN
            
            (* I'm glad we can read into buffer directly.
               Try to read as much into the block as possible. *)
            
            VAR
              (* bytenumber to read *)
              readSize: CARDINAL;

              (* fbn to start read *)
              startFileBlock := UfsFs.lblkno(self.vnode.mp.sb, offset);
              (* fbn to read up to *)
              endFileBlock   := startFileBlock;
              nextFileBlock  := startFileBlock + 1;
              
              (* lbn to read from *)
              startDiskBlock := BlockMap(self.vnode, startFileBlock);

              (* lbn to read up to *)
              endDiskBlock   := startDiskBlock;
            BEGIN
              IF startDiskBlock = 0 THEN
                (* we are already at EOF *)
                RAISE Error.E(NEW(File.ErrorT).init(File.FS_NOT_IN_FILE));
              END;
              
              (* Go as far as block # is contiguous *)
              WHILE nextFileBlock < UfsFs.lblkno(self.vnode.mp.sb, offset + size) DO 
                WITH nextDiskBlock = BlockMap(self.vnode,nextFileBlock) DO 
                  IF nextDiskBlock = 0
                    OR nextDiskBlock # endDiskBlock + self.vnode.mp.sb.fs_frag THEN
                    EXIT;
                  END;
                  endDiskBlock := nextDiskBlock;
                  endFileBlock := nextFileBlock;
                END;
                INC(nextFileBlock);
              END;

              readSize := (endFileBlock - startFileBlock) * self.vnode.mp.sb.fs_bsize
              + BlockSize(self.vnode, endFileBlock);
              

              IF Debug THEN
                IO.Put(Fmt.F("read: trying to read %s file block(s)(%s bytes)\n",
                             Fmt.Int(endFileBlock-startFileBlock+1),
                             Fmt.Int(readSize)));
              END;

              readSize := self.vnode.mp.dev.read(
                                      SUBARRAY(data^,from + bytesread, readSize),
                                      UfsFs.fsbtodb(self.vnode.mp.sb, startDiskBlock)*BLOCK_SIZE);
              readSize := MIN(readSize, self.vnode.inode.ic_size.size - offset);
              
              <*ASSERT readSize > 0*>
              INC(offset, readSize);
              DEC(size, readSize);
              INC(bytesread, readSize);
            END;
          ELSE 
            BufReadFile(self.vnode, offset, buf, bufStart, bufSize);

            IF Debug THEN
              IO.Put("read_file <- BufReadFile off="&Fmt.Int(offset)&
                ",start="&Fmt.Int(bufStart)&",size="&Fmt.Int(bufSize)&"\n");
            END;
            
            bufSize := MIN(size, bufSize);
            IF bufSize = 0 THEN EXIT; END;
            
            (* Copy BUF[bufStart..] into DATA[BYTESREAD .. ] *)
            SUBARRAY(data^, from+bytesread, bufSize) :=
                SUBARRAY(buf^, bufStart, bufSize);
            INC(offset, bufSize);
            DEC(size, bufSize);
            INC(bytesread, bufSize);
          END;
        END;
      EXCEPT
      | Error.E(e) =>
        bytes := bytesread;

        IF Debug THEN IO.Put("read_file RETURNs "&e.message()&"\n"); END;
        
        CASE e.resultCode() OF 
        | File.FS_NOT_IN_FILE => 
        ELSE

          IF Debug THEN IO.Put("read_file() error: "&e.message()&"\n"); END;
          RAISE Error.E(e);
        END;
      END; (* TRY *)
      IF Debug THEN IO.Put("Read, read "&Fmt.Int(bytes)&"bytes\n"); END;
    END; (* LOCK *)
    RETURN bytesread;
  END ReadRef;

PROCEDURE Open (
    self: FileT;
    <*UNUSED*>mode: INTEGER): File.T
  RAISES { Error.E } <*NOWARN*> =
  VAR
  BEGIN
    (* XXX access check *)
    RETURN self;
  END Open;

PROCEDURE Close (self: FileT) 
  RAISES {Error.E} <*NOWARN*> =
  BEGIN
    LOCK self.mu DO
      self := NIL;
    END;
  END Close;

PROCEDURE Stat (self: FileT; 
		VAR s: FileStat.T) =
  BEGIN
    s.dev     := 0; (*XXX fixme *)
    s.ino     := self.vnode.inumber;
    s.mode    := self.vnode.inode.ic_mode;
    s.nlink   := self.vnode.inode.ic_nlink;
    s.uid     := self.vnode.inode.ic_uid;
    s.gid     := self.vnode.inode.ic_gid;
    s.rdev    := 0;
    s.size    := self.vnode.inode.ic_size.size;
    s.atime   := self.vnode.inode.ic_atime;
    s.mtime   := self.vnode.inode.ic_mtime;
    s.ctime   := self.vnode.inode.ic_ctime;
    s.blksize := BLOCK_SIZE;
    s.blocks  := self.vnode.inode.ic_blocks;
    s.flags   := 0;
    s.gen     := self.vnode.inode.ic_gen;
  END Stat;

PROCEDURE FileRoot (self: FileT): FSRoot.T =
  BEGIN
    RETURN NEW(UFSRoot, mp := self.vnode.mp);
  END FileRoot;

(* ------------------- MOUNT SERVICES ------------------- *)

(* The type "MountT" represents a mount point exported by this file
   system.  The tmp file system does not maintain additional
   information for mount points, consequently is acts just like a
   regular DiretoryT.  A MountT is a subtype of a DirectoryT. *)

TYPE MountT = DirectoryT BRANDED OBJECT
    (* The device comes from some extension which has registered
       itself through Device.Register(). *)
    dev           : Disk.T; 
    (* DEBUG: hold on to the device name that we mounted *)
    devName       : TEXT;
    (* Superblock content *)
    sb            : UfsFs.T;
    (* Cumulative # of indirect blocks at each level. *)
    nindir        : ARRAY [0..UfsFs.NIADDR] OF INTEGER;
  OVERRIDES
    init := MountInit;
  END;

PROCEDURE MountInit(self:MountT): NameServer.T = 
  BEGIN
    self.mu := NEW(MUTEX);
    RETURN DirectoryT.init(self);
  END MountInit;

(* ------------------- FileSystem SERVICES ------------------- *)
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
    
    (* get the device we are mounting *)
    mp.dev := Device.Lookup(NSName.ToText(name));
    mp.dev.open();
    
    (* scribble away the device name for debugging purposes *)
    mp.devName := NSName.ToText(name);

    (* grab the super block *)
    ReadSuperBlock(mp);
    
    (* get the root inode from disk and hold on to it *)
    mp.vnode.inumber := UfsFs.ROOTINO;
    FindInode(mp, mp.vnode.inumber, mp.vnode.inode);
    RETURN mp;
  END Newfs;

PROCEDURE Deletefs (<*UNUSED*>self: FileSystemT; root: Directory.T) =
  VAR
    mp := NARROW(root, MountT);
  BEGIN
    (* go through unmount procedure for ufs. *)
    LOCK mp.mu DO 
      bufferCache.purgeEntries(mp);
    END; (* LOCK *)
  END Deletefs;

(* ------------------- LOW LEVEL FS SERVICES ------------------- *)
VAR 
  bufferCache: Cache.T;
  vnodeCache: Cache.T;
  
PROCEDURE ReadCachedBlock(
    mp : MountT; 
    block: FileDefs.daddr_t;
    size: CARDINAL): REF ARRAY OF CHAR 
  RAISES {Error.E} =
  VAR 
    buf: REF ARRAY OF CHAR;
    nBytesRead: CARDINAL;
  BEGIN
    (* check in the cache for this particular block *)
    buf := bufferCache.find(mp, block);
    IF buf # NIL THEN RETURN buf; END;
    
    IF Debug THEN IO.Put("**cache miss.\n"); END;
    
    buf := NEW(REF ARRAY OF CHAR, size);
    nBytesRead := mp.dev.read(SUBARRAY(buf^, 0, size),
			      UfsFs.fsbtodb(mp.sb, block)*BLOCK_SIZE);
    <*ASSERT nBytesRead = size *>
    bufferCache.insert(buf, mp, block);
    RETURN buf;
  END ReadCachedBlock;

(* "BlockSize" determines the size of a file block in the file system. *)
PROCEDURE BlockSize (
    READONLY vnode: VnodeT; 
    lbn: INTEGER): INTEGER =
  BEGIN
    WITH fs = vnode.mp.sb DO 
      IF (lbn >= UfsFs.NDADDR
	  OR vnode.inode.ic_size.size>=Word.LeftShift(lbn+1, fs.fs_bshift)) THEN
	RETURN fs.fs_bsize;
      ELSE
	RETURN UfsFs.fragroundup(fs, UfsFs.blkoff(fs, vnode.inode.ic_size.size));
      END;
    END;
  END BlockSize;
  
(* Read inode "inumber" from disk and create a new fs object. *)
PROCEDURE FindInode (
    mp      : MountT;
    inumber : FileDefs.ino_t; 
    VAR inode: UfsFs.icommon)
  RAISES {Error.E} =
  CONST
    InodeSize = BYTESIZE(UfsFs.icommon) ;
  VAR
    buf    : REF ARRAY OF CHAR;
    diskBlock: FileDefs.daddr_t;

  BEGIN
    diskBlock := UfsFs.itod(mp.sb, inumber);
    
    IF Debug THEN
      IO.Put("FindInode" & Fmt.Int(inumber)
	     &",block="&Fmt.Int(diskBlock,16) & "bsize="
	     &Fmt.Int(mp.sb.fs_bsize)&".\n");
      IO.Put("dev="& mp.devName
	     &" block#="&Fmt.Int(UfsFs.fsbtodb(mp.sb, diskBlock))&".\n");
    END;
    
    buf := ReadCachedBlock(mp, diskBlock, mp.sb.fs_bsize);
    
    inode := VIEW(SUBARRAY(buf^,
                           InodeSize * UfsFs.itoo(mp.sb, inumber),
                           InodeSize), UfsFs.icommon);
  END FindInode;

(* Given an offset in a file, find the disk block number that contains
   that block.  Returns 0 if block is larger than the file size.  *)
PROCEDURE BlockMap (
    READONLY vnode: VnodeT;
    fileBlock: FileDefs.daddr_t): FileDefs.daddr_t
  RAISES {Error.E} =
  VAR
    level, idx : INTEGER;
    ind_block_num: FileDefs.daddr_t;
  BEGIN
    (*
      Index structure of an inode:
     
      i_db[0..UfsFs.NDADDR-1]    hold block numbers for blocks
                           0..NDADDR-1
     
      i_ib[0]              index block 0 is the single indirect
                           block
                           holds block numbers for blocks
                           NDADDR .. NDADDR + NINDIR(fs)-1
     
      i_ib[1]              index block 1 is the double indirect
                           block
                           holds block numbers for INDEX blocks
                           for blocks
                           NDADDR + NINDIR(fs) ..
                           NDADDR + NINDIR(fs) + NINDIR(fs)**2 - 1
     
      i_ib[2]              index block 2 is the triple indirect
                           block
                           holds block numbers for double-indirect
                           blocks for blocks
                           NDADDR + NINDIR(fs) + NINDIR(fs)**2 ..
                           NDADDR + NINDIR(fs) + NINDIR(fs)**2
                                  + NINDIR(fs)**3 - 1
     *)
  
    IF Debug THEN
      IO.Put("BlockMap() ,fileBlock="&Fmt.Int(fileBlock,16)&"\n");
    END;
  
    IF fileBlock < UfsFs.NDADDR THEN
      (* Direct block. *)
      WITH i = VIEW(vnode.inode.ic_indirect, UfsFs.IndirectBlocks) DO 
	RETURN i.ic_db[fileBlock];
      END;
    END;

    DEC(fileBlock, UfsFs.NDADDR);
    
    (*
      nindir[0] = NINDIR
      nindir[1] = NINDIR**2
      nindir[2] = NINDIR**3
      etc
    *)
    level := 0;
    LOOP
      IF (level >= UfsFs.NIADDR) THEN
	(* Block number too high *)
	IF Debug THEN IO.Put("BlockMap() Block number too high.\n"); END;
	RETURN 0;
      END;
      IF fileBlock < vnode.mp.nindir[level] THEN EXIT; END;
      DEC(fileBlock, vnode.mp.nindir[level]);
      INC(level);
    END;
    
    WITH i = VIEW(vnode.inode.ic_indirect, UfsFs.IndirectBlocks) DO 
      ind_block_num := i.ic_ib[level];
    END;
  
    WHILE (level >= 0) DO
      VAR
	buf: REF ARRAY OF CHAR;
      BEGIN
	
	IF ind_block_num = 0 THEN EXIT; END;
	
	buf := ReadCachedBlock(vnode.mp, ind_block_num, vnode.mp.sb.fs_bsize);
	
	IF level > 0 THEN
	  idx := fileBlock DIV vnode.mp.nindir[level-1];
	  fileBlock := fileBlock MOD vnode.mp.nindir[level-1];
	ELSE
	  idx := fileBlock;
	END;
	ind_block_num := VIEW(SUBARRAY(buf^, idx * ADRSIZE(FileDefs.daddr_t),
				       BYTESIZE(FileDefs.daddr_t)),
			      FileDefs.daddr_t);
      END;
      DEC(level);
    END (* FOR level *);
    
    RETURN ind_block_num;
  END BlockMap;
    

(* "BufReadFile" reads a portion of a vnode into an internal buffer.
   Return the location in the buffer and the amount in the buffer.

   The amount of data read into BUF is no more than block size of the
   file system.
 
   startOffset holds the first index into buf from which data starts,
   and size holds the size of valid data. (Size # BYTESIZE(buf)-startOffset)
   when we reached EOF. *)

PROCEDURE BufReadFile (
    VAR vnode           : VnodeT;
    offset              : FileDefs.vm_offset_t;
    (*OUT*)VAR buf      : REF ARRAY OF CHAR;
    (*OUT*)VAR startOffset: INTEGER;
    (*OUT*)VAR size     : INTEGER) RAISES {Error.E} =
  VAR
    off: FileDefs.vm_offset_t;
    fileBlock: FileDefs.daddr_t;
    diskBlock: FileDefs.daddr_t;
    blockSize: CARDINAL;
  BEGIN
    IF Debug THEN
      IO.Put("bufread: size="&Fmt.Int(vnode.inode.ic_size.size)&"\n");
    END;
  
    IF offset >= vnode.inode.ic_size.size THEN
      buf := NIL;
      size := 0;
      RETURN;
    END;

    off       := UfsFs.blkoff(vnode.mp.sb, offset);
    fileBlock := UfsFs.lblkno(vnode.mp.sb, offset);
    blockSize := BlockSize(vnode, fileBlock);

    IF fileBlock # vnode.bufBlkno THEN
      (* We don't have the requested portion in our cache *)
      
      (* Locate the block# at which the region is held. *)
      diskBlock := BlockMap(vnode, fileBlock);
      
      IF diskBlock = 0 THEN
	IF Debug THEN
	  IO.Put("BufReadFile diskblock = 0 allocating a new empty buffer.\n");
	END;
	(* allocate a new buffer *)
	vnode.buf := NEW(REF ARRAY OF CHAR, blockSize);
	(* Assume buf is initialized to 0 *)
      ELSE
	(* XXX should figure out how many blocks are physically next
	  to each other on the disk and get them all with one request,
	  with a user size constraint.  *)
	IF vnode.buf = NIL OR NUMBER(vnode.buf^) < blockSize THEN
	  vnode.buf := NEW(REF ARRAY OF CHAR, blockSize);
	END;
      
	blockSize := vnode.mp.dev.read(SUBARRAY(vnode.buf^,0,blockSize),
				    UfsFs.fsbtodb(vnode.mp.sb, diskBlock)*BLOCK_SIZE);
      END;
      vnode.bufBlkno := fileBlock;
    END;
    size := MIN(blockSize - off, vnode.inode.ic_size.size - offset);
    <*ASSERT size >= 0 *>
    buf := vnode.buf;
    startOffset := off;
  END BufReadFile;    

(* Read the superblock from the device MP.DEV. Information is returned on
 *  MP.SB.
 *)
PROCEDURE ReadSuperBlock (mp: MountT) RAISES {Error.E} =
  TYPE SuperBlockBuf = REF ARRAY [1..UfsFs.SBSIZE] OF CHAR;
  VAR
    sbBuf: SuperBlockBuf;
    mult: INTEGER;
    sbBufSize: CARDINAL := UfsFs.SBSIZE;
  BEGIN
    IF Debug THEN
      IO.Put("read_fs "& mp.devName &",SBLOCK := "&Fmt.Int(UfsFs.SBLOCK)
	     &", SBSIZE := "&Fmt.Int(UfsFs.SBSIZE)&"\n");
    END;
    
    sbBuf := NEW(SuperBlockBuf);
    sbBufSize := mp.dev.read(sbBuf^, UfsFs.SBLOCK*BLOCK_SIZE);
    mp.sb := VIEW(sbBuf^, UfsFs.T);
    
    WITH sb = mp.sb DO
      IF Debug THEN
        IO.Put("read_fs magic="&Fmt.Int(sb.fs_magic)&
          ",bsize="&Fmt.Int(sb.fs_bsize)&".\n");
      END;
      
      IF (sb.fs_magic # UfsFs.FS_MAGIC 
        OR sb.fs_bsize > FileDefs.MAXBSIZE
        OR sb.fs_bsize < BYTESIZE(UfsFs.T)) THEN
        IF Debug THEN
          IO.Put("read_fs() failed.  fs_magic("&Fmt.Int(sb.fs_magic,16)
          &") fs_bsize("&Fmt.Int(sb.fs_bsize)&")\n");
          IO.Put(" BYTESIZE time_t := "&Fmt.Int(BYTESIZE(FileDefs.time_t))&"\n");
        END;
        sbBuf := NIL;
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_INVALID_FS));
      END;
      
      (* Calculate indirect block levels.  *)
      mult := 1;

      (* XXX Here, I implicitly assume that MULT is 8byte long and
         * no overflow occurs during the multiplication. If MULT is 4 byte long,
         * overflow may occur and this may lead to exception! *)

      FOR level:=0 TO UfsFs.NIADDR-1 DO
        mult := mult * sb.fs_nindir;
        IF Debug THEN IO.Put(Fmt.Int(level)&"=>"&Fmt.Int(mult)); END;
        mp.nindir[level] := mult;
      END;
      IF Debug THEN IO.Put("\n"); END;
      sbBuf := NIL;
    END;
  END ReadSuperBlock;

BEGIN
  TRY
    FileSystem.Register("ufs", NEW(FileSystemT));
  EXCEPT
  | Error.E(e) =>
    IO.Put(e.message()&" during UfsFileSystem initialization.\n");
  END;
  bufferCache := NEW(Cache.T).init(512);
  vnodeCache  := NEW(Cache.T).init(512);
END RofsUfs.

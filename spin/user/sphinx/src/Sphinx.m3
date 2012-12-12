(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Fixed Munmap().
 *
 * 18-Jun-97  Tsutomu Owa (owa) at the University of Washington
 *	fixed minor bugs in Fork() and Waitpid()
 *
 * 13-Jun-97  David Becker at the University of Washington
 *      Change all procs that use File to raise Error.E
 * 20-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added Proc.AllocateMemory
 * 4-oct-96  becker at the University of Washington
 *	Say rupsage is a nop only in Debug mode
 *
 * 26-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Accomodate mef's nameserver change. Stack expansion.
 *
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Excised keyboard control.  Run stuff in the background. Make new
 *	 shells.
 *	Control stuff that way.
 *
 * 6-21-96  becker at the University of Washington
 *	added Fork
 *
 * 11-Jun-96 oystr at the University of Washington
 *	Add file descriptor manipulation.
 *	Changed WaitInternal to use osf args.
 *
 * 29-May-96  Stefan Savage (savage) at the University of Washington
 *	Working SIGSEGV and Mprotect (signals currently require a BIG hack)
 *	Also, fix to Brk to deallocate memory.
 *
 * 28-May-96  becker at the University of Washington
 *	Moved exclusive open from device to dlib
 *
 * 20-May-96 oystr at the University of Washington
 *	Initialize methods at allocation time.  Close routine
 *	now does the right thing.  Added NullSelect.
 *
 * 13-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Socket procs moved to DlibSocket.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	A little more work on signals and a check in Brk
 *	to deal with negative Brk's.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 17-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added basic version of mprotect and signal
 *
 * 14-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed Console read/write functions to have a CARDINAL from
 *	parameter.
 *
 * 14-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE Sphinx EXPORTS Sphinx, SphinxPrivate;
IMPORT IO, Fmt, Text;
IMPORT Spy; <*NOWARN*>
IMPORT Round;
IMPORT OpenFile;
IMPORT StreamFile;
IMPORT Space, Strand, FileSystem;
IMPORT FileStat, Error, Errno, ErrnoDep, NameServer;
IMPORT File;
IMPORT Directory;
IMPORT Translation;
IMPORT Word, CPU;
IMPORT Ctypes;
IMPORT Proc, ProcRep;
IMPORT Auth, SphinxInterface, UserSpaceThread;
IMPORT ExecCmd;
IMPORT ProcQ;
IMPORT BsdWait;
IMPORT BsdSignal;
IMPORT Fcntl AS BsdFcntl;
IMPORT Thread;
IMPORT RefQ;
IMPORT Dispatcher;
IMPORT VMError;
IMPORT VMTypes;
IMPORT InodePager;
IMPORT MemoryObject;
IMPORT Trap;
IMPORT SphinxMachineDep;
IMPORT Protection;
IMPORT StatFs;
IMPORT Symlink AS SymlinkFile;
IMPORT Sig;
IMPORT Clock;
IMPORT NSName;
IMPORT FSRoot;
IMPORT OpenFileQ;
FROM SphinxUtils IMPORT Debug, ProcName, Msg;
IMPORT SphinxUtils;

  
(* See if "path" is contains the device directory "/dev". *)
PROCEDURE DevPath (READONLY path: NameServer.Name): BOOLEAN =
  BEGIN
    IF (path.end - path.from  < 5) THEN
      RETURN FALSE;
    END;
    RETURN path.str[path.from] = '/'
      AND path.str[path.from+1] = 'd'
      AND path.str[path.from+2] = 'e'
      AND path.str[path.from+3] = 'v';
  END DevPath;

(* Get the device name part out of "path". Eg, DevName("/dev/mouse")
   returns "mouse" *)
PROCEDURE DevName (READONLY path: NameServer.Name): TEXT =
  BEGIN
    RETURN Text.FromChars(SUBARRAY(path.str^, path.from+5,
				   path.end-path.from-5));
  END DevName;

VAR statSpy, writeSpy, getpidSpy: Spy.T;
  
PROCEDURE Open (READONLY path: NameServer.Name; mode, createMode: INTEGER)
     : INTEGER RAISES {Errno.E, Error.E, NameServer.Error} =
  VAR
    proc: Proc.T;
    fh: OpenFile.T;
    fd: INTEGER;
  BEGIN
    proc := Translation.GetCurrent();
    fh := OpenInternal(proc, path, mode, createMode);
    LOCK proc.mu DO 
      fd := Proc.AllocateFD(proc, fh);
    END;
    RETURN fd;
  END Open;

(* This is the "namei" equivalent; Lookup "path" and return the file object or
   nameserver object. "proc" is used only when the "path" is relative to
   get the current working dir. "derefSymlink" specifies whether to
   get the stuff pointed to by symlink if the last name component
   of the "path" turns out to be symlink. 
*)
PROCEDURE LookupPath (proc: Proc.T; path: TEXT; derefSymlink := TRUE): REFANY
  RAISES {Errno.E} =
  BEGIN
    TRY 
      RETURN FileSystem.Lookup(proc.cwd, path, NOT derefSymlink);
    EXCEPT
    | NameServer.Error =>
      RAISE Errno.E(ErrnoDep.ENOENT);
    END;
  END LookupPath;
  
PROCEDURE OpenInternal (proc: Proc.T;
			READONLY path: NameServer.Name;
			flags: INTEGER;
			<*UNUSED*>createMode: INTEGER): OpenFile.T
	  RAISES {Errno.E, NameServer.Error, Error.E} =
  VAR
    fh: OpenFile.T;
  BEGIN
    fh := OpenFileQ.Allocate();
    fh.offset := 0;
    fh.refCount := 0; (* incremented in Proc.AllocateFD *)
    fh.flags := flags;
    
    IF Debug THEN
      fh.path := NSName.ToText(path);
    END;
    
    (* "/dev" directory is handled in a beautiful way; we just rely on
       the pathname to see if it's a device name or not. All the
       supported device names are hardcoded here. *)
    IF DevPath(path) THEN
      (* "/dev/foo" kind of path *)
      VAR
	name := DevName(path);
      BEGIN
	IF Text.Equal(name,"console") OR Text.Equal(name,"mouse") THEN
	  WITH h = NEW(StreamFile.T) DO
	    h.dev := FileSystem.Lookup(NIL, "/../svc/devices/" & name);
	    h.dev.open();
	    h.info := StreamFile.InternTty(name);
	    Sig.SetSigHandler(h.dev);
	    fh.h := h;
	    fh.seekable := FALSE;
	  END;	    
	ELSIF Text.Equal(name, "null") THEN
	  fh.h := NEW(StreamFile.Null);
	  fh.seekable := FALSE;
	ELSIF Text.Equal(name, "regress") THEN
	  fh.h := NEW(StreamFile.Text);
	  fh.seekable := FALSE;
	ELSE
	  IF Debug THEN
	    IO.PutError(name & ": unsupported sphinx device.\n");
	  END;
	  RAISE Errno.E(ErrnoDep.ENOENT);
	END;
      END;
    ELSE
      (* Ordinary files *)
      TRY
	fh.h := FileSystem.LookupName(proc.cwd, path);
      EXCEPT
      | NameServer.Error(ec) =>
	IF ec = NameServer.EC.NameNotFound
	   AND Word.And(flags, BsdFcntl.O_CREAT) # 0 THEN
	  (* Create the file on the fly *)
	  VAR
	    dirName, fileName: NSName.T;
	    dir: Directory.T;
	  BEGIN
	    dirName := path;
	    NameServer.GetDirName(dirName, fileName);
	    dir := FileSystem.LookupName(proc.cwd, dirName);
	    fh.h := dir.mkfile(fileName);
	  END;
	ELSE
	  RAISE NameServer.Error(ec);
	END;
      END;
      
      TYPECASE fh.h OF 
      | File.T(file) =>
	fh.h := file.open(flags);
	fh.seekable := TRUE;
	fh.memObj := InodePager.Create(file, NIL, path);
	IF Word.And(flags, BsdFcntl.O_APPEND) # 0 AND fh.seekable THEN
	  VAR stat: FileStat.T;
	  BEGIN
	    file.stat(stat);
	    fh.offset := stat.size;
	  END;
	END;
	IF OpenFile.Register(fh) THEN
	  TRY
	    fh.binding := Dispatcher.InstallHandler(MemoryObject.Destroyed,
						    NIL, MemObjDestroyed,
						    key := fh.memObj);
	  EXCEPT
	  | Dispatcher.Error(ec) =>
	    Msg("mmap", Fmt.Int(ORD(ec)));
	  END;
	END;
      ELSE
	fh.seekable := FALSE; (* XXX (mef) hope this is right. *)
      END;
    END;

    RETURN fh;
  END OpenInternal;

PROCEDURE Close (fd: Word.T): INTEGER RAISES {Errno.E, Error.E} =
  VAR
    proc: Proc.T := Translation.GetCurrent();
    fh: OpenFile.T;
  BEGIN
    LOCK proc.mu DO 
      fh := Proc.FindFH(proc, fd);
      proc.fdTable[fd] := NIL;

      LOCK fh DO
	DEC(fh.refCount);
	IF fh.refCount = 0 THEN 
	  (* XXX there's a race condition between close and other file
             operations. I avoid this by closing the file while
             holding proc.mu lock. Hope this works. *)
          TYPECASE fh.h OF
          | File.T(file) =>
            file.close();
          ELSE
	  END;
	  fh.memObj := NIL;
	  fh.path := NIL;
	  fh.h := NIL;
	  fh.binding := NIL;
	  OpenFileQ.Free(fh);
	END;
      END;
    END;

    RETURN 0;
  END Close;

PROCEDURE Read (fd, addr, size: INTEGER): INTEGER RAISES {Errno.E, VMError.E, Error.E} =
  VAR
    fh: OpenFile.T;
    buf: REF ARRAY OF CHAR;
    nRead: CARDINAL;
    proc: Proc.T;
    from: CARDINAL := 0;
    
  PROCEDURE Callback (<*UNUSED*>pos: CARDINAL; VAR frame: ARRAY OF CHAR) =
    BEGIN
      Translation.Write(proc, frame, addr);
      INC(addr, NUMBER(frame));
    END Callback;
    
  BEGIN
    proc := Translation.GetCurrent();
    IF size <= 0 THEN
      IF size = 0 THEN 
	RETURN 0;
      ELSE
	RAISE Errno.E(ErrnoDep.EINVAL);
      END;
    END;
    
    fh := proc.fdTable[fd];
    IF fh = NIL THEN RAISE Errno.E(ErrnoDep.EBADF); END;

    IF Word.And(fh.flags, 3) = BsdFcntl.O_WRONLY THEN
      RAISE Errno.E(ErrnoDep.EBADF);
    END;
      
    <*ASSERT fh.h # NIL*>
    TYPECASE fh.h OF 
    | File.T(file) =>
      IF fh.seekable THEN
	LOCK fh DO 
	  IF fh.memObj # NIL THEN
	    (* Do cached read on the memory object. *)
	    VAR
	      stat: FileStat.T;
	    BEGIN
	      file.stat(stat);
	      nRead := MIN(size, stat.size - fh.offset);
	      MemoryObject.Access(fh.memObj, fh.offset, nRead, Callback);
	      (* XXX have to deal the case where file expanded in behind *)
	    END;
	  ELSE
	    buf := Proc.AllocateMemory(proc, size);
	    nRead := file.readRef(buf, size, fh.offset, from);
	    Translation.Write(proc, SUBARRAY(buf^, 0, nRead), addr);
	  END;
	  INC(fh.offset, nRead);
	END;
      ELSE
	buf := Proc.AllocateMemory(proc, size);
	nRead := file.readRef(buf, size, 0, from);
	Translation.Write(proc, SUBARRAY(buf^, 0, nRead), addr);
      END;
    ELSE
      IO.PutError("Sphinx.Read reading non-file. contact mef.\n");
      RAISE Errno.E(ErrnoDep.EBADF);
    END;

    Sig.Check(proc);
    RETURN nRead;
  END Read;
  
PROCEDURE Write (fd, addr, size: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E, Error.E} =
  VAR
    fh: OpenFile.T;
    buf: REF ARRAY OF CHAR;
    nWrite: CARDINAL;
    chunk: CARDINAL;
    retain: BOOLEAN;
    orgSize := size;
    proc: Proc.T := Translation.GetCurrent();
  BEGIN
    IF SphinxUtils.MicroBench THEN Spy.Enter(writeSpy); END;
    IF size <= 0 THEN
      IF size = 0 THEN 
	RETURN 0;
      ELSE
	RAISE Errno.E(ErrnoDep.EINVAL);
      END;
    END;
    
    fh := proc.fdTable[fd];
    IF fh = NIL THEN RAISE Errno.E(ErrnoDep.EBADF); END;
    
    IF Word.And(fh.flags, 3) = BsdFcntl.O_RDONLY THEN
      RAISE Errno.E(ErrnoDep.EBADF);
    END;
    
    TYPECASE fh.h OF 
    | File.T(file) =>
      (* Do small I/O at a time, to conserve the traced heap space. *)
      (* XXX need to copy this logic into read. *)
      REPEAT
	chunk := MIN(size, 16_8000); (* Note: 8000 is the max. fast buffer
					size. *)
	buf := Proc.AllocateMemory(proc, chunk);
	Translation.Read(proc, addr, SUBARRAY(buf^, 0, chunk));
	IF fh.seekable THEN
	  LOCK fh DO
	    nWrite := file.writeRef(buf, chunk, fh.offset, 0, retain);
	    INC(fh.offset, nWrite);
	  END;
	ELSE
	  nWrite := file.writeRef(buf, chunk, 0, 0, retain);
	END;
	INC(addr, nWrite);
	DEC(size, nWrite);
	IF retain THEN
	  proc.tmpbuf := NIL;
	END;
      UNTIL size = 0 OR nWrite < chunk;
    ELSE
      IO.PutError("Sphinx.Write writing to non-file. contact mef.\n");
      RAISE Errno.E(ErrnoDep.EBADF);
    END;
    
    Sig.Check(proc);
    IF SphinxUtils.MicroBench THEN Spy.Exit(writeSpy); END;
    RETURN orgSize - size;
  END Write;

TYPE Iovec = RECORD
  base: Word.T;
  len: Ctypes.int;
END;
  
PROCEDURE ReadIovec (proc: Proc.T; ptr: Word.T; n: INTEGER;
		     VAR vec: ARRAY [0..127] OF Iovec): CARDINAL
  RAISES {VMError.E, Errno.E} =
  VAR total: CARDINAL := 0;
  BEGIN
    IF n > LAST(vec) THEN
      IO.Put("{read,write}v:more than 128 iovecs.\n");
      RAISE Errno.E(ErrnoDep.ENOSYS);
    END;
    
    Translation.Read(proc, ptr,
		     VIEW(SUBARRAY(vec, 0, n), ARRAY OF CHAR));
    FOR i := 0 TO n-1 DO
      IF vec[i].len < 0 THEN
	RAISE Errno.E(ErrnoDep.EINVAL);
      END;
      
      INC(total, vec[i].len);
    END;
    RETURN total;
  END ReadIovec;
  
PROCEDURE Writev (fd: INTEGER; ptr: Word.T; n: INTEGER)
  	: INTEGER RAISES {Errno.E, VMError.E, Error.E} =
  VAR
    vec: ARRAY [0 .. 127] OF Iovec;
    proc: Proc.T := Translation.GetCurrent();
    totalInput: CARDINAL;
    totalWritten: CARDINAL;
    buf: REF ARRAY OF CHAR;
    bufIdx: CARDINAL := 0;
    fh: OpenFile.T;
  BEGIN
    totalInput := ReadIovec(proc, ptr, n, vec);
    buf := Proc.AllocateMemory(proc, totalInput);
    FOR i := 0 TO n-1 DO
      Translation.Read(proc, vec[i].base,
		       SUBARRAY(buf^, bufIdx, vec[i].len));
      INC(bufIdx, vec[i].len);
    END;
    <*ASSERT totalInput = bufIdx*>
    
    fh := Proc.FindFH(proc, fd);

    TYPECASE fh.h OF 
    | File.T(file) =>
      IF fh.seekable THEN
	LOCK fh DO
	  totalWritten := file.write(SUBARRAY(buf^, 0, bufIdx), fh.offset);
	  INC(fh.offset, totalWritten);
	END;
      ELSE
	totalWritten := file.write(SUBARRAY(buf^, 0, bufIdx), 0);
      END;
    ELSE
      IO.PutError("Sphinx.Write writing to non-file. contact mef.\n");
      RAISE Errno.E(ErrnoDep.EBADF);
    END;        
    
    RETURN totalWritten;
  END Writev;
  
PROCEDURE Readv (fd: INTEGER; ptr: Word.T; n: INTEGER)
  	: INTEGER RAISES {Errno.E, VMError.E, Error.E} =
  VAR
    vec: ARRAY [0 .. 127] OF Iovec;
    proc: Proc.T := Translation.GetCurrent();
    totalInput: CARDINAL;
    totalRead: CARDINAL;
    buf: REF ARRAY OF CHAR;
    bufIdx: CARDINAL := 0;
    fh: OpenFile.T;
  BEGIN
    totalInput := ReadIovec(proc, ptr, n, vec);
    buf := Proc.AllocateMemory(proc, totalInput);

    fh := Proc.FindFH(proc, fd);
    IF Word.And(fh.flags, 3) = BsdFcntl.O_WRONLY THEN
      RAISE Errno.E(ErrnoDep.EBADF);
    END;

    <*ASSERT fh.h # NIL*>
    TYPECASE fh.h OF 
    | File.T(file) =>
      IF fh.seekable THEN
	LOCK fh DO
	  totalRead := file.read(SUBARRAY(buf^, 0, totalInput),
				 fh.offset);
	  INC(fh.offset, totalRead);
	END;
      ELSE
	totalRead := file.read(SUBARRAY(buf^, 0, totalInput), 0);
      END;
    ELSE
      IO.PutError("Sphinx.Readv reading non-file. contact mef.\n");
      RAISE Errno.E(ErrnoDep.EBADF);
    END;

    FOR i := 0 TO n-1 DO
      Translation.Write(proc, SUBARRAY(buf^, bufIdx, vec[i].len),
			vec[i].base);
      INC(bufIdx, vec[i].len);
      IF bufIdx >= totalRead THEN EXIT; END;
    END;

    RETURN totalRead;
  END Readv;

PROCEDURE Unlink (READONLY path: NSName.T): INTEGER
  RAISES {Errno.E, NameServer.Error} =
  VAR
    dirName, fileName: NSName.T;
    proc: Proc.T := Translation.GetCurrent();
  BEGIN
    dirName := path;
    NameServer.GetDirName(dirName, fileName);
    TYPECASE FileSystem.LookupName(proc.cwd, dirName) OF
    | Directory.T(dir) =>
      dir.detach(fileName);
    ELSE
      RAISE Errno.E(ErrnoDep.ENOTDIR);
    END;
    RETURN 0;
  END Unlink;
  
PROCEDURE StatSub (entry: REFANY; VAR s: FileStat.T) RAISES {Errno.E, Error.E} =
  BEGIN
    TYPECASE entry OF 
    | File.T(file) =>
      file.stat(s);
    | Directory.T(dir) =>
      dir.stat(s);
    | SymlinkFile.T(sym) =>
      sym.stat(s);
    | NameServer.T(dir) =>
      FileStat.Init(s);
      s.size  := dir.size();
      s.mode  := 8_0040777; (* XXX (mef) hack to set directory mode *)
    ELSE
      RAISE Errno.E(ErrnoDep.EBADF);
    END;      
  END StatSub;
  
PROCEDURE Stat (READONLY path: NameServer.Name; VAR s: FileStat.T)
  : INTEGER RAISES {Errno.E, Error.E, NameServer.Error} =
  VAR
    proc: Proc.T := Translation.GetCurrent();
  BEGIN
    IF SphinxUtils.MicroBench THEN Spy.Enter(statSpy); END;
    StatSub(FileSystem.LookupName(proc.cwd, path, FALSE), s);
    IF SphinxUtils.MicroBench THEN Spy.Exit(statSpy); END;
    RETURN 0;
  END Stat;
  
PROCEDURE Fstat (fd: INTEGER; VAR s: FileStat.T): INTEGER RAISES {Errno.E, Error.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T  := Translation.GetCurrent();
  BEGIN
    fh := Proc.FindFH(proc, fd);
    StatSub(fh.h, s);
    RETURN 0;
  END Fstat;

PROCEDURE Lstat (READONLY path: NameServer.Name; VAR s: FileStat.T)
  : INTEGER RAISES {Errno.E, Error.E, NameServer.Error} =
  VAR
    proc: Proc.T := Translation.GetCurrent();
  BEGIN
    StatSub(FileSystem.LookupName(proc.cwd, path, TRUE), s);
    RETURN 0;
  END Lstat;

PROCEDURE Readlink (path: TEXT; buf, size: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E} =
  VAR
    proc: Proc.T := Translation.GetCurrent();
    linkName: TEXT;
    linkArray: REF ARRAY OF CHAR;
    name: NameServer.Name;
  BEGIN
    TYPECASE LookupPath(proc, path, FALSE) OF 
    | NULL =>
      RAISE Errno.E(ErrnoDep.ENOENT);
    | SymlinkFile.T(sym) =>
      (* XXX rewrite *)
      sym.getName(name);
      linkName := NSName.ToText(name);
      WITH len = Text.Length(linkName) DO
	IF len+1 > size THEN
	  RAISE Errno.E(ErrnoDep.ERANGE);
	END;
	linkArray := Proc.AllocateMemory(proc, len+1);
	Text.SetChars(linkArray^, linkName);
	linkArray[len] := '\000'; (* C null terminator. *)
	Translation.Write(proc, SUBARRAY(linkArray^, 0, len+1), buf);
	RETURN len; (* readlink retval doesn't count the eos char. *)
      END;
    ELSE
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
  END Readlink;

PROCEDURE StatfsSub (entry: REFANY; VAR s: StatFs.T)
 : INTEGER RAISES {Errno.E, Error.E} =
  VAR root: FSRoot.T;
  BEGIN
    TYPECASE entry OF 
    | NULL => (* skip *)
      RAISE Errno.E(ErrnoDep.ENOENT);
    | File.T(file) =>
      root := file.root();
    | Directory.T(dir) =>
      root := dir.root();
    ELSE
      RAISE Errno.E(ErrnoDep.EBADF);
    END;
    root.statfs(s);
    RETURN 0;
  END StatfsSub;
  
PROCEDURE Statfs (path: TEXT; VAR s: StatFs.T; <*UNUSED*>len: INTEGER)
 : INTEGER RAISES {Errno.E, Error.E} =
  BEGIN
    RETURN StatfsSub(LookupPath(Translation.GetCurrent(), path), s);
  END Statfs;

PROCEDURE Fstatfs (fd: INTEGER; VAR s: StatFs.T; <*UNUSED*>len: INTEGER)
 : INTEGER RAISES {Errno.E, Error.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T  := Translation.GetCurrent();
  BEGIN
    fh := Proc.FindFH(proc, fd);
    RETURN StatfsSub(fh.h, s);
  END Fstatfs;

PROCEDURE Truncate (path: TEXT; len: INTEGER): INTEGER RAISES {Errno.E, Error.E} =
  VAR entry := LookupPath(Translation.GetCurrent(), path);
  BEGIN
    TYPECASE entry OF 
    | NULL => 
      RAISE Errno.E(ErrnoDep.ENOENT);
    | File.T(file) =>
      file.truncate(len);
    ELSE
      RAISE Errno.E(ErrnoDep.EBADF);
    END;
    RETURN 0;
  END Truncate;
  
PROCEDURE Ftruncate (fd: INTEGER; len: INTEGER): INTEGER RAISES {Errno.E, Error.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T  := Translation.GetCurrent();
  BEGIN
    fh := Proc.FindFH(proc, fd);
    TYPECASE fh.h OF 
    | File.T(file) =>
      file.truncate(len);
    ELSE
      RAISE Errno.E(ErrnoDep.EBADF);
    END;
    RETURN 0;
  END Ftruncate;
  
PROCEDURE Access (path: TEXT; mode: INTEGER): INTEGER RAISES {Errno.E, Error.E} =
  VAR
    s: FileStat.T;
    pname := NSName.FromText(path);
  BEGIN
    (* XXX This is not right!. If "mode" is 0, then we should take
       a special fast path that doesn't require the inode lookup. Ie,
       we need to add the "access" method to the vnode intf.*)
    EVAL Stat(pname, s);
    CASE mode OF
    | 0 => (* F_OK *)
      RETURN 0;
    | 1, 2, 4 => (* {X,W,R}_OK *)
      IF Word.And(s.mode, mode) # 0 THEN
	RETURN 0;
      ELSE
	RAISE Errno.E(ErrnoDep.EACCES);
      END;
    ELSE
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
  END Access;

PROCEDURE Getdirentries (fd, addr, size: INTEGER; VAR basep: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E} =
  VAR
    buf     : REF ARRAY OF CHAR;
    ent: ARRAY [0 .. 64] OF NameServer.Entry;
    nEnt: CARDINAL;
    fh      : OpenFile.T;
    recLen  : CARDINAL;
    proc    : Proc.T   := Translation.GetCurrent();
    bufIdx  : CARDINAL := 0;
  BEGIN
    fh := Proc.FindFH(proc, fd);
    TYPECASE fh.h OF 
    | NameServer.T(dir) =>
      (* allocate temporary space to copy directory entries into. *)
      buf := Proc.AllocateMemory(proc, size);
      LOOP
	nEnt := dir.getEntries(fh.offset, ent);
	FOR i := 0 TO nEnt-1 DO 
	  (* calculate the length of this struct dirent *)
	  WITH nameLen = ent[i].name.end - ent[i].name.from DO 
	    recLen := 4 + 2 + 2 + nameLen + 1;
	    (* round up the record length to 8 *)
	    IF recLen MOD 8 # 0 THEN
	      recLen := (recLen DIV 8 + 1) * 8;
	    END;
	    
	    IF bufIdx + recLen >= size THEN EXIT; END;
	    fh.offset := ent[i].cookie;
	    basep := ent[i].cookie;
	    
	    (* Append the entry to the buf *)
	    
	    VIEW(SUBARRAY(buf^, bufIdx, 4), Ctypes.int) :=
	    VIEW(ent[i].id, Ctypes.int); (* inode num *)
	    VIEW(SUBARRAY(buf^, bufIdx+4, 2), Ctypes.unsigned_short) := recLen;
	    VIEW(SUBARRAY(buf^, bufIdx+6, 2), Ctypes.unsigned_short) := nameLen;
	    SUBARRAY(buf^, bufIdx+8, nameLen) := SUBARRAY(ent[i].name.str^,
							  ent[i].name.from,
							  nameLen);
							 
	    buf[bufIdx+8+nameLen] := '\000';
	    INC(bufIdx, recLen);
	  END;
	END;
	IF bufIdx + recLen >= size THEN EXIT; END;
	IF nEnt < NUMBER(ent) THEN EXIT; END; (* end of dir *)
      END;
      Translation.Write(proc, SUBARRAY(buf^, 0, bufIdx), addr);
    ELSE
      IO.PutError("Sphinx.Getdirentries from non-directory. contact mef.\n");
      RAISE Errno.E(ErrnoDep.EBADF);
    END;
    RETURN bufIdx;
  END Getdirentries;


PROCEDURE Madvise (<*UNUSED*>addr, len, behav: INTEGER): INTEGER =
  BEGIN
    (* just ignore *)
    RETURN 0;
  END Madvise;
  
PROCEDURE Seek (fd: INTEGER; off, whence: INTEGER)
  	: INTEGER RAISES {Errno.E, Error.E} =
  VAR
    curOffset, newOffset: INTEGER;
    proc: Proc.T  := Translation.GetCurrent();
    fh: OpenFile.T;
  BEGIN
    fh := Proc.FindFH(proc, fd);

    IF NOT fh.seekable THEN
      RAISE Errno.E(ErrnoDep.ESPIPE);
    END;
    
    curOffset := fh.offset;
    
    LOCK fh DO 
      CASE whence OF
      | 0 => (* SEEK_SET *)
	newOffset := off;
      | 1 => (* SEEK_CUR *)
	newOffset := fh.offset + off;
      | 2 => (* SEEK_END *)
	VAR
	  stat: FileStat.T;
	BEGIN
	  StatSub(fh.h, stat);
	  newOffset := stat.size - off;
	END;
      ELSE
	RAISE Errno.E(ErrnoDep.EINVAL);
      END;
      fh.offset := newOffset;
    END;
    
    RETURN curOffset;
  END Seek;


(*
 Break ups the procs end of memory point.
 libc translates sbrk and brk into obreak(void *newbreak)

 *)
PROCEDURE Break (newBreak: Word.T): INTEGER RAISES {VMError.E} =
  VAR
    proc: Proc.T;
    breakPage, length: INTEGER;
  BEGIN
    breakPage := Round.UpToPage(newBreak);
    proc := Translation.GetCurrent();
    length := breakPage - proc.break;
    IF length = 0 THEN
      (* nothing to do *)
    ELSIF length > 0 THEN
      (* expand the memory *)
      Space.Allocate(proc, proc.break, length);
    ELSE
      (* shrink *)
      Space.Deallocate(proc, breakPage, -length);
    END;
    proc.break := breakPage;
    RETURN 0;
  END Break;

(*
 Mmap support
 *)

(* This is an event handler installed on MemoryObject.Destroyed event.
   It is called when memory object is no longer used. *)  
PROCEDURE MemObjDestroyed (memObj: MemoryObject.T) =
  VAR
    array: REF ARRAY OF OpenFile.T;
    of: OpenFile.T;
  BEGIN
    array := OpenFile.Find(memObj);
    OpenFile.Delete(memObj);

    (* For all the open file entry that uses the memory object,
       close it. *)
    FOR i := 0 TO LAST(array^) DO
      of := array[i];
      IF of # NIL THEN 
	LOCK of DO
	  TRY
	    (*DEC(of.refCount);*)
	    Dispatcher.Uninstall(of.binding);
	  EXCEPT
	  | Dispatcher.Error(e) =>
	    IO.Put("memobj delete handler: " & Fmt.Int(ORD(e)) & ".\n");
	  END;
	END;
      END;
    END;
  END MemObjDestroyed;
  
PROCEDURE Mmap (addr, len: INTEGER;
		<*UNUSED*>prot: INTEGER;
		flags, fd, off: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T  := Translation.GetCurrent();
    pageLoc: VMTypes.PageNumber;
    anywhere := TRUE;
    mapEnd := addr + len;
    memObj: MemoryObject.T;
  BEGIN
    addr := Round.DownToPage(addr);
    len := Round.UpToPage(mapEnd) - addr;
    
    IF Word.And(flags, MAP_FIXED) # 0 THEN
      anywhere := FALSE;
    END;
    
    IF Word.And(flags, MAP_ANONYMOUS) # 0 THEN
      memObj := NEW(MemoryObject.T).init(len DIV CPU.PAGESIZE);
    ELSE
      fh := Proc.FindFH(proc, fd);
      memObj := fh.memObj;
      IF memObj = NIL THEN
	(* you can't map a non-file *)
	RAISE Errno.E(ErrnoDep.EBADF);
      END;
      
      (* Note: In POSIX, the file reference count must be incremented
	 here so that the close() call don't deallocate the open file
	 entry. We rely on GC to do the same thing. *)
      
      INC(fh.refCount);
      IF Word.And(flags, MAP_PRIVATE) # 0 THEN
	(* We do cow only when we are asked to map file privately.
	   If MAP_ANONYMOUS, we ignore the MAP_PRIVATE flag. *)
	memObj := memObj.copyOnWrite();
      END; 
    END;
    
    pageLoc := addr DIV CPU.PAGESIZE;
    IF pageLoc = 0 THEN
      (* The return value "0" means failure in mmap. So we have to make sure
	 the memobject is not mapped at the virt address 0. *)
      pageLoc := 16_10000;
    END;
    proc.allocate(pageLoc, len DIV CPU.PAGESIZE, anywhere);
    
    proc.map(pageLoc, len DIV CPU.PAGESIZE, memObj,
		   off DIV CPU.PAGESIZE, TRUE);
    
    IF Debug THEN
      VAR name: TEXT;
      BEGIN
	IF fh # NIL THEN
	  name := fh.path;
	ELSE
	  name := "anonymous";
	END;
	Msg("mmap: ", name& "@" & Fmt.Int(pageLoc*CPU.PAGESIZE, 16));
      END;
    END;
    RETURN pageLoc*CPU.PAGESIZE;
  END Mmap;

PROCEDURE Munmap (addr, len: INTEGER): INTEGER  RAISES {VMError.E} =
  VAR proc: Proc.T  := Translation.GetCurrent();
      mapEnd := addr + len;
  BEGIN
    addr := Round.DownToPage(addr);
    len := Round.UpToPage(mapEnd) - addr;
    proc.deallocate(addr DIV CPU.PAGESIZE,
		    len DIV CPU.PAGESIZE);
    RETURN 0;
  END Munmap;
  
PROCEDURE Mprotect (addr: INTEGER; size: INTEGER; iprot: INTEGER)
  	: INTEGER RAISES {VMError.E} =
  VAR
    proc: Proc.T  := Translation.GetCurrent();
    prot: Protection.T;
    read, write, execute: BOOLEAN;
  BEGIN
    read := Word.And(iprot, 1) # 0; (* PROT_READ *)
    write := Word.And(iprot, 2) # 0; (* PROT_WRITE *)
    execute := Word.And(iprot, 4) # 0;  (* PROT_EXECUTE *)
    
    prot := Protection.T{read, write, execute, 0};
    Translation.ChangeMapping(proc, addr DIV CPU.PAGESIZE,
			      (addr+size-1) DIV CPU.PAGESIZE+1, prot);
    (* XXX this is not right: the protection info must be remembered even
       after subsequest page faults. *)
    
    RETURN 0;
  END Mprotect;

PROCEDURE Exit (status: INTEGER) =
  VAR
    proc: Proc.T  := Translation.GetCurrent();
  BEGIN
    ExitInternal(proc, status);
  END Exit;

PROCEDURE ExitInternal (proc: Proc.T; exitCode: INTEGER) =
  VAR 
    fh: OpenFile.T;
    th := proc.thread;
  BEGIN
    <*ASSERT proc = Translation.GetCurrent()*>
    
    Thread.Acquire(proc.mu);
    
    (* First, wake up the bound kernel thread in case it's suspended. *)
    proc.state := Proc.State.Zombie;
      
    (*
       Close all files, particularly sockets, so
       that we can re-use the addresses.
    *)
    FOR i := FIRST(proc.fdTable) TO LAST(proc.fdTable) DO
      IF proc.fdTable[i] # NIL THEN
	fh := proc.fdTable[i];
	proc.fdTable[i] := NIL;
	
	LOCK fh DO
	  TRY 
	    DEC(fh.refCount);
	    IF fh.refCount = 0 THEN 
	      TYPECASE fh.h OF
	      | NULL =>
		(* skip *)
	      | File.T(file) =>
		file.close();
	      ELSE
		(* skip *)
	      END;
	    END;
	  EXCEPT
	  | Error.E(e) =>
	    IO.Put("sphinx exit: failed to close, " & e.message() & ".\n");
	  END;
	END;
      END;
    END;
      
    proc.exit := BsdWait.Compose(0, exitCode);
    (* XXX the use of bsdwait is funny??? *)
    IF proc.parent # NIL THEN
      (* Rechain the process from active list to
	 other list in the parent PCB, so that the parent can
	 know my status has changed. *)
      Proc.AddToWaitList(proc);
      (* Notify the parent about my death *)
      KillInternal(proc.parent, BsdSignal.CHLD);
    ELSE
      (* I'm solitary, ie, I was started using "sphinx exec" *)
      (* remove from the tables myself, because no one will wait for me. *)
      Proc.Destroy(proc);
      
      (* If I have children, make it solitary.
	 XXX This somehow mimics the UNIX behavior where lost children are
	 adopted by the init process. *)
      VAR
	child: Proc.T;
      BEGIN
	WHILE NOT ProcQ.Empty(proc.activeChildren) DO
	  child := proc.activeChildren.next;
	  (* Avoid deadlock. We always lock in child->parent order,
	     not other way round. This means that we unlock parent.
	     Note that all the fields in "proc" are consistent, so
	     there is no danger in unlocking "proc" here. *)
	  Thread.Release(proc.mu);
	  Thread.Acquire(child.mu);
	  Thread.Acquire(proc.mu);
	  
	  ProcQ.Remove(child); (* unlink from the queue *)
	  child.parent := NIL;
	  IF Debug THEN
	    Msg("exit: " & ProcName(child) & " is made orphan.\n");
	  END;
	  Thread.Release(child.mu);
	  (* "proc" is still locked. *)
	END;
	
	WHILE NOT ProcQ.Empty(proc.otherChildren) DO
	  child := proc.otherChildren.next;
	  IF Debug THEN
	    Msg("exit: " & ProcName(child) & "* is made orphan.\n");
	  END;
	  Thread.Release(proc.mu);
	  Thread.Acquire(child.mu);
	  Thread.Acquire(proc.mu);
	  
	  CASE BsdWait.StatusCode(child.exit) OF
	  | BsdWait.WSTOPPED_, BsdWait.WCONTINUED_ =>
	    (* Rechain the child to the active list *)
	    child.parent := NIL;
	    ProcQ.Remove(child);
	  ELSE
	    (* Child is dead. remove from the tables.
	       Here, we first unchain child from the chain.
	       This is to avoid deadlocking in Destroy which tries to
	       lock the parent when it unchains the process.  *)
	    child.parent := NIL;
	    ProcQ.Remove(child);
	    Proc.Destroy(child);
	  END;

	  Thread.Release(child.mu);
	END;
      END;
      
      (* The spin shell that invoked the process may be waiting.
	 Wake him up. *)
      ExecCmd.Signal();
    END;
      
    (* Finally, destroy the user space thread and space *)
    proc.destroy();
    (*UserSpaceThread.Destroy(th);*)
    Strand.Block(th);

    Thread.Release(proc.mu);
  END ExitInternal;
  
PROCEDURE Gettimeofday (tv: INTEGER; tz: INTEGER) 
	RAISES { VMError.E} =
  VAR
    space := Translation.GetCurrent();
    ltv : Clock.TimeVal;
  BEGIN
    IF tv # 0 THEN
      Clock.TimeOfDay(ltv);
      Translation.Write(space, VIEW(ltv, ARRAY OF CHAR), tv);
    END;
    IF tz # 0 THEN
      (* An ancient tradition: recompile the kernel to change timezone *)
      ltv.tv_sec := 480;
      ltv.tv_usec := 1;
      Translation.Write(space, VIEW(ltv,ARRAY OF CHAR), tz);
    END;
  END Gettimeofday;

PROCEDURE Waitpid (pid: INTEGER; stateAddr: INTEGER; opt: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E} =
  VAR
    proc: Proc.T  := Translation.GetCurrent();
    child: Proc.T;
    r: ProcQ.T;
    rc: INTEGER;
    itr: ProcQ.Iterator;
  BEGIN
    (* Make sure that the specified process is really a child of the
     caller. *)
    IF pid # -1 THEN 
      child := Proc.FindFromID(pid);
      IF child.parent # proc THEN
	RAISE Errno.E(ErrnoDep.ECHILD);
      END;
    END;

    (* Look for the child proc, and sleep if there isn't any *)
    LOOP
      child := NIL;
      LOCK proc.mu DO
	IF pid # -1 THEN
	  (* look for a process named "pid" *)
	  itr := ProcQ.Iterate(proc.otherChildren);
	  WHILE ProcQ.NextItr(itr, r) DO
	    IF NARROW(r, Proc.T).pid = pid THEN
	      child := r;
	      EXIT;
	    END;
	  END;
	ELSE
	  (* any process is ok *)
	  IF NOT ProcQ.Empty(proc.otherChildren) THEN
	    child := proc.otherChildren.next;
	  END;
	END;

	IF child # NIL THEN
	  EXIT;
	END;
	
	(* No child is other now *)
	IF Word.And(opt, BsdWait.WNOHANG) # 0 THEN RETURN 0; END;
	Thread.Wait(proc.mu, proc.cond);
      END;
    END;

    rc := child.pid;
    
    IF stateAddr # 0 THEN
      Translation.Write(proc, VIEW(child.exit, ARRAY OF CHAR), stateAddr);
    END;

    IF Debug THEN
      Msg("waitpid: removing ", Fmt.Int(child.pid));
    END;
      
    IF Word.And(opt, BsdWait.WNOWAIT) # 0 THEN
      (* keep it *)
      Sig.Check(proc);
      RETURN rc;
    END;
      
    CASE BsdWait.StatusCode(child.exit) OF
    | BsdWait.WSTOPPED_, BsdWait.WCONTINUED_ =>
      (* Rechain the child to the active list *)
      LOCK proc.mu DO
	ProcQ.Remove(child);
	ProcQ.InsertTail(proc.activeChildren, child);
      END;
    ELSE
      (* Child is dead. remove from the tables *)
      Proc.Destroy(child);
    END;
    Sig.Check(proc);
    RETURN rc;
  END Waitpid;

PROCEDURE Fork (<*UNUSED*>us: Strand.T; VAR s: CPU.SavedState)
  RAISES {VMError.E} =
  VAR
    proc: Proc.T  := Translation.GetCurrent();
    parentThread := UserSpaceThread.Self();
    newThread: UserSpaceThread.T;
    newProc: Proc.T;
    parentState, childState: UserSpaceThread.State;
  BEGIN
    newProc := Proc.Create(proc.print()&"@", proc);
    Space.Duplicate(newProc, proc);
    newThread := UserSpaceThread.Create(newProc);
    newProc.thread := newThread;
    
    parentState := NEW(UserSpaceThread.State);
    parentState.cpustate := NEW(REF CPU.GeneralRegs);
    parentState.fpustate := NEW(REF CPU.FloatRegs);
    UserSpaceThread.GetState(parentThread, parentState);

    childState := NEW(UserSpaceThread.State);
    childState.cpustate := NEW(REF CPU.GeneralRegs);
    childState.fpustate := NEW(REF CPU.FloatRegs);
    childState.cpustate^ := parentState.cpustate^;
    childState.fpustate^ := parentState.fpustate^;

    LOCK newProc.mu DO
      (* Copy file descriptors. *)
      FOR i := FIRST(proc.fdTable) TO LAST(proc.fdTable) DO
	WITH of = proc.fdTable[i] DO
	  IF of # NIL THEN
	    LOCK of DO
	      Proc.AllocateFDatSlot(newProc, of, i);
	    END;
	  END;
	END;
      END;
      newProc.closeOnExec := proc.closeOnExec;
      
      (* Copy cwd *)
      newProc.cwd := proc.cwd;

      newProc.break := proc.break;
      newProc.stackTop := proc.stackTop;
      newProc.sigMask := proc.sigMask;  (* XXX Should I copy those? *)
      newProc.sig := proc.sig;          (*     Should I copy those? *)
    END;
    
    SphinxMachineDep.ForkReturn(proc.pid, newProc.pid, s, childState.cpustate^);
    UserSpaceThread.SetState(newThread, childState);
    Proc.InstallStandardHandlers(newProc);
    
    UserSpaceThread.Resume(newThread);

    IF Debug THEN
      Msg("forked the proc ", ProcName(newProc));
    END;
  END Fork;

PROCEDURE Vfork(us: Strand.T; VAR s: CPU.SavedState)
  RAISES {VMError.E}=
  BEGIN
    Fork(us, s);
  END Vfork;

PROCEDURE Chdir (path: TEXT): INTEGER RAISES {Errno.E} =
  VAR
    proc: Proc.T  := Translation.GetCurrent();
    dir: REFANY;
  BEGIN
    dir := LookupPath(proc, path);
    IF dir # NIL AND ISTYPE(dir, NameServer.T) THEN
      proc.cwd := dir;
      RETURN 0;
    END;
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Chdir;
  
PROCEDURE Fchdir (fd: INTEGER) : INTEGER RAISES {Errno.E} =
  VAR
    of: OpenFile.T;
    proc: Proc.T  := Translation.GetCurrent();
  BEGIN
    LOCK proc.mu DO
      of := Proc.FindFH(proc, fd);
    END;
    LOCK of DO 
      TYPECASE of.h OF 
      | NULL => RAISE Errno.E(ErrnoDep.EBADF);
      | NameServer.T(dir) =>
	proc.cwd := dir;
      ELSE
	RAISE Errno.E(ErrnoDep.EBADF);
      END;
    END;
    RETURN 0;
  END Fchdir;

(*
 
 PID related system calls.
 
 *)
PROCEDURE Getpid (<*UNUSED*>us: Strand.T; VAR ss: CPU.SavedState) =
  VAR
    proc: Proc.T;
    ppid := -1;
  BEGIN
    IF SphinxUtils.MicroBench THEN Spy.Enter(getpidSpy); END;    
    proc := Translation.GetCurrent();
    IF proc.parent # NIL THEN 
      ppid := proc.parent.pid;
    END;
    SphinxMachineDep.GetpidReturn(ss, proc.pid, ppid);
    IF SphinxUtils.MicroBench THEN Spy.Exit(getpidSpy); END;    
  END Getpid;

PROCEDURE Getpgrp(): INTEGER =
  VAR proc: Proc.T := Translation.GetCurrent();
  BEGIN
    RETURN proc.grp.gid;
  END Getpgrp;

PROCEDURE Setsid (): INTEGER RAISES {Errno.E} =
  VAR proc: Proc.T  := Translation.GetCurrent();
  BEGIN
    (* XXX We have no notion of session right now. Setsid is same as
       making caller the group leader. *)
    Setpgid(proc.pid, proc.pid);
    RETURN proc.pid;
  END Setsid;
  
PROCEDURE Setpgid (pid, pgid: INTEGER) RAISES {Errno.E} =
  VAR
    proc := Proc.FindFromID(pid);
    grp: Proc.Group;
  BEGIN
    IF proc = NIL THEN
      RAISE Errno.E(ErrnoDep.ESRCH);
    END;

    IF pgid = proc.grp.gid THEN RETURN; END;

    LOCK proc.mu DO 
      (* XXX has to check if PID is child of PROC *)
      
      (* XXX restriction: you can't change the group id if
	 you are currently the group leader and you have some
	 members in the group other than you. *)

      IF proc.grp.gid = proc.pid AND NOT RefQ.Empty(proc.grp.members) THEN
	(* XXX If I'm a group leader, I can't change my group *)
	RAISE Errno.E(ErrnoDep.ESRCH);
      END;
      
      Proc.RemoveFromCurrentGroup(proc);
      
      IF pgid = proc.pid THEN
	(* I am the group reader from now on *)
	grp := NEW(Proc.Group, mu := NEW(MUTEX), gid := proc.pid,
		   members := RefQ.NewHeader());
	Proc.AddToGroup(proc, grp);
      ELSE
	VAR grpLeader := Proc.FindFromID(pgid);
	BEGIN
	  (*
	   The above FindFromID raises an exception if
	   the group leader does not exist.
	   XXX this is probably not the UNIX semantics.
	   *)
	  
	  IF proc.grp.gid = proc.pid
	    AND NOT RefQ.Empty(proc.grp.members) THEN
	    (* XXX If I'm a group leader and I have members within my
	     group, I can't change my group *)
            RAISE Errno.E(20010);
	  END;
	  
	  grp := grpLeader.grp;
	  Proc.AddToGroup(proc, grp);
	END;
      END;
    END;
  END Setpgid;

<*UNUSED*>
CONST
  RLIMIT_CPU	=0;
  RLIMIT_FSIZE	=1;
  RLIMIT_DATA	=2;
  RLIMIT_STACK	=3;
  RLIMIT_CORE	=4;
  RLIMIT_RSS	=5;
CONST
  RLIMIT_NOFILE	=6;
  RLIMIT_AS	=7;
<*UNUSED*>
CONST
  RLIMIT_VMEM	=RLIMIT_AS;
CONST
  RLIM_INFINITY	=LAST(INTEGER);
  
PROCEDURE Getdtablesize (): INTEGER =
  BEGIN
    RETURN ProcRep.MAX_NOFILE;
  END Getdtablesize;
  
PROCEDURE Setrlimit (<*UNUSED*>type: INTEGER;
		     <*UNUSED*>VAR l: Rlimit): INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("setrlimit: not implemented.\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Setrlimit;
  
PROCEDURE Getrlimit (type: INTEGER; VAR l: Rlimit): INTEGER =
  BEGIN
    CASE type OF
    | RLIMIT_NOFILE =>
      l.cur := ProcRep.MAX_NOFILE;
      l.max := ProcRep.MAX_NOFILE;
    ELSE
      l.cur := RLIM_INFINITY;
      l.max := RLIM_INFINITY;
    END;
    RETURN 0;
  END Getrlimit;

PROCEDURE Getrusage (who: INTEGER; <*UNUSED*>VAR r: Rusage): INTEGER
  RAISES {Errno.E} =
  BEGIN
    CASE who OF
    | 0, -1 => (* RUSAGE_SELF / RUSAGE_CHILDREN *)
      (* XXX. *)
      IF Debug THEN
        IO.Put("getrusage is nop now.\n");
      END;
    ELSE
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    RETURN 0;
  END Getrusage;
  
(*
 
 Event Handlers.

*)

(* This page fault handler handles only faults on stack segments.
   All other faults should be handled by generic AddressSpace faults.

   We guess whether the fault is on stack by comparing the fault address with
   the current stack top. If the difference between
   two is "small", it is a stack fault.

*)
PROCEDURE PageFault (<*UNUSED*>strand: Strand.T; 
		     <*UNUSED*>VAR ss: CPU.SavedState;
		     map: Translation.T;		    
		     addr: CPU.VirtAddress;
		     <*UNUSED*>type: Word.T;
		     VAR done: BOOLEAN) =
  VAR
    proc: Proc.T;
  BEGIN
    IF done THEN RETURN; END;
    
    proc := Translation.GetCurrent();
    
    IF proc = NIL THEN
      (* This happens when the process is killed by a handler called before *)
      RETURN;
    END;
    
    <*ASSERT map = proc*>
    IF addr >= proc.stackTop OR addr < proc.stackTop - 16_20000 THEN
      (* out of stack seg. *)
      RETURN;
    END;

    Msg("sphinx page fault@", Fmt.Int(addr, 16));

    addr := Round.DownToPage(addr);
    TRY
      Space.Allocate(proc, addr, proc.stackTop - addr);
      proc.stackTop := addr;
      done := TRUE;
    EXCEPT
    | VMError.E(e) =>
      Msg("stack fault handler: ", Fmt.Int(e));
    END;
  END PageFault;
  
  
(* "UnhandledException" is called when a thread died abnormally. *)
PROCEDURE UnhandledException (s: Strand.T;
			      <*UNUSED*>VAR ss: CPU.SavedState) =
  VAR
    proc: Proc.T;
  BEGIN
    IF NOT ISTYPE(s, UserSpaceThread.T) THEN
      IO.Put("sphinx:not UST???\n");
      RETURN;
    END;
    
    proc := UserSpaceThread.GetSpace(s);

    IF proc = NIL THEN
      IO.Put("sphinx:this one is not sphinx process.\n");
      RETURN;
    END;
    IO.Put("sphinx: proc \"" & proc.print() & "\" died.\n"); 
    ExitInternal(proc, 33);
  END UnhandledException;

PROCEDURE Usleep (newp, oldp: INTEGER): INTEGER RAISES {VMError.E} =
  VAR  
    proc: Proc.T := Translation.GetCurrent();
    tv, otv : Clock.TimeVal;
  BEGIN
    IF newp = 0 THEN RETURN 0; END;
    IF newp # 0 THEN
      Translation.Read(proc, newp, VIEW(tv,ARRAY OF CHAR));
      Thread.Pause(tv.tv_sec * 1000000 + tv.tv_usec);
      IF oldp # 0 THEN 
	otv.tv_sec := 0;
	otv.tv_usec := 0;
	Translation.Write(proc, VIEW(otv,ARRAY OF CHAR), oldp);
      END;
    END;
    RETURN 0;
  END Usleep;

CONST (* defined in include/sys/time.h *)
  ITIMER_REAL = 0;
  ITIMER_VIRTUAL = 1;	<*NOWARN*>
  ITIMER_PROF = 2;	<*NOWARN*>

PROCEDURE ItimerExpired(arg : REFANY) =
  VAR
    proc := NARROW(arg, Proc.T);
    ticks, clockRate : CARDINAL;
  BEGIN
    IO.Put("ItimerExpired called\n");

    ticks := 1000000 * proc.timer.interval.tv_sec + proc.timer.interval.tv_usec;
    IF ticks > 0 THEN
      (* reload alarm *)
      clockRate := Clock.InterruptRate();
      IF clockRate = 0 THEN clockRate := 100; END;
      ticks := ticks * clockRate DIV 1000000;
      Clock.SetAlarm(ticks, ItimerExpired, proc);
    ELSE
      proc.timer.interval.tv_sec := 0;
      proc.timer.interval.tv_usec := 0;
    END;

    (* send SIGALRM to the proc *)
    BsdSignal.AddSet(proc.sig, BsdSignal.ALRM);
    (* The syscall handler for this proc may be waiting on
       sigsuspend. Wake her up *)
    Thread.Signal(proc.cond);
  END ItimerExpired;

(* XXX. this is not accurate!  BSD's setitimer is based on a real timer *)
PROCEDURE Setitimer(which : INTEGER;
		    VAR value : ItimerVal;
		    ovalue : INTEGER)
	: INTEGER RAISES {Errno.E} =
  VAR
    self : Proc.T := Translation.GetCurrent();
    ticks : CARDINAL;
    clockRate := Clock.InterruptRate();
    retVal : ItimerVal;
  BEGIN
    (* XXX. support only ITIMER_REAL *)
    IF which # ITIMER_REAL THEN
      IO.Put("Getitimer:: ITIMER_VIRTUAL, ITIMER_PROF not supported\n");
      RAISE Errno.E(ErrnoDep.ENOSYS);
    END;

    IF ovalue # 0 THEN				(* need to return old value *)
      EVAL Getitimer(which, retVal);		(* save old value *)
    END;

    (* XXX, does passing self ensure to cancel the alarm previously set? *)
    EVAL Clock.CancelAlarm(ItimerExpired, self);

    (* convert sec and usec to ticks *)
    (* XXX. Workaround for InterruptRate problem on PC *)
    IF clockRate = 0 THEN clockRate := 100; END;
    ticks := (1000000 * value.value.tv_sec + value.value.tv_usec);
    ticks := ticks * clockRate DIV 1000000;
    self.timer := value;
    IF ticks > 0 THEN
      Clock.SetAlarm(ticks, ItimerExpired, self);
    END;

    IF ovalue # 0 THEN				(* copy old value *)
      Translation.Write(self, VIEW(retVal, ARRAY OF CHAR), ovalue);
    END;
    
    RETURN 0;
  END Setitimer;

PROCEDURE Getitimer(which : INTEGER;
		    VAR value : ItimerVal)
	 : INTEGER RAISES {Errno.E} =
  VAR
    self : Proc.T := Translation.GetCurrent();
  BEGIN
    (* XXX. support only ITIMER_REAL *)
    IF which # ITIMER_REAL THEN
      IO.Put("Getitimer:: ITIMER_VIRTUAL, ITIMER_PROF not supported\n");
      RAISE Errno.E(ErrnoDep.ENOSYS);
    END;

    (* do I have to lock mu and/or call splclock()? *)

    value := self.timer;
    RETURN 0;
  END Getitimer;

(* XXX.  X86 only.  Better move into IX86_SPIN/XXX *)
PROCEDURE Syscall_(<*UNUSED*>us : Strand.T; VAR state : CPU.SavedState)
	RAISES {Errno.E} =
  VAR
    code, ret: INTEGER;
    proc : Proc.T := Translation.GetCurrent();
    arg0: Word.T;
  BEGIN
    (* get code first *)
    Translation.Read(proc, state.usp+4, VIEW(arg0, ARRAY OF CHAR));
    code := arg0;

    CASE code OF
    | 197 =>		(* ok it's Mmap request *)
      (* /spin/archive/freebsd/2.1src/lib/libc/sys/mmap.c *)
      VAR
	args_: ARRAY [0..8] OF Word.T;
	addr, len, prot, flags, fh, off: INTEGER;
      BEGIN
        Translation.Read(proc, state.usp+4, VIEW(args_, ARRAY OF CHAR));
	addr:= args_[2]; len:= args_[3]; prot:= args_[4]; 
	flags := args_[5]; fh:= args_[6]; off:= args_[8]; 
        ret := Mmap(addr, len, prot, flags, fh, off);
      END;
    | 199 =>
      (* /spin/archive/freebsd/2.1src/lib/libc/sys/lseek.c *)
      IO.Put("skip syscall: " & Fmt.Int(code) & ". lseek.\n");
      RAISE Errno.E(ErrnoDep.ENOTSUP);
    | 200 =>
      (* /spin/archive/freebsd/2.1src/lib/libc/sys/truncate.c *)
      IO.Put("skip syscall: " & Fmt.Int(code) & ". truncate.\n");
      RAISE Errno.E(ErrnoDep.ENOTSUP);
    | 201 =>
      (* /spin/archive/freebsd/2.1src/lib/libc/sys/ftruncate.c *)
      IO.Put("skip syscall: " & Fmt.Int(code) & ". ftruncate.\n");
      RAISE Errno.E(ErrnoDep.ENOTSUP);
    ELSE
      IO.Put("skip syscall: " & Fmt.Int(code) & ".\n");
      RAISE Errno.E(ErrnoDep.ENOTSUP);
    END;

    SphinxMachineDep.Syscall_Return(state, ret);
  END Syscall_;

BEGIN
  TRY
    EVAL SphinxInterface.Export(NEW(Auth.AuthAlways));
    (*Dispatcher.InstallAuthorizerForInterface(NEW(Auth.T,
						 authorize := Authorize),
					     INTERFACE_UNIT(Sphinx),
					     THIS_MODULE()); *)
    EVAL Dispatcher.InstallHandler(Trap.UnhandledUserSpaceException,
				   NIL, UnhandledException,
   	       options := Dispatcher.Options{Dispatcher.Opt.First,
					     Dispatcher.Opt.Cancel
					     });
    IO.Put("Sphinx handler extension ready.\n");
  EXCEPT
  | Dispatcher.Error(ec) =>
    IO.Put("sphinx init: dispatcher error(" & Fmt.Int(ORD(ec)) & ".\n");
  END;
  IF SphinxUtils.MicroBench THEN
    writeSpy := Spy.Create("sphinx-write", TRUE, 400);
    statSpy := Spy.Create("sphinx-stat", TRUE, 400);
    getpidSpy := Spy.Create("sphinx-getpid", TRUE, 400);
  END;
END Sphinx.

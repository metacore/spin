(*
 * copyrights 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NameServer.Name representation
 * 20-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Fixed a bug in CP()
 *
 * 06-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Cleanup. Also, changed not to skip entry even if the value is NIL.
 *	This is to cope with the semantics change in iterate.next();
 *	it now does lookup for each name.
 * 30-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to list nameserver aliases and object.
 *
 * 22-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use new nameserver interface.
 *)

(* Filesystem commands *)
MODULE FsCommands;

IMPORT Directory, Error, File, FileStat, FileSystem, FSRoot, Fmt, Glob, IO,
       NameServer, ParseParams, RefList, RefListSort, Shell, Text,
       Word, Commands, DefaultPager;
IMPORT Symlink;
IMPORT NSName;
CONST ScreenWidth = 78;

PROCEDURE CWD(<*UNUSED*>path: TEXT := NIL):NameServer.T = 
  VAR 
    cwd: TEXT;
    entry: REFANY;
  BEGIN
    cwd := Glob.GetVariable(Shell.Vars(),"cwd");
    IF cwd = NIL THEN
      (* $%#^&*! someone forgot to set 'cwd' at boot *)
      cwd := "/";
      Glob.SetVariable(Shell.Vars(),"cwd",cwd);
    END;

    TRY
      (* get the handle to the current working dir *)
      entry := FileSystem.Lookup(NIL, cwd);
      TYPECASE entry OF 
      | NameServer.T(dir) =>
	RETURN dir;
      ELSE
	RAISE NameServer.Error(NameServer.EC.Panic);
      END;
    EXCEPT
    | NameServer.Error =>
      IO.PutError(Fmt.F("cwd: %s : no such directory. using /\n", cwd));
      cwd := "/";
      Glob.SetVariable(Shell.Vars(), "cwd", cwd);
      RETURN FileSystem.GetRoot();
    END;
  END CWD;

(* that's the fucking implementation of TRY!!!  it does setjmp which
 * freezes the value of "path" to be NIL, then sets path to some value
 * and then longjmp wipes it out back to NIL!!!!
 *)
PROCEDURE Mkdir (closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    path: TEXT;
    dir: NameServer.T;
    dirName, lastName: NSName.T;
    ok: BOOLEAN :=TRUE;

     (* FIXME: this is an awful hack to avoid the value of path to
	be wiped out by longjmp when the NameServer exception is raised.
	It should be a single TRY statement instead. *)
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      path := pp.getNext();
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(closure);
      ok := FALSE;
    END;

    IF ok THEN
      TRY
        dirName := NSName.FromText(path);
        NameServer.GetDirName(dirName, lastName);

        dir := FileSystem.LookupName(CWD(), dirName);
        EVAL dir.create(lastName);
      EXCEPT
      | NameServer.Error => 
        IO.PutError("mkdir "&path&" failed.  No such dir: "
        		&NSName.ToText(dirName) & "\n");
      END;
    END;
    RETURN TRUE;
  END Mkdir;

PROCEDURE Newfs(<*UNUSED*>closure: REFANY;
		<*UNUSED*>pp: ParseParams.T): BOOLEAN =
  BEGIN
    IO.PutError("newfs: this command is obsolete.\n");
    RETURN TRUE;
  END Newfs;

PROCEDURE Sync(closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    path: TEXT;
    entry: REFANY;
    dirName: NSName.T;
    root: FSRoot.T;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();		(* skip self *)
      path := pp.getNext();
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(closure);
    END;
   
    TRY
      dirName := NSName.FromText(path);
      IF dirName.str[0] # '/' THEN
	IO.PutError("path should be absolute path.\n");
	RETURN TRUE;
      END;
      entry := FileSystem.LookupName(NIL, dirName);
      TYPECASE entry OF
      | File.T(file) =>
        root := file.root();
        root.sync();
      ELSE
	IO.Put("mkdir absolute_path(filename)\n");
      END;
    EXCEPT
    | NameServer.Error => 
      IO.PutError("mkdir "&path&" failed.");
    END;
    RETURN TRUE;
  END Sync;

TYPE Alias = NameServer.Alias OBJECT 
  name : TEXT;
OVERRIDES
  getObject := GetObject;
END;

PROCEDURE GetObject (self: Alias) : REFANY = 
  BEGIN
    TRY
      RETURN FileSystem.Lookup(NIL, self.name);
    EXCEPT
    | NameServer.Error(ec) =>
      IO.Put("NS.methodGetObject namespace error " & Fmt.Int(ORD(ec)) & "\n");
      RETURN NIL;
    END;
  END GetObject;

PROCEDURE Ln (closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    data, path : TEXT;
    dirName, lastName: NSName.T;
    parent: Directory.T;
    alias: Alias;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      path := pp.getNext();
      data := pp.getNext();
      
      dirName := NSName.FromText(path);
      NameServer.GetDirName(dirName, lastName);
      parent := FileSystem.LookupName(CWD(), dirName);
      
      alias := NEW(Alias, name := data);
      alias.name := data;
      parent.attach(lastName, alias);
    EXCEPT
    | NameServer.Error(ec) =>
      IO.Put("ln: " & path & ":" & Fmt.Int(ORD(ec)) & ".\n");
    | ParseParams.Error =>
      Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END Ln;

PROCEDURE Rm(closure: REFANY; pp: ParseParams.T):BOOLEAN =
  VAR
    path: TEXT;
    dirName, lastName: NameServer.Name;
    entry: REFANY;
    dir: Directory.T;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      path := pp.getNext();
      
      dirName := NSName.FromText(path);
      NameServer.GetDirName(dirName, lastName);
      entry := FileSystem.LookupName(CWD(), dirName);
      
      IF NOT ISTYPE(entry, Directory.T) THEN
	IO.PutError("rm: directory not exist.\n");
      ELSE
	dir := entry;
	dir.detach(lastName);
      END;
    EXCEPT
    | NameServer.Error(ec) =>
      IO.PutError("rm error " & Fmt.Int(ORD(ec)) & "\n");
    | ParseParams.Error =>
      Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END Rm;

PROCEDURE Touch (closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    path: TEXT;
    dirName, lastName: NameServer.Name;
    entry: REFANY;
    dir: Directory.T;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      path := pp.getNext();

      dirName := NSName.FromText(path);
      NameServer.GetDirName(dirName, lastName);
			      
      entry := FileSystem.LookupName(CWD(), dirName);
      IF NOT ISTYPE(entry, Directory.T) THEN
	IO.PutError("touch: non-directory specified.\n");
      END;
      dir := entry;

      entry := FileSystem.LookupSoft(dir, NSName.ToText(lastName));
      TYPECASE entry OF 
      | NULL =>
	EVAL dir.mkfile(lastName);
      | File.T(file) =>
	EVAL file.open(0); (* should update the time stamp *)
	file.close();
      ELSE
	IO.PutError("touch: non-file specified.\n");
      END;
    EXCEPT
    | NameServer.Error (ec) =>
      IO.Put("touch error " & Fmt.Int(ORD(ec)) & "\n");
    | ParseParams.Error =>
      Commands.ParseError(closure);
    | Error.E(e) =>
      IO.PutError(path & ":" & e.message() & "\n");
    END;
    RETURN TRUE;
  END Touch;

PROCEDURE Cd (<*UNUSED*> closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    path      : TEXT;
    root      : NameServer.T;
    entry     : REFANY;
  BEGIN

    pp.reset();
    TRY
      pp.skipNext();
      path := pp.peekNext();
      IF path # NIL THEN 
        path := pp.getNext();
      END;
    EXCEPT
    | ParseParams.Error =>
      path := NIL;
    END;

    TRY
      root := CWD(path);
      entry := FileSystem.Lookup(root, path);
      TYPECASE entry OF 
      | NameServer.T =>
        VAR cwd : TEXT;
        BEGIN
          cwd := Glob.GetVariable(Shell.Vars(),"cwd");
          (* make sure that concat of 'cwd' & 'path' has
             no more or no less than one '/' separating them *)
          
          IF Text.GetChar(path,0) = '/' THEN
            (* set absolute path *)
            Glob.SetVariable(Shell.Vars(), "cwd", path);
          ELSE
            (* set relative path *)
            IF Text.GetChar(cwd,Text.Length(cwd)-1) # '/' THEN
              cwd := cwd & "/" & path;
            ELSE
              cwd := cwd & path;
            END;
            Glob.SetVariable(Shell.Vars(), "cwd", cwd);
          END;
        END;
      ELSE
        IO.Put(path);
        IO.Put(" not a directory\n");
      END;
    EXCEPT
      NameServer.Error (ec) =>
        IO.Put("cd error " & Fmt.Int(ORD(ec)) & "\n");
    END;
    RETURN TRUE;
  END Cd;

PROCEDURE Ls (closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  TYPE FileEntry = REF RECORD
    name: TEXT;
    stat: FileStat.T;
    symlink: TEXT;
  END;

  TYPE SortBy = {Name, Time, Size};

  VAR
    nFiles    : INTEGER; (* # of files in the dir *)
    nColumns  : INTEGER; (* # of colums in the display *)
    pathLen   : INTEGER;
    path      : TEXT;
    entry     : REFANY;
    fileEntry : FileEntry;
    root      : NameServer.T;

  VAR
    files: RefList.T := NIL;
    maxFileNameLen : CARDINAL  := 0;
    parsedOptions := FALSE;
    longFormat := FALSE;
    derefSymlink := FALSE;
    sortBy := SortBy.Name;
    
  PROCEDURE Compare (r1, r2 : REFANY) : [-1 .. 1] =
    VAR
      a := NARROW(r1, FileEntry);
      b := NARROW(r2, FileEntry);
    BEGIN
      CASE sortBy OF
      | SortBy.Name => RETURN Text.Compare(a.name, b.name);
      | SortBy.Time => RETURN a.stat.atime - b.stat.atime;
      | SortBy.Size => RETURN a.stat.size - b.stat.size;
      END;
    END Compare;
    
  PROCEDURE AddEntry (name: TEXT): FileEntry =
    VAR fileEntry := NEW(FileEntry, name := name);
    BEGIN
      files := RefList.Cons(fileEntry, files);
      maxFileNameLen := MAX(maxFileNameLen, Text.Length(name));
      RETURN fileEntry;
    END AddEntry;

  PROCEDURE FillEntry (entry: REFANY; VAR ent: FileEntry) RAISES {Error.E} =
    BEGIN
      ent.symlink := NIL;
      ent.stat.size := 0;
      ent.stat.mode := 0;
      TYPECASE entry OF
      | NULL =>
      | Symlink.T(symlink) =>
	symlink.stat(ent.stat);
	VAR name: NameServer.Name;
	BEGIN
	  symlink.getName(name);
	  ent.symlink := NSName.ToText(name);
	END;
      | Directory.T(dir) =>
	dir.stat(ent.stat);
      | File.T(file) =>
	file.stat(ent.stat);
      | NameServer.T(dir) =>
	ent.stat.size := dir.size();
	ent.stat.mode := 16_4000; (* XXX arbitrary value *)
      | NameServer.Alias =>
	ent.stat.mode := 16_A000; (* XXX arbitrary value *)
      ELSE
	ent.stat.mode := 16_E000; (* XXX  arbitrary value *)
      END;
    END FillEntry;
    
  PROCEDURE DirectoryOp (dir: NameServer.T) RAISES {Error.E} = 
    VAR
      entry: REFANY;
      key: NameServer.Name;
      itr := dir.iterate();
    BEGIN
      WHILE itr.next(key,entry) DO
	fileEntry := AddEntry(NSName.ToText(key));
	IF longFormat THEN FillEntry(entry, fileEntry); END;
      END;
    END DirectoryOp; 

  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* Command *)

      (* Parse all the options *)
      REPEAT
        parsedOptions := FALSE;
        IF pp.testNext("-l") THEN
          longFormat := TRUE;
          parsedOptions := TRUE;
	END;
        IF pp.testNext("-t") THEN
          sortBy := SortBy.Time;
          parsedOptions := TRUE;
        END;
        IF pp.testNext("-L") THEN
	  derefSymlink := TRUE;
          parsedOptions := TRUE;
        END;
      UNTIL parsedOptions = FALSE;
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(closure);
      RETURN TRUE;
    END;
      
    TRY 
      path := ".";
      IF pp.peekNext() # NIL THEN
        path := pp.getNext();
      END;
    EXCEPT
    | ParseParams.Error =>
    END;

    root := CWD(path);
    TRY
      entry := FileSystem.Lookup(root, path, NOT derefSymlink);
      TYPECASE entry OF 
      | NULL => 
        RAISE NameServer.Error(NameServer.EC.NameNotFound);
      | NameServer.T(dir) => DirectoryOp(dir);
      ELSE
	fileEntry := AddEntry(path);
	IF longFormat THEN
	  FillEntry(entry, fileEntry);
	END;
      END;
    EXCEPT
    | NameServer.Error =>
      IO.Put("ls: " & path & " not found.\n");
    | Error.E(e) =>
      IO.Put("ls:" & path & ":" & e.message());
    END;

    (* Sort the files in user-specified order *)
    files := RefListSort.Sort(files, Compare);
    nFiles := RefList.Length(files);

    IF longFormat THEN
      FOR i := 0 TO nFiles-1 DO 
        fileEntry := NARROW(RefList.Nth(files, i), FileEntry);
        IO.Put(fileEntry.name);
        FOR i := 0 TO maxFileNameLen - Text.Length(fileEntry.name) + 2 DO 
          IO.Put(" ");
        END;

        CASE Word.And(fileEntry.stat.mode, 16_F000) OF
        | 16_4000 => (* directory *)
          IO.Put("d");
        | 16_8000 => (* reg *)
          IO.Put("r");
        | 16_A000 => (* link *)
          IO.Put("l");
        | 16_E000 => (* special *)
          IO.Put("s");
        ELSE
          IO.Put("u");
        END;
        IO.Put(Fmt.Unsigned(Word.And(fileEntry.stat.mode,16_0FFF)));
        IO.Put(Fmt.Pad(Fmt.Int(fileEntry.stat.uid), 8));
        IO.Put(Fmt.Pad(Fmt.Int(fileEntry.stat.gid), 8));
        IO.Put(Fmt.Pad(Fmt.Int(fileEntry.stat.size), 8));
	IF fileEntry.symlink # NIL THEN
	  IO.Put(" -> " & fileEntry.symlink);
	END;
        IO.Put("\n");
      END;
    ELSE 
      nColumns := ScreenWidth DIV (maxFileNameLen+2);
      FOR row := 0 TO ((nFiles-1) DIV nColumns) DO
        FOR column := 0 TO nColumns-1 DO
          VAR
            idx := column + row * nColumns;
          BEGIN
            IF idx < nFiles THEN
              fileEntry := NARROW(RefList.Nth(files, idx), FileEntry);
              pathLen := Text.Length(fileEntry.name);
              IO.Put(fileEntry.name);
            ELSE
              pathLen := 0;
            END;
            IF column < nColumns-1 THEN
              FOR i := 0 TO maxFileNameLen - pathLen + 2 DO 
                IO.Put(" ");
              END;
            END;
          END;
        END;
        IO.Put("\n");
      END;
    END; (* not longFormat *)
    RETURN TRUE;
  END Ls;

PROCEDURE Mount (<*UNUSED*> closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    dev     : TEXT;
    fs      : TEXT;
    path    : TEXT;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* Command *)
      IF pp.testNext("-u") THEN
        path := pp.getNext();
        FileSystem.Unmount(path);
      ELSE
        fs   := pp.getNext(); 
        dev  := pp.getNext();
        path := pp.getNext();
        FileSystem.Mount(fs, dev, path);
      END
    EXCEPT
    | Error.E(e) =>
      IO.PutError("mount: "& e.message() & "\n");
    | ParseParams.Error =>
      IO.PutError("mount: Param error. See help.\n");
    END;
    RETURN TRUE;
  END Mount;

PROCEDURE SIZE (<*UNUSED*> closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    path    : TEXT;
    root : NameServer.T;
    entry : REFANY;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      path := pp.getNext();
    EXCEPT
      ParseParams.Error => 
      IO.Put("size <file>\n"); 
      RETURN TRUE;
    END;

    root := CWD(path);
    TRY
      IO.Put("File " & path & "\n");
      entry := FileSystem.Lookup(root, path);
    EXCEPT
    | NameServer.Error =>
      IO.Put ("size namespace error\n");
      RETURN TRUE;
    END;

    TYPECASE entry OF
    | NULL =>
      IO.PutError(path & " does not exist.\n");
    | File.TPublic(file) =>
      CONST BufferSize = 8 * 1024;
      VAR
        buf     : REF ARRAY OF CHAR;
        from    : CARDINAL;
	bufsize : CARDINAL;
        size    : CARDINAL := 0;
      BEGIN
        TRY
          file := file.open(0);
          TRY
	    REPEAT
	      bufsize := file.readRef(buf, BufferSize, size, from);
              size := size + bufsize;
	    UNTIL bufsize < BufferSize;
            IO.Put("Read " & Fmt.Int(size) & " from file " & path & "\n");
          FINALLY
            file.close();
          END;
        EXCEPT
        | Error.E => IO.PutError("Could not size " & path & "\n");
        END;
      END;
    ELSE
      IO.PutError(path & " is not a file.\n");
    END;
    RETURN TRUE;
  END SIZE;

PROCEDURE Cat (closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    path: TEXT;
    entry: REFANY;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      path := pp.getNext();
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(closure);
      RETURN TRUE;
    END;

    IO.Put("cat: " & path & "\n");
    TRY
      entry := FileSystem.Lookup(CWD(), path);
    EXCEPT
    | NameServer.Error =>
      IO.PutError("cat namespace error\n");
      RETURN TRUE;
    END;

    TYPECASE entry OF
    | File.T(file) =>
      VAR
        bytes : CARDINAL;  
        buf   : REF ARRAY OF CHAR;
        from  : CARDINAL;  
        stat  : FileStat.T;
      BEGIN
        TRY
          file := file.open(0 (* XXX need real mode *));
          TRY
            file.stat(stat);
	    bytes := file.readRef(buf, stat.size, 0, from);
            IO.Put("Read " & Fmt.Int(bytes) & " from file " & path & "\n");
            IF buf # NIL THEN 
              IO.Put(Text.FromChars(SUBARRAY(buf^, from, bytes)));
            END;
          FINALLY
            file.close();
          END;
        EXCEPT
        | Error.E(e) => IO.PutError(path & ":" & e.message() & "\n");
        END;
      END;
    ELSE
      IO.Put("Not a file.\n");
    END;
    RETURN TRUE;
  END Cat;

PROCEDURE Mkfile (closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    path         : TEXT;
    entry        : REFANY;
    size         : CARDINAL;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      path := pp.getNext();
      size := pp.getNextInt();
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(closure);
      RETURN TRUE;
    END;

    IO.Put("mkfile: manipulating " & path & "\n");
    TRY
      entry := FileSystem.Lookup(CWD(), path);
    EXCEPT
    | NameServer.Error =>
      IO.PutError("mkfile namespace error\n");
      RETURN TRUE;
    END;

    TYPECASE entry OF
    | File.T(file) =>
      VAR
        bytes: CARDINAL;  
        buf: ARRAY [0..0] OF CHAR;
      BEGIN
        TRY
	  file := file.open(0);
          TRY
            buf[0] := '\000';
            bytes := file.write(buf, size-1);
          FINALLY
            file.close();
          END;
        EXCEPT
        | Error.E(e) => IO.PutError(path & ":" & e.message() & "\n");
        END;
      END;
    ELSE
      IO.Put("Not a file.\n");
    END;
    RETURN TRUE;
  END Mkfile;

PROCEDURE Swapon (closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  VAR
    path         : TEXT;
    root : NameServer.T;
    entry        : REFANY;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      path := pp.getNext();
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(closure);
      RETURN TRUE;
    END;

    root := CWD(path);
    TRY
      entry := FileSystem.Lookup(root, path);
    EXCEPT
    | NameServer.Error =>
      IO.PutError("swapon namespace error\n");
      RETURN TRUE;
    END;

    DefaultPager.SwapOn(entry);
    RETURN TRUE;
  END Swapon;

PROCEDURE Cp (closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  CONST BufferSize = 8 * 1024;
  VAR
    src, dst: TEXT;
    cwd: NameServer.T;
    srcentry, dstentry : REFANY;
    srcfp, dstfp : File.TPublic;
    buf: REF ARRAY OF CHAR;
    outsize: CARDINAL;
    insize: CARDINAL := BufferSize;
    from: CARDINAL := 0;
    off: CARDINAL := 0;
    retain: BOOLEAN;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      src := pp.getNext();
      dst := pp.getNext();
    EXCEPT
      ParseParams.Error => 
      Commands.ParseError(closure);
      RETURN TRUE;
    END;

    TRY
      cwd := CWD();
      srcentry := FileSystem.Lookup(cwd, src);
      dstentry := FileSystem.LookupSoft(cwd, dst);

      IF NOT ISTYPE(srcentry, File.T) THEN
	IO.PutError(src & " is not a file.\n");
      END;
      srcfp := srcentry;
      
      TYPECASE dstentry OF
      | NULL =>
	VAR
	  dirName, lastName: NameServer.Name;
	  parent: Directory.T;
	BEGIN
	  dirName := NSName.FromText(dst);
	  NameServer.GetDirName(dirName, lastName);
	  parent := FileSystem.LookupName(CWD(), dirName);
	  dstfp := parent.mkfile(lastName);
	END;
      | File.T(dstfile) =>
	dstfp := dstfile;
      ELSE
	IO.PutError("cp: " & dst & " is not a file.\n");
	RETURN TRUE;
      END;

      srcfp := srcfp.open(0); (* XXX *)
      dstfp := dstfp.open(0); (* XXX *)
      TRY
	WHILE insize = BufferSize DO
	  buf := NIL;
	  insize := srcfp.readRef(buf, insize, off, from);
	  <* ASSERT buf # NIL *>
	  outsize := dstfp.writeRef(buf, insize, off, from, retain);
	  IF outsize # insize THEN
	    IO.PutError("CP short write\n");
	    RETURN TRUE;
	  END;
	  INC(off,insize);
	END;
      FINALLY
	srcfp.close();
	dstfp.close();
      END;
    EXCEPT
    | NameServer.Error => IO.PutError("cp: ns error.\n");
    | Error.E(e) => IO.PutError("cp: " & src & e.message() & ".\n");
    END;
    RETURN TRUE;
  END Cp;

PROCEDURE Dd (r: REFANY; pp: ParseParams.T): BOOLEAN =
  VAR
    path: TEXT;
    root: NameServer.T;
    buf := NEW(REF ARRAY OF CHAR, 1024 * 1024);
    off := 0;
    stat: FileStat.T;
    outSize: CARDINAL;
    retain: BOOLEAN;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
      path := pp.getNext();
      root := CWD(path);
      TYPECASE FileSystem.Lookup(root, path) OF 
      | File.T(file) =>
	EVAL file.open(0);
	file.stat(stat);
	WHILE off < stat.size DO 
	  outSize := file.writeRef(buf, MIN(NUMBER(buf^), stat.size-off),
				   off, 0, retain);
	  IF outSize <= 0 THEN EXIT; END;
	  IO.Put(".");
	  INC(off, outSize);
	END;
      END;
      IO.Put(path & ": 16_" & Fmt.Int(off, 16) & " bytes cleared.\n");
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(r);
    | NameServer.Error =>
      IO.PutError("dd: ns error.\n");
    | Error.E(e) =>
      IO.PutError("dd: " & e.message());
    END;
    RETURN TRUE;
  END Dd;
 
PROCEDURE Cmp (closure: REFANY; pp: ParseParams.T):BOOLEAN = 
  CONST BufferSize = 8 * 1024;
  VAR
    src, dst: TEXT;
    cwd: NameServer.T;
    srcentry, dstentry : REFANY;
    srcfp, dstfp : File.TPublic;
    srcbuf, dstbuf: REF ARRAY OF CHAR;
    srcsize, dstsize: CARDINAL := BufferSize;
    from: CARDINAL := 0;
    off: CARDINAL := 0;
    retain : BOOLEAN;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      src := pp.getNext();
      dst := pp.getNext();
    EXCEPT
      ParseParams.Error => 
      Commands.ParseError(closure);
      RETURN TRUE;
    END;

    TRY
      cwd := CWD();
      srcentry := FileSystem.Lookup(cwd, src);
      dstentry := FileSystem.LookupSoft(cwd, dst);

      IF (srcentry = NIL) OR (NOT ISTYPE(srcentry, File.T)) THEN
	IO.PutError(src & " is not a file.\n");
	RETURN TRUE;
      END;
      srcfp := srcentry;
      
      IF (dstentry = NIL) OR (NOT ISTYPE(dstentry, File.T)) THEN
	IO.PutError(dst & " is not a file.\n");
	RETURN TRUE;
      END;
      dstfp := dstentry;

      srcfp := srcfp.open(0);
      dstfp := dstfp.open(0);
      TRY
	WHILE srcsize = BufferSize DO
	  srcbuf := NIL; dstbuf := NIL;
	  retain := FALSE;
	  srcsize := srcfp.readRef(srcbuf, srcsize, off, from);
	  retain := FALSE;
	  dstsize := dstfp.readRef(dstbuf, dstsize, off, from);
	  <* ASSERT srcbuf # NIL *> <* ASSERT dstbuf # NIL *>
	  FOR i:= 0 TO MAX(srcsize, dstsize)-1 DO
	    (* XXX Fixme! This is too SLOW! *)
	    IF srcbuf[i] # dstbuf[i] THEN
	      IO.Put(src & dst & " differ at " & Fmt.Int(off+i+1)); 
	      IO.Put(" " & Text.FromChar(srcbuf[i]) &
		     ", " & Text.FromChar(dstbuf[i]) & "\n");
	      srcfp.close();
	      dstfp.close();
	      RETURN TRUE;
	    END;
	  END;
	  INC(off, srcsize);
	END;
      FINALLY
	srcfp.close();
	dstfp.close();
      END;
    EXCEPT
    | NameServer.Error => IO.PutError("cp: ns error.\n");
    | Error.E(e) => IO.PutError("cmp: " & src & e.message() & ".\n");
    END;
    RETURN TRUE;
  END Cmp;

PROCEDURE Init(<*UNUSED*> verbose: BOOLEAN) = 
  BEGIN
    EVAL Commands.Install(Cat,"cat","file");
    EVAL Commands.Install(Cd,"cd", "dir");
    EVAL Commands.Install(Ln,"ln", "source target");
    EVAL Commands.Install(Ls,"ls", "[-l] path");
    EVAL Commands.Install(Mkdir,"mkdir", "path");
    EVAL Commands.Install(Mkfile,"mkfile", "path size");
    EVAL Commands.Install(Mount,"mount", "fs-type device path| -u path");
    EVAL Commands.Install(Newfs, "newfs", "fsname devname");
    EVAL Commands.Install(Rm, "rm", "path");
    EVAL Commands.Install(SIZE, "size", "path");
    EVAL Commands.Install(Sync, "sync", "fsname devname");
    EVAL Commands.Install(Touch, "touch", " path");
    EVAL Commands.Install(Swapon, "swapon", " path size");
    EVAL Commands.Install(Cp, "cp", " source-file destination-file");
    EVAL Commands.Install(Dd, "dd", " dest-file", "clear DEST-FILE");
    EVAL Commands.Install(Cmp, "cmp", " file1 file2");
  END Init;

BEGIN
  Init(TRUE);
END FsCommands. 

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 06-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Cleanup. Also, changed not to skip entry even the value is NIL.
 *	This is to cope with the semantics change in iterate.next();
 *	it now does lookup for each name.
 * 30-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to list nameserver aliases and object.
 *
 * 22-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use new nameserver interface.
 *
 * 09-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE Ls;
IMPORT IO, Error, File, FileStat, FileSystem, ParseParams, Fmt, Text,
       RefList, RefListSort, NameServer, Word, Glob, Shell;

CONST ScreenWidth = 78;

TYPE FileEntry = REF RECORD
  name : TEXT;
  stat : FileStat.T;
END;

TYPE SortBy = {Name, Time, Size};

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    nFiles    : INTEGER; (* # of files in the dir *)
    nColumns  : INTEGER; (* # of colums in the display *)
    pathLen   : INTEGER;
    path      : TEXT;
    root      : NameServer.TBase;
    component : NameServer.Name;
    parent    : NameServer.TBase;

  VAR
    files          : RefList.T := NIL;
    maxFileNameLen : CARDINAL  := 0;
    parsedOptions  : BOOLEAN   := FALSE;
    longFormat     : BOOLEAN   := FALSE;
    sortBy                     := SortBy.Name;
    
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
      maxFileNameLen := MAX(maxFileNameLen, Text.Length(key));
      RETURN fileEntry;
    END AddEntry;

  PROCEDURE FillStat (entry: REFANY; stat: FileStat.T) =
    BEGIN
      fileEntry.stat.size := 0;
      fileEntry.stat.mode := 0;
      TYPECASE entry OF
      | Directory.T(dir) =>
	dir.stat(fileEntry.stat);
      | File.T(file) =>
	file.stat(fileEntry.stat);
      | NameServer.T(dir) =>
	fileEntry.stat.size := dir.size();
	fileEntry.stat.mode := 16_4000; (* XXX arbitrary value *)
      | NameServer.Alias =>
	fileEntry.stat.mode := 16_A000; (* XXX arbitrary value *)
      ELSE
	fileEntry.stat.mode := 16_E000; (* XXX  arbitrary value *)
      END;
    END FillStat;
    
  PROCEDURE DirectoryOp (dir: NameServer.TBase) = 
    VAR
      entry: REFANY;
      key: NameServer.Name;
      fileEntry: FileEntry;
      entry: REFANY;
      itr := dir.iterate();
    BEGIN
      WHILE itr.next(key,entry) DO
	fileEntry := AddEntry(key);
	IF longFormat THEN FillStat(entry, fileEntry.stat); END;
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
      UNTIL parsedOptions = FALSE;

      root := FileSystem.GetRoot();
      TRY
        (* list the file or directory *)
        path := pp.getNext();
        IF Text.GetChar(path,0) # '/' THEN
          VAR 
            cwd : TEXT;
          BEGIN
            cwd := Glob.GetVariable(Shell.Vars(), "cwd");
            IF cwd # NIL THEN
              entry := root.lookup(cwd,component,parent);
              TYPECASE entry OF 
              | NULL => (* leave root as is *)
              | NameServer.TBase(dir) =>
                root := dir; (* change root to 'cwd' *)
              ELSE (* leave root as is *)
              END;
            ELSE
              Glob.SetVariable(Shell.Vars(), "cwd","/");
            END;
          END;
        END;
      EXCEPT
      | ParseParams.Error =>
        path := Glob.GetVariable(Shell.Vars(), "cwd");
        IF path = NIL THEN path := "/"; END;
      END;

      TRY
        entry := root.lookup(path,component,parent);
        TYPECASE entry OF 
	| NameServer.TBase(dir) => DirectoryOp(dir);
	ELSE
	  VAR fileEntry := AddEntry(file);
	  BEGIN
	    IF longFormat THEN FillStat(entry, fileEntry.stat); END;
	  END;
	END;
      EXCEPT
      | NameServer.Error =>
        IO.Put ("LS namespace error\n");
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
    EXCEPT
    | Error.E(e) =>
      IO.PutError("ls : " & e.message() & ".\n");
    | ParseParams.Error =>
      IO.PutError("Usage : " & CommandHelp & "\n");
    END;
    RETURN TRUE;
  END Run;

BEGIN
END Ls.

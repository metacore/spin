(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 22-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted. Stat fills all the fields.
 *)
(*
	An Info File implements a File.T for procfs style files.
	Clients create a file with the Create call supplying a pathname
	for the file and a fill function.  The fill function is called
	whenever the file is opened.
 *)

MODULE InfoFile;

IMPORT Error, File, FileStat, FileSystem, NameServer, IO,
       TextF, TextWr;
IMPORT NSName;

(* ------------------- FILE SERVICES ------------------- *)

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

TYPE FileT = File.T BRANDED OBJECT
    data  : TEXT := NIL; (* REF ARRAY OF CHAR; Where the bits really live *)
    fillFile : FillFileT; (* PROC that writes the data *)
  OVERRIDES
    read     := methodFileRead;
    close    := methodFileClose;
    open     := methodFileOpen;
    stat     := methodFileStat;
    ioctl    := methodFileIoctl;
  END;

PROCEDURE methodFileOpen (
    self: FileT;
    <*UNUSED*>mode : INTEGER): File.T
  RAISES { Error.E } <*NOWARN*> =
  VAR
    wr := TextWr.New();
    f := NEW(FileT);
  BEGIN
    f.fillFile := self.fillFile;
    f.fillFile(wr);
    f.data := TextWr.ToText(wr);
    RETURN f;
  END methodFileOpen;


PROCEDURE methodFileRead(
			 self    : FileT;
			 VAR data : ARRAY OF CHAR;
			 offset   : File.OffsetT
			 ): CARDINAL 
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

PROCEDURE methodFileClose (self: FileT) 
  RAISES {Error.E} <*NOWARN*> =
  BEGIN
    self.data := NIL
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

PROCEDURE methodFileIoctl (
    <*UNUSED*>self    : FileT;
    cmd     : INTEGER;
    <*UNUSED*>VAR arg     : ARRAY OF CHAR;
    <*UNUSED*>mode    : INTEGER) RAISES {Error.E} =
  BEGIN
    (* wish keeps sending TIOCGETP -- are we a tty? no. *)
    IF cmd # 16_40067408 THEN
      IO.Put("InfoFile File.Ioctl default not implemented.\n");
    END;
    RAISE Error.E(NEW(File.ErrorT).init(File.FS_NOT_SUPPORTED));
  END methodFileIoctl;

PROCEDURE Create(path:TEXT; fillFile: FillFileT) RAISES {Error.E} = 
  VAR
    name, lastName: NameServer.Name;
    dir: NameServer.T;
    entry: REFANY;
  BEGIN
    TRY 
      entry := FileSystem.LookupSoft(NIL, path);
      IF entry # NIL THEN
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_INVALID_PARAMETER));
      END;

      name := NSName.FromText(path);
      NameServer.GetDirName(name, lastName);
      dir :=  FileSystem.LookupName(NIL, name);
      dir.attach(lastName, NEW(FileT, fillFile := fillFile));
    EXCEPT
    | NameServer.Error =>
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_INVALID_FS));
    END;
  END Create;


BEGIN
END InfoFile.

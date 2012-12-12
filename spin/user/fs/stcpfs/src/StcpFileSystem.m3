(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Arp-97  Tsutomu Owa (owa) at the University of Washington
 *	Added the ability to select a server, SimpleHttp.FreeMemory().
 *
 * 01-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created by modifying tftpfs.
 *)

MODULE StcpFileSystem;
IMPORT Directory, Error, File, FileStat, FileSystem, IO,
       NameServer, Text;
IMPORT SimpleHttp;
(* IMPORT Shell, Glob; *)
IMPORT NSName;
IMPORT BuildInfo, Salnet, NetText;


REVEAL DirectoryT = Directory.T BRANDED OBJECT
  OVERRIDES
    lookup := Lookup;
  END;

PROCEDURE Lookup (<*UNUSED*> self: DirectoryT; 
		  VAR xname: NameServer.Name; 
		  <*UNUSED*> getalias: BOOLEAN)
  : REFANY RAISES {NameServer.Error} = 
  VAR
    name := NSName.ToText(xname);
    entry: FileT;
    server: Salnet.IpAddr;
  BEGIN
    (* name given by filesystem is relative *)
    IF Text.GetChar(name,0) # '/' THEN
      (* XXX until we have softlinks we have to put /spin here
             so gdb can run -- DB *)
      name := "/spin/" & name; 
    END;
    xname.end := 0; (* let the caller know that xname is parsed up. *)
    server := NetText.TextToIp( BuildInfo.GetHttpServAddr() );

    TRY
      entry := NEW(FileT, data := SimpleHttp.Get(name, server));
      SimpleHttp.FreeMemory();
    EXCEPT
    | SimpleHttp.Error(msg) =>
      IO.Put("http error: " & msg);
      RAISE NameServer.Error(NameServer.EC.NameNotFound);
    END;
    RETURN entry;
  END Lookup;

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

REVEAL
  FileT = File.T BRANDED OBJECT
    data: REF ARRAY OF CHAR; (* Where the bits really live *)
  OVERRIDES
    read := Read;
    stat := Stat;
    close := Close;
  END;

PROCEDURE Read (self: FileT;
		VAR data: ARRAY OF CHAR;
		offset: File.OffsetT): CARDINAL RAISES {Error.E} = 
  BEGIN
    IF NUMBER(data) = 0 THEN RETURN 0; END;
    IF self.data = NIL OR offset > NUMBER(self.data^) THEN
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_NOT_IN_FILE));
    END;
    (* copy into user provided buffer *)
    WITH size = MIN(NUMBER(self.data^)-offset,NUMBER(data)),
         outdata = SUBARRAY(self.data^,offset, size) DO
      SUBARRAY(data,0,size) := outdata;
      RETURN size;
    END;
  END Read;

PROCEDURE Close (self: FileT) =
  BEGIN
    self.data := NIL;
  END Close;

PROCEDURE Stat (self: FileT; VAR stat: FileStat.T) =
  BEGIN
    FileStat.Init(stat);
    IF self.data # NIL THEN
      stat.size := NUMBER(self.data^);
    ELSE
      stat.size := 0;
    END;
  END Stat;

(* Note: I have no idea what I should do to mount. So I just copy
   everything from tftp file service. Maybe we need some conceptual
   streamlining of the service. -- yas*)

TYPE MountT = DirectoryT OBJECT
  stcpServer: TEXT;
END;

TYPE FileSystemT = FileSystem.T OBJECT
  OVERRIDES
    newfs := Newfs;
    deletefs := Deletefs;
  END;

PROCEDURE Newfs (<*UNUSED*>self: FileSystemT; 
		 READONLY name: NameServer.Name): Directory.T =
  VAR mp: MountT;
  BEGIN
    mp := NEW(MountT).init();
    mp.stcpServer := NSName.ToText(name);
    (* Do something so that we can use different stcp server, but it
       requires that we fix sal stcp first. *)
    RETURN mp;
  END Newfs;

PROCEDURE Deletefs (<*UNUSED*>self: FileSystemT; <*UNUSED*>root: Directory.T) =
  BEGIN
    IO.Put("stcpfs: disabling simple http.\n");
    SimpleHttp.Uninit();
  END Deletefs;
  
BEGIN 
  TRY
    VAR
      dir: Directory.T;
      name: NameServer.Name;
    BEGIN
      FileSystem.Register("stcp", NEW(FileSystemT));
      dir := FileSystem.GetRoot();
      (* create a directory *)
      name := NSName.FromText("spin");
      EVAL dir.create(name);
      FileSystem.Mount("stcp","silk","/spin");
    END;
  EXCEPT
  | NameServer.Error => 
    IO.PutError("StcpFileSystem creating /stcp directory failed.");
  | Error.E(e) =>
    IO.Put(e.message()&" during StcpFileSystem mount onto /.\n");
  END;
END StcpFileSystem.

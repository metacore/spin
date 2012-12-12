(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 21-May-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added mountPath variable so NFS can get to the mount path
 *      without changing 2000 interfaces.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Made initialization explicit.
 *
 * 04-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	Added sync, mkdir, rmdir, unlink to mountpoint.
 *
 * 27-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Keep file systems in name server.
 *
 * 05-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *      Resurrection. This file implements file system independent
 *      operations.
 *
 * 21-Dec-95  David Becker (becker) at the University of Washington
 *      Created.
 *)


MODULE FileSystem EXPORTS FileSystem, FileSystemPrivate;


IMPORT NameServer, Error, IO, Fmt, Text, File, Directory, DirectoryRep,
       FileId, FileIdPrivate, FileSystemInterface;
IMPORT NSName;
IMPORT Debugger; <*NOWARN*>
VAR svcNS  : NameServer.T;
VAR fscore : NameServer.T;

TYPE DirectoryT = Directory.T BRANDED OBJECT
  METHODS
  OVERRIDES
    (* "create" allocates a new DirectoryT for tmpfs *)
    init := methodDirectoryInit;
  END;

PROCEDURE methodDirectoryInit(self : DirectoryT): NameServer.T =
  BEGIN
    self.id := rootId;
    RETURN Directory.T.init(self);
  END methodDirectoryInit;

VAR root: Directory.T;
CONST rootId = FileId.T { VAL(16_FF,CHAR), ..};
        (* "rootId" is the filesystem root handle *)

PROCEDURE GetRoot (): Directory.T = 
  BEGIN
    RETURN root;
  END GetRoot;

<* INLINE *>
PROCEDURE Test(name: TEXT) RAISES {Error.E} =
  BEGIN
    IF name = NIL OR Text.Empty(name) THEN
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_NO_ENTRY));
    END;
  END Test;

(* ------------------- FileSystem Registration ------------------- *)

PROCEDURE Register(name: TEXT; fs: T) RAISES {Error.E} = 
  BEGIN
    Test(name);
    TRY
      IF NameServer.LookupSoft(fscore, name) = NIL THEN
        NameServer.Attach(fscore, name, fs);
      ELSE
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_INVALID_FS));
      END;
    EXCEPT
      NameServer.Error =>
      IO.Put("FileSystem : Can't register "&name&"\n");
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_NO_ENTRY));
    END;
  END Register;

PROCEDURE Deregister(name: TEXT; VAR fs: T) RAISES {Error.E} = 
  BEGIN
    Test(name);
    TRY
      fs := NameServer.Lookup(fscore, name);
      NameServer.Detach(fscore, name);
    EXCEPT
      NameServer.Error =>
      IO.Put("FileSystem : Can't deregister "&name&"\n");
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_NO_ENTRY));
    END;
  END Deregister;

(* ------------------- MOUNT SERVICES ------------------- *)

PROCEDURE Mount (filesystemName: TEXT;
		 deviceName: TEXT;
		 path: TEXT) RAISES {Error.E} =
  VAR
    fs: T;
    entry      : REFANY;
    name: NameServer.Name;
  BEGIN
    Test(deviceName);
    Test(path);

    TRY 
      (* look up the file system *)
      TYPECASE NameServer.Lookup(fscore, filesystemName) OF
      | T(t) =>
	fs := t;
      ELSE
	IO.PutError("mount: " & filesystemName & " is not a file system.\n");
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
      END;

      entry := Lookup(NIL, path, FALSE);
      TYPECASE entry OF
      | Directory.T(oldRoot) =>
	VAR
	  newRoot: Directory.T;
	  mountInfo: DirectoryRep.MountInfo;
	BEGIN
	  name := NSName.FromText(deviceName);
	  TRY
	    newRoot := fs.newfs(name);
	  EXCEPT
	  | Error.E(e) =>
	    RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
	  END;
	  
	  LOCK mountPath DO
	    (* tell the filesystem where it is being mounted *)
	    mountPath.t := path;
	  END;

	  mountInfo := NEW(DirectoryRep.MountInfo,
			   root := newRoot,
			   shadow := oldRoot,
			   fs := fs);
	  
	  oldRoot.attr := Directory.Attr.MountShadow;
	  oldRoot.mountInfo := mountInfo;
	  newRoot.attr := Directory.Attr.MountPoint;
	  newRoot.mountInfo := mountInfo;
	  
	  IF Text.Equal(path, "/") THEN
	    (* Root directory is replaced. *)
	    root := newRoot;
	    (*NameServerPrivate.SetRoot(new);
	       IO.Put("Name server root set to file system root.\n");*)
	  END;
	END;
      ELSE
	RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
      END;
    EXCEPT
    | NameServer.Error =>
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
    END;
      
    IO.Put(Fmt.F("Mounted %s at %s\n",filesystemName, path));
  END Mount;

PROCEDURE Unmount (path: TEXT) RAISES {Error.E} =
  VAR
    entry: REFANY;
    oldRoot: Directory.T;
  BEGIN
    TRY 
      entry := Lookup(NIL, path, FALSE);
      TYPECASE entry OF
      | Directory.T(newRoot) =>
	IF newRoot.attr # Directory.Attr.MountPoint THEN
	  IO.PutError("FileSystem.Unmount: " & path
		      & " : not a mount point.\n");
	  RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
	END;
	oldRoot := newRoot.mountInfo.shadow;
	<*ASSERT newRoot.mountInfo = oldRoot.mountInfo *>
	newRoot.attr := Directory.Attr.Ordinary;
	oldRoot.attr := Directory.Attr.Ordinary;
	newRoot.mountInfo.fs.deletefs(newRoot);
	newRoot.mountInfo := NIL;
	oldRoot.mountInfo := NIL;
      ELSE
	IO.PutError("FileSystem.Unmount: " & path
		    & " : not a directory.\n");
	RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
      END;
    EXCEPT
    | NameServer.Error =>
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
    END;
  END Unmount;

(* ------------------- FileSystem Services ------------------- *)

REVEAL T = Public BRANDED OBJECT 
  OVERRIDES
    newfs := MethodNewfs;
    deletefs := Deletefs;
  END;

PROCEDURE MethodNewfs (<*UNUSED*>self: T;
		       <*UNUSED*>READONLY dev: NameServer.Name): Directory.T
  RAISES {Error.E} = 
  BEGIN
    RAISE Error.E(NEW(File.ErrorT).init(File.FS_NOT_SUPPORTED));
  END MethodNewfs;
  
PROCEDURE Deletefs (<*UNUSED*>self:T; <*UNUSED*>root: Directory.T) =
  BEGIN
  END Deletefs;
  
PROCEDURE NewFs(<*UNUSED*>fsname, devname: TEXT)  =
  BEGIN
    IO.Put("FileSystem.NewFs is now a nop.\n");
  END NewFs;

PROCEDURE Sync(<*UNUSED*>fsname, devname: TEXT) =
  BEGIN
    IO.Put("FileSystem.Sync is now a nop.\n");
  END Sync;

PROCEDURE Open(name: TEXT): REFANY RAISES {Error.E} =
  VAR
    entry: REFANY;
  BEGIN
    Test(name);
    TRY
      entry := Lookup(NIL, name, FALSE);
    EXCEPT
    | NameServer.Error =>
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_NO_ENTRY));
    END;
    TYPECASE entry OF 
    | NULL => (* skip *)
    | File.T(file) =>
      entry := file.open(0);  (* XXX need different mode *)
    ELSE (* skip *)
    END;
    RETURN entry;
  END Open;
  
PROCEDURE Lookup (dir: NameServer.T;
		  name: TEXT;
		  dontFollowAlias: BOOLEAN): REFANY
  RAISES {NameServer.Error} =
  BEGIN
    IF dir = NIL THEN dir := root; END;
    RETURN NameServer.Lookup(dir, name, dontFollowAlias, root);
  END Lookup;
  
PROCEDURE LookupSoft (dir: NameServer.T;
		      name: TEXT;
		      dontFollowAlias: BOOLEAN): REFANY
  RAISES {NameServer.Error} =
  BEGIN
    IF dir = NIL THEN dir := root; END;
    RETURN  NameServer.LookupSoft(dir, name, dontFollowAlias, root);
  END LookupSoft;
  
PROCEDURE LookupName(dir: NameServer.T;
		     READONLY name: NameServer.Name;
		     dontFollowAlias: BOOLEAN): REFANY
  RAISES {NameServer.Error} =
  BEGIN
    IF dir = NIL THEN dir := root; END;
    RETURN NameServer.LookupName(dir, name, dontFollowAlias, root);
  END LookupName;

(* -------------- FileSystem Core Registration ------------------- *)

PROCEDURE Init(verbose:BOOLEAN) =
  VAR
    nsRoot: NameServer.T;
  BEGIN
    TRY
      nsRoot := NameServer.Lookup(NIL, "/");
      (* initialize the mountPath variable *)
      mountPath := NEW(MPT);

      (* the file system root has the path "/fsroot". *)
      root := NEW(DirectoryT);
      EVAL root.init();
      NameServer.Attach(root, ".", root);
      NameServer.Attach(root, "..", nsRoot);
					  
      (* create the fs namespace *)
      svcNS := NameServer.Lookup(NIL, "/../svc");

      fscore := NEW(NameServer.Default).init();
      NameServer.Attach(svcNS, "fs", fscore);
					  
      EVAL FileSystemInterface.Export(NIL);

      IF verbose THEN 
        IO.Put("FileSystem namespace initialized\n"); 
      END;
    EXCEPT
    | NameServer.Error(ec) =>
      IO.PutError("FileSystem.Init: " & NameServer.Fmt(ec) &".\n");
    END;
  END Init;

BEGIN
  FileIdPrivate.Init(TRUE);
  Init(TRUE);
END FileSystem.

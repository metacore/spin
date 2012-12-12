(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description.
 *
 * HISTORY
 *)

(*
 * This module provides a least-recently-used write-through cache
 * that stacks on top of any other file system.
 * 
 * That's the theory, anyway...
 *
 * Current status:  UNDER DEVELOPMENT
 *)

MODULE LruWt;

IMPORT Error, Directory, File, FileId, FileStat, FileSystem, IO,
       NameServer, Rd, LruWtInterface;

(* -------------------- FILEID SUPPORT -------------------- *)

CONST NULLCHAR = VAL(16_00, CHAR);
VAR FileIdStart := FileId.T{
       'l',      'r',      'u',      'w',      't', NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR, NULLCHAR,
  NULLCHAR, NULLCHAR};

(* -------------------- ITERATOR SUPPORT -------------------- *)

REVEAL IteratorT = NameServer.Iterator BRANDED OBJECT
  ShadowIterator : NameServer.Iterator;
METHODS
  init(shadow : NameServer.Iterator) : IteratorT := methodIteratorInit;
OVERRIDES
  next  := methodIteratorNext;
  seek  := methodIteratorSeek;
  reset := methodIteratorReset;
END (* IteratorT *);

PROCEDURE methodIteratorInit(
    self   : IteratorT;
    shadow : NameServer.Iterator) : IteratorT =
  BEGIN
    self.ShadowIterator := shadow;
    RETURN self;
  END methodIteratorInit;

PROCEDURE methodIteratorNext(
    self : IteratorT;
    VAR k : NameServer.Name;
    VAR v : REFANY) : BOOLEAN =
  BEGIN
    RETURN self.ShadowIterator.next(k, v);
  END methodIteratorNext;

PROCEDURE methodIteratorSeek(
    self       : IteratorT;
    READONLY k : NameServer.Name) =
  BEGIN
    self.ShadowIterator.seek(k);
  END methodIteratorSeek;

PROCEDURE methodIteratorReset(
    self : IteratorT) =
  BEGIN
    self.ShadowIterator.reset();
  END methodIteratorReset;



(* -------------------- DIRECTORY SERVICES -------------------- *)

REVEAL DirectoryT = Directory.T BRANDED OBJECT
  ShadowDirectory: Directory.T;
  valid: BOOLEAN;                   (* A DirectoryT is not valid *)
                                    (* until given a shadow.     *)
METHODS
  init(root: Directory.T): DirectoryT := methodDirectoryInit;
  makeShadow(shadow: Directory.T) := methodMakeShadow;
OVERRIDES
  lookup       := methodDirectoryLookup;
  attach       := methodDirectoryAttach;
  detach       := methodDirectoryDetach;
  rename       := methodDirectoryRename;
  replace      := methodDirectoryReplace;
  iterate      := methodDirectoryIterate;
  size         := methodDirectorySize;
  getRd        := methodDirectoryGetRd;
  getComponent := methodDirectoryGetComponent;
  setDelimiter := methodDirectorySetDelimiter;
  create       := methodDirectoryCreate;
  mkfile       := methodDirectoryMkfile;
END;

PROCEDURE methodDirectoryGetRd(
    self : DirectoryT;
    name : NameServer.Name) : Rd.T =
  BEGIN
    <* ASSERT self.valid *>
    RETURN self.ShadowDirectory.getRd(name);
  END methodDirectoryGetRd;

PROCEDURE methodDirectoryGetComponent(
    self                    : DirectoryT;
    VAR (* INOUT *) name    : NameServer.Name;
    VAR (* OUT *) component : NameServer.Name) =
  BEGIN
    <* ASSERT self.valid *>
    self.ShadowDirectory.getComponent(name, component);
  END methodDirectoryGetComponent;

(* We must return a FileT or DirectoryT wrapper around whatever our
 * cache directory returns!
 *
 * If I understood m3 garbage collection, I'd be able to come up with
 * a better way to write this!
 *)
PROCEDURE methodDirectoryLookup(
    self                    : DirectoryT;
    name                    : NameServer.Name;
    VAR (* out *) component : NameServer.Name;
    VAR (* out *) parent    : NameServer.TBase;
    getalias                : BOOLEAN) : REFANY
  RAISES {NameServer.Error} =
  VAR
    obj                     : REFANY; (* the object with the name 'name' *)
    newFp                   : FileT;
    newDir                  : DirectoryT;
  BEGIN
    <* ASSERT self.valid *>
    IO.Put("In LruWt.methodDirectoryLookup("&name&").\n");
    obj := Directory.T.lookup(self, name, component, parent, getalias);
    TYPECASE obj OF
    | NULL =>
      (* The object isn't in our space.  See if we can get it *)
      (* from our shadow. *)
      obj := self.ShadowDirectory.lookup(name, component, parent, getalias);
      TYPECASE obj OF
      | NULL =>
        (* There *is* no object of that name.  So just pass it along. *)
        RETURN obj;
      | File.T(fp) =>
        (* We found a file... *)
        newFp := NEW(FileT).init(fp);
        self.attach(name, newFp);     (* Add this to our namespace *)
        TYPECASE parent OF
        | Directory.T(parentdir) =>
          (* We need to put a wrapper around the parent. *)
          newDir := NEW(DirectoryT).init(FileSystem.GetRoot());
          newDir.makeShadow(parentdir);
          (* XXX figure out what to do about the newDir & namespace !! *)
          parent := newDir;
        ELSE
          IO.Put("Unexpected parent of a file!\n");
        END (* typecase *);
        RETURN newFp;

      | Directory.T(dir) =>
        (* We found a directory... *)
        newDir := NEW(DirectoryT).init(FileSystem.GetRoot());
        newDir.makeShadow(dir);
        self.attach(name, newDir);    (* Add this to our namespace *)
        RETURN newDir;
      ELSE
        IO.Put("LruWt.methodDirectoryLookup:Found unexpected item!\n");
        RETURN obj;
      END (* typecase *);
    ELSE
      (* We found an object by that name in our namespace. *)
      (* return it. *)
      RETURN obj;
    END (* typecase *);
  END methodDirectoryLookup;

(* If we're attaching a DirectoryT or a FileT, we want to attach
 * their shadows to our shadow. 
 *)
PROCEDURE methodDirectoryAttach(
    self : DirectoryT;
    name : NameServer.Name;
    obj  : REFANY)
  RAISES {NameServer.Error} =
  BEGIN
    <* ASSERT self.valid *>
    IO.Put("In LruWt.methodDirectoryAttach.\n");
    TYPECASE obj OF
    | FileT(fp) =>
      (* We're attaching a FileT to ourselves.  So attach
       * its shadow to our shadow... *)
      self.ShadowDirectory.attach(name, fp.ShadowFile);
    | DirectoryT(dir) =>
      (* Attach dir's shadow directory to ourselves. *)
      self.ShadowDirectory.attach(name, dir.ShadowDirectory);
    ELSE
      IO.Put("LruWt.methodDirectoryAttach:Unexpected object type!\n");
      self.ShadowDirectory.attach(name, obj);
    END(*typecase*);
  END methodDirectoryAttach;

PROCEDURE methodDirectoryDetach(
    self    : DirectoryT;
    name    : NameServer.Name;
    VAR obj : REFANY)
  RAISES {NameServer.Error} =
  BEGIN
    <* ASSERT self.valid *>
    self.ShadowDirectory.detach(name, obj);
  END methodDirectoryDetach;

PROCEDURE methodDirectoryRename(
    self : DirectoryT;
    from : NameServer.Name;
    to   : NameServer.Name)
  RAISES {NameServer.Error} =
  BEGIN
    <* ASSERT self.valid *>
    self.ShadowDirectory.rename(from, to);
  END methodDirectoryRename;

PROCEDURE methodDirectoryReplace(
    self     : DirectoryT;
    name     : NameServer.Name;
    VAR obj  : REFANY)
  RAISES {NameServer.Error} =
  BEGIN
    <* ASSERT self.valid *>
    self.ShadowDirectory.replace(name, obj);
  END methodDirectoryReplace;

PROCEDURE methodDirectorySetDelimiter(
    self      : DirectoryT;
    delimiter : CHAR) =
  BEGIN
    <* ASSERT self.valid *>
    self.ShadowDirectory.setDelimiter(delimiter);
  END methodDirectorySetDelimiter;

PROCEDURE methodDirectorySize(
    self : DirectoryT) : CARDINAL =
  BEGIN
    <* ASSERT self.valid *>
    RETURN self.ShadowDirectory.size();
  END methodDirectorySize;

PROCEDURE methodDirectoryIterate(
    self : DirectoryT) : NameServer.Iterator =
  VAR
    i : NameServer.Iterator;
  BEGIN
    <* ASSERT self.valid *>
    IO.Put("In LruWt.methodDirectoryIterate.\n");
    i := self.ShadowDirectory.iterate();
    RETURN NEW(IteratorT).init(i);
  END methodDirectoryIterate;

PROCEDURE methodDirectoryInit(
    self   : DirectoryT;
    root   : Directory.T): DirectoryT =
  BEGIN
    IO.Put("Initializing LRU-WT directory.\n");
    self.id := FileId.Inc(FileIdStart);
    self.valid := FALSE;
    RETURN Directory.T.init(self, root);
  END methodDirectoryInit;

PROCEDURE methodMakeShadow(
    self : DirectoryT;
    shadow : Directory.T) =
  BEGIN
    IO.Put("Attaching LRU-WT directory to its shadow.\n");
    self.ShadowDirectory := shadow;
    self.valid := TRUE;
  END methodMakeShadow;

PROCEDURE methodDirectoryCreate(
    self : DirectoryT;
    name : NameServer.Name;
    <*UNUSED*>root : NameServer.TBase) : NameServer.TBase =
  VAR
    dir : DirectoryT;
    newShadow : NameServer.TBase;
  BEGIN
    IO.Put("Creating new LRU-WT directory.\n");
    <* ASSERT self.valid *>
    (* First, tell our shadow directory to create a directory. *)

    TRY
      newShadow := self.ShadowDirectory.create(name);
      TYPECASE newShadow OF
      | Directory.T(shadow) =>
        (* Success. Now, we create a mirror structure for this. *)
        dir := NEW(DirectoryT).init(FileSystem.GetRoot());
        dir.makeShadow(shadow);
        self.attach(name, dir);
        Directory.T.attach(dir, ".", dir);
        Directory.T.attach(dir, "..", self);
      ELSE
        (* Bug if we get here!! *)
        <* ASSERT FALSE *>
      END;
      RETURN dir;
    EXCEPT
    | NameServer.Error =>
      IO.Put("Am I not allowed to raise any errors from here??\n");
      RETURN NIL;
    END;
  END methodDirectoryCreate;

PROCEDURE methodDirectoryMkfile(
    self : DirectoryT;
    name : NameServer.Name): File.TPublic =
  VAR
    fp : FileT;
    shadowFile : File.T;
  BEGIN
    IO.Put("Making a LRU-WT file.\n");
    <* ASSERT self.valid *>
    TRY
      (* First, tell our shadow directory to make a file. *)
      shadowFile := self.ShadowDirectory.mkfile(name);
      (* Our shadow directory created newfile.  Now we create its mirror. *)
      fp := NEW(FileT).init(shadowFile);
      fp.id := FileId.Inc(FileIdStart);
      self.attach(name, fp);
      RETURN fp;
    EXCEPT
    | NameServer.Error =>
      (* I wish I could propogate an error along... *)
      RETURN NIL;
    END
  END methodDirectoryMkfile;
    
(* -------------------- FILE SERVICES -------------------- *)

REVEAL FileT = File.T BRANDED OBJECT
  (* We shadow a File.T from another filesystem. *)
  (* ShadowFile is that File.T *)
  ShadowFile: File.T;
METHODS
  init(shadow: File.T): FileT := methodFileInit;
OVERRIDES
  read     := methodFileRead;
  readRef  := methodFileReadRef;
  write    := methodFileWrite;
  writeRef := methodFileWriteRef;
  close    := methodFileClose;
  open     := methodFileOpen;
  stat     := methodFileStat;
END;

PROCEDURE methodFileInit(
    self   : FileT;
    shadow : File.T) : FileT =
  BEGIN
    self.ShadowFile := shadow;
    RETURN self;
  END methodFileInit;

PROCEDURE methodFileRead(
    self     : FileT;
    offset   : File.OffsetT;
    VAR data : ARRAY OF CHAR) : CARDINAL
  RAISES {Error.E} =
  BEGIN
    RETURN self.ShadowFile.read(offset, data);
  END methodFileRead;

PROCEDURE methodFileReadRef(
    self              : FileT;
    recnum            : File.OffsetT;
    VAR bytes         : CARDINAL;      (* how many to read, how many got *)
    VAR data          : REF ARRAY OF CHAR;
    VAR from          : CARDINAL)
  RAISES {Error.E} =
  BEGIN
    self.ShadowFile.readRef(recnum, bytes, data, from);
  END methodFileReadRef;

PROCEDURE methodFileWrite(
    self          : FileT;
    offset        : File.OffsetT;
    READONLY data : ARRAY OF CHAR) : CARDINAL
  RAISES {Error.E} =
  BEGIN
    RETURN self.ShadowFile.write(offset, data);
  END methodFileWrite;

PROCEDURE methodFileWriteRef(
    self      : FileT;
    offset    : File.OffsetT;
    VAR bytes : CARDINAL;
    data      : REF ARRAY OF CHAR;
    iOffset   : CARDINAL)
  RAISES {Error.E} =
  BEGIN
    self.ShadowFile.writeRef(offset, bytes, data, iOffset);
  END methodFileWriteRef;

PROCEDURE methodFileOpen(
    self : FileT;
    mode : INTEGER) : File.T
  RAISES {Error.E} =
  VAR
    fp : File.T;
  BEGIN
    fp := self.ShadowFile.open(mode);
    RETURN self;
  END methodFileOpen;

PROCEDURE methodFileClose(
    self : FileT;
    ) 
  RAISES {Error.E} =
  BEGIN
    self.ShadowFile.close();
  END methodFileClose;

PROCEDURE methodFileStat(
    self     : FileT;
    VAR stat : FileStat.T) 
  RAISES {Error.E} =
  BEGIN
    self.ShadowFile.stat(stat);
  END methodFileStat;


(* -------------------- MOUNT SUPPORT -------------------- *)
TYPE MountT = DirectoryT; (* At the moment, the same as a directory *)

(* -------------------- FILESYSTEM SUPPORT -------------------- *)

REVEAL FileSystemT = FileSystem.T BRANDED OBJECT
OVERRIDES
  create := methodFileSystemCreate;
  attach := methodFileSystemAttach;
  detach := methodFileSystemDetach;
END;

(* This is called to create a new mount point *)
PROCEDURE methodFileSystemCreate(
    self: FileSystemT;
    name: NameServer.Name;
    parent: NameServer.TBase) : NameServer.TBase =
  VAR
    mp : MountT;
  BEGIN
    TRY
      mp := NEW(MountT).init(FileSystem.GetRoot());
      Directory.T.attach(mp, ".", mp);
      Directory.T.attach(mp, "..", parent);
      self.attach(name, mp);
      RETURN mp;
    EXCEPT
    | NameServer.Error =>
      RETURN NIL;
    END;
  END methodFileSystemCreate;

PROCEDURE methodFileSystemAttach(
    self: FileSystemT;
    name: NameServer.Name;
    obj : REFANY) RAISES {NameServer.Error} =
  VAR
    shadowFs : REFANY;
    component : NameServer.Name;
    parent    : NameServer.TBase;
  BEGIN
    (* mount... *)
    TYPECASE obj OF
    | MountT(mp) =>
      IO.Put("Attempting to make " & name & " a shadow directory.\n");
      shadowFs := FileSystem.GetRoot().lookup(name, component, parent);
      TYPECASE shadowFs OF
      | NULL =>
        IO.Put(name&" must be an existing directory.\n");
        RAISE NameServer.Error(NameServer.EC.InvalidName);
      | Directory.T(shadowDir) =>
        IO.Put("Lookup successful.  Component is "&component&".\n");
        mp.makeShadow(shadowDir);
      ELSE
        IO.Put(name& " must be an existing directory.\n");
        RAISE NameServer.Error(NameServer.EC.InvalidName);
      END;
      FileSystem.T.attach(self, name, mp);
    ELSE
      (* Bug if we get here *)
      <* ASSERT FALSE *>
    END;
  END methodFileSystemAttach;

PROCEDURE methodFileSystemDetach(
    self    : FileSystemT;
    name    : NameServer.Name;
    VAR obj : REFANY)
  RAISES {NameServer.Error} =
  BEGIN
    TYPECASE obj OF
    | MountT (* (mp) *) =>
      FileSystem.T.detach(self, name, obj);
    ELSE
      (* Bug if we get here *)
      <* ASSERT FALSE *>
    END;
  END methodFileSystemDetach;


(* -------------------- FILESYSTEM REGISTRATION -------------------- *)

PROCEDURE Init() =
  BEGIN
    TRY
      WITH new = NEW(FileSystemT).init(NIL) DO
        FileSystem.Register("lruwt", new);
      END;
      EVAL LruWtInterface.Export(NIL);
      IO.Put("LRU-WT filesystem initialized.\n");
    EXCEPT
    | Error.E(e) =>
      IO.Put(e.message()&" during LRU-WT filesystem initialization.\n");
    END;
  END Init;

BEGIN
  Init();
END LruWt.


(* 
 * HISTORY
 * 02-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Got rid of FileId.Get in lookup.
 * 22-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NameServer.Name representation.
 * 09-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE Directory;
IMPORT DirectoryRep;
IMPORT NameServer, FileSystem, FileId, File, IO; <* NOWARN *>
IMPORT FileStat;
IMPORT Error;
IMPORT FSRoot;

REVEAL T = DirectoryRep.Public BRANDED OBJECT
  OVERRIDES
    lookup := Lookup;
    attach := Attach;
    detach := Detach;
    create := methodCreate;
    stat   := Stat;
    mkfile := methodMkfile;
    root := Root;
  END;

PROCEDURE methodCreate(self : T;
		       name : NameServer.Name): NameServer.T
  RAISES {NameServer.Error}= 
  VAR new : T;
  BEGIN
    new := NEW(T).init();
    self.attach(name, new);
    RETURN new;
  END methodCreate;
  
PROCEDURE Lookup (self: T; 
		  VAR name: NameServer.Name; 
		  getalias: BOOLEAN): REFANY RAISES {NameServer.Error} = 
  VAR entry: REFANY;
    component: NameServer.Name;
  BEGIN
    (* get filesystem mapping *)
    NameServer.GetComponent(name, component);
    
    IF component.end - component.from = 2
      AND component.str[component.from] = '.'
      AND component.str[component.from+1] = '.'
      AND self.attr = Attr.MountPoint THEN
      (* Special case: if ".." is looked up at the mount point, then
	 we actually look up from mount shadow. *)
      self := self.mountInfo.shadow;
    END;
    
    entry := NameServer.Default.lookup(self, component, getalias);
    TYPECASE entry OF
    | T(dir) =>
      IF dir.attr = Attr.MountShadow THEN
	(* Crossed the mount point here. *)
	RETURN dir.mountInfo.root;
      END;
    ELSE
    END;
    RETURN entry;
  END Lookup;

PROCEDURE Attach(self : T; 
		 READONLY name : NameServer.Name; 
		 obj: REFANY) RAISES {NameServer.Error} = 
  BEGIN
    (* add the {name,obj} association in the nameserver table *)
    NameServer.Default.attach(self,name,obj);

    TYPECASE obj OF
    | NULL             =>
    | T(dir) => FileId.Put(dir.id,dir);
    | File.T(file)     => FileId.Put(file.id,file);
    ELSE
    END;
  END Attach;

PROCEDURE Detach(self: T; 
		 READONLY name: NameServer.Name)
    RAISES {NameServer.Error} =
  VAR
    namex := name;
    obj := self.lookup(namex);
  BEGIN
    NameServer.Default.detach(self, name);

    TYPECASE obj OF
    | NULL         =>
    | T(dir)       => EVAL FileId.Delete(dir.id);
    | File.T(file) => EVAL FileId.Delete(file.id);
    ELSE
    END;
  END Detach;

PROCEDURE methodMkfile(
    <*UNUSED*> self:T;
    <*UNUSED*> name : NameServer.Name): File.T =
  BEGIN
    RETURN NIL;
  END methodMkfile;
  
PROCEDURE Stat (self: T; VAR s: FileStat.T) =
  BEGIN
    (* This is a hackup. You need to provide more accurate
     one if possible. *)
    s.dev := 0;
    s.ino := 0; (* XXX this is a problem!!! *)
    s.mode  := 8_0040777; (* XXX (mef) hack to set directory mode *)
    s.nlink := 2; (* XXX this is a problem!!! *)
    s.uid := 0;
    s.gid := 0;
    s.rdev := 0;
    s.size := self.size();
    s.atime := 0;
    s.mtime := 0;
    s.ctime := 0;
    s.blksize := 8192;
    s.blocks := s.size DIV s.blksize + 1;
    s.flags := 0;
    s.gen := 0;
  END Stat;
  
PROCEDURE Root (<*UNUSED*>self: T): FSRoot.T RAISES {Error.E} =
  BEGIN
    RAISE Error.E(NEW(File.ErrorT).init(File.FS_NOT_SUPPORTED));
  END Root;
  
BEGIN
END Directory.

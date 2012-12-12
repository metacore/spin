(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 *
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Switched over to new security manager.
 *
 * 25-Jun-97  Marc Fiuczynski (mef) at the University of Washington
 *	Changed Mount procedure to use root uid,gid instead of mine.
 *	This should fix the problem that some NFS servers wont allow non
 *	root user mount.
 *
 * 31-May-97  David Becker at the University of Washington
 *      New Errno interface
 *
 * 27-May-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added support for DTE access checks to both NFS directory
 *      and file operations. In the course, made directory and file
 *      initialization safe (only init iff not inited before) and
 *      removed UnresolvedNameT (it is not used anymore).
 *      Note that DirectoryGetEntries currently has no way of
 *      signalling which is bad.
 *
 * 08-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	All FileT and stuff points to MountT. This is needed to implement
 *	statfs systemcall.
 * 06-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to use getEntries instead of iterate.
 * 17-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Stat now sets blksize and blocks
 *
 * 20-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Fetch system variables for hostnamefull.
 *
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Getting hostname from Glob environment.
 *	Changed DirRead size to 1024 so that 
 *	Transarc AFS/NFS translator doesn't break.
 *
 * 21-Jun-96  Trent Piepho (tap) at the University of Washington
 *      Fixed the cases previously fixed on 13th for real.  Handle the
 *      stale handle problems the handle cache introduces.  Made it
 *      work with large block sizes.  Fixed write RPC stub to not need
 *      an extra copy.
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *      Fixed case of symlink deref when the symlink start with "/".
 *      We probably need to do something about symlinks containing "."
 *      and "..".
 *  
 * 11-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *      Added simple directory cache support.
 *
 * 10-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *      Fixed Read() function to read end of file correctly.
 *
 * 08-Jun-96  Trent Piepho (tap) at the University of Washington
 *      Implemented directory reading, symlink dereferencing, and 
 *      writing/creating files.
 *      
 * 11-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *      Created.
 *)

(* "Nfs" implements the network file system client interface *)

UNSAFE MODULE Nfs;

IMPORT Error, File, Directory, FileStat, FileSystem, Errno, IO, Fmt,
       Text, RPC, RPCOS, RPCSun, RPCAuth, RPCAuth_x, XDRMem,
       NfsMountProt, NfsProt, Glob, Shell, Thread, NameServer, FileId,
       NfsInterface;
IMPORT Strand;
IMPORT NSName;
IMPORT AccessMode, SecurityManager, SIDMap;
IMPORT Dispatcher;
IMPORT Ctypes; <*NOWARN*> 
IMPORT CharArray;

<*FATAL Thread.Alerted *>


CONST
  myUID = 10683;
  myGID = 1069;
  READ     = AccessMode.SimpleT{ AccessMode.READ  };
  WRITE    = AccessMode.SimpleT{ AccessMode.WRITE };
  AsynchWrite = TRUE; (* background write mode, a la biod *)
			 
(* ------------------- FILEID SUPPORT ------------------- *)

VAR FileIdStart := FileId.T{'n', 'f', 's', '\000',..};

(* ------------------- DIRECTORY SERVICES ------------------- *)

(* The "DirectoryT" defines what a directory looks like for this
   filesystem.  A DirectoryT is a subtype of fscore's Directory.T *)
TYPE VnodeT = RECORD
  gotAttr: BOOLEAN;
  mu: MUTEX;
  mp: MountT; (* root directory. *)
  attr: NfsProt.fattr; (* file attributes *)
  file: NfsProt.nfs_fh; (* NFS file handle *)
  name: NameServer.Name;
  parent: DirectoryT;
END;
  
REVEAL DirectoryT = Directory.T BRANDED OBJECT
    vnode: VnodeT;
    mnode: SIDMap.T; (* Pointer to the corresponding SID mapping *)
  METHODS
    init(id: FileId.T): DirectoryT := DirectoryInit;
  OVERRIDES
    create  := DirectoryCreate;
    lookup  := Lookup;
    attach  := Attach;
    detach  := Detach;
    mkfile  := Mkfile;
    getEntries := DirectoryGetEntries;
  END;

PROCEDURE DirectoryInit (self: DirectoryT; id: FileId.T): DirectoryT =
  BEGIN
    (* Make sure we only initialize once *)
    IF self.vnode.mu = NIL THEN
      self.id       := id;
      self.vnode.mu := NEW(MUTEX);
      EVAL Directory.T.init(self);
    END;
    RETURN self;
  END DirectoryInit;

PROCEDURE MakeEntry(
    self            : DirectoryT;
    name            : NameServer.Name;
    READONLY attr   : NfsProt.fattr;
    READONLY nfs_fh : NfsProt.nfs_fh): REFANY = 
  BEGIN
    CASE attr.type OF 
    | NfsProt.ftype_NFNON, NfsProt.ftype_NFREG =>
      VAR fp: FileT;
      BEGIN
        fp := NEW(FileT).init();
        fp.vnode.parent := self;
        fp.vnode.attr   := attr;
        fp.vnode.file   := nfs_fh;
        fp.vnode.name   := NSName.DeepCopy(name);
        fp.vnode.mp     := self.vnode.mp;
        RETURN fp;
      END;
    | NfsProt.ftype_NFDIR =>
      VAR dir: DirectoryT;
      BEGIN
	dir := NEW(DirectoryT).init(nfs_fh.data);
        dir.vnode.mp     := self.vnode.mp;
        dir.vnode.parent := self;
        dir.vnode.attr   := attr;
        dir.vnode.file   := nfs_fh;
        dir.vnode.name   := NSName.DeepCopy(name);
        RETURN dir;
      END;
    | NfsProt.ftype_NFLNK =>
      VAR link: LinkT;
      BEGIN
        link := NEW(LinkT).init();
        link.vnode.parent := self;
        link.vnode.attr   := attr;
        link.vnode.file   := nfs_fh;
        link.vnode.mp     := self.vnode.mp;
        link.vnode.name   := NSName.DeepCopy(name);
        RETURN link;
      END;
    | NfsProt.ftype_NFBLK, NfsProt.ftype_NFCHR =>
      IO.PutError("NFS: special file ");
      IO.Put(NSName.ToText(name));
      IO.Put(" not supported.\n");
      RETURN NIL;
    | NfsProt.ftype_NFSOCK,NfsProt.ftype_NFFIFO =>
      IO.PutError("NFS: socket/fifo file ");
      IO.Put(NSName.ToText(name));
      IO.Put(" not supported.\n");
      RETURN NIL;
    | NfsProt.ftype_NFBAD =>
      IO.PutError("NFS: got bad file type for ");
      IO.Put(NSName.ToText(name));
      IO.Put("\n");
      RETURN NIL;
    END;
  END MakeEntry;

PROCEDURE DirectoryCreate(self: DirectoryT; 
			  name: NameServer.Name) : NameServer.T = 
  VAR dir: DirectoryT;
  BEGIN
    dir := NEW(DirectoryT).init(FileId.Inc(FileIdStart));
    (* other fields are initialized in attach *)
    TRY
      (* Note that access check is done in attach *)
      self.attach(name, dir);
    EXCEPT 
    | NameServer.Error =>
      RETURN NIL;
    END;
    RETURN dir;
  END DirectoryCreate;

PROCEDURE Lookup (self: DirectoryT;
		  VAR name: NameServer.Name;
		  dontFollowSymlink: BOOLEAN): REFANY 
  RAISES {NameServer.Error} = 
  VAR
    component, tmp: NameServer.Name;
    entry: REFANY;
  BEGIN
    (* Optional access check *)
    IF SIDMap.UseMe THEN
      IF NOT SecurityManager.CheckSimplePermissions(self.mnode.sid,
                                                    READ)    THEN
        RAISE NameServer.Error(NameServer.EC.Unauthorized);
      END;
    END;

    (* check if we already know about this file *)
    TRY
      tmp := name; (* save in case of NameNotFound error *)
      entry := Directory.T.lookup(self, tmp, dontFollowSymlink);
      name := tmp;
    EXCEPT
    | NameServer.Error(ec) =>
      IF ec = NameServer.EC.NameNotFound THEN
	(* check if the file is on the server *)
	NameServer.GetComponent(name, component);
	entry := NfsLookup(self,component);
	IF entry # NIL THEN 
	  (* place entry in in-core directory table *)
	  Directory.T.attach(self,component,entry);
	ELSE
	  RAISE NameServer.Error(NameServer.EC.NameNotFound);
	END;
      END;
    END;

    IF SIDMap.UseMe THEN
      TYPECASE entry OF
      (* Special-case NFS directories for DTE mapping *)
      | DirectoryT(dir) =>
	(* Fix access control mapping if necessary *)
	IF dir.mnode = NIL THEN
	  <*ASSERT self.mnode#NIL*>
	  dir.mnode := SIDMap.WalkNode(NSName.ToText(component),
				       self.mnode, TRUE);
	  (* IO.Put("*** Added mapping for " & component & "\n");
	     IO.Put("    sid is " & Fmt.Int(dir.mnode.sid)
	     & "\n"); *)
	END;
      | FileT(file) =>
	(* Special-case NFS files for SID mapping *)
	IF file.mnode = NIL THEN
	  <*ASSERT self.mnode#NIL*>
	  file.mnode := SIDMap.WalkNode(NSName.ToText(component),
					self.mnode,FALSE);
	  IF file.mnode = NIL THEN file.mnode := self.mnode; END;
	  (* IO.Put("*** Added file " & component & " into mapping\n");
	     IO.Put("    sid is " & Fmt.Int(file.mnode.sid)
	     & "\n"); *)
	END;
      ELSE
	(* nothing to do *)
      END;
    END;
    RETURN entry;
  END Lookup;

PROCEDURE Attach (self: DirectoryT;
		  READONLY name: NameServer.Name;
		  obj: REFANY) RAISES {NameServer.Error} = 
  VAR
    args: NfsProt.createargs;
    res: NfsProt.diropres;

  PROCEDURE Ok(
      VAR vnode: VnodeT; 
      READONLY file: NfsProt.nfs_fh; 
      READONLY attr: NfsProt.fattr;
      mp : MountT) =
    BEGIN
      LOCK vnode.mu DO
	vnode.gotAttr := TRUE;
        vnode.attr := attr;
        vnode.file := file;
        vnode.mp := mp;
        vnode.parent := self;
	vnode.name := NSName.DeepCopy(name);
      END;
    END Ok;

  BEGIN
    TYPECASE obj OF 
    | NULL =>
      RAISE NameServer.Error(NameServer.EC.Panic);
    | FileT(file) =>
      IF SIDMap.UseMe THEN
        (* Check access rights *)
        IF NOT SecurityManager.CheckSimplePermissions( self.mnode.sid,
                                                       WRITE )  THEN
          RAISE NameServer.Error(NameServer.EC.Unauthorized);
        END;
        (* Add link to SID map *)
        <*ASSERT self.mnode#NIL*>
        file.mnode := SIDMap.WalkNode(NSName.ToText(name),
                                      self.mnode, FALSE);
        IF file.mnode = NIL THEN file.mnode := self.mnode; END;
      END;

      args.where.name := NSName.ToText(name);
      args.attributes.mode := 8_666; (* XXX fix this *)
      args.attributes.uid := myUID;
      args.attributes.gid := myGID;
      args.attributes.size := 0;

      LOOP
	args.where.dir := self.vnode.file;
	
	TRY
	  res := self.vnode.mp.rpc.NFSPROC_CREATE(args);
	EXCEPT
	| RPC.Failed (e) =>
	  dprint("RPC failure: " & e.info & "\n");
	  RAISE NameServer.Error(NameServer.EC.Panic);
	END;
	
	CASE res.status OF
	| NfsProt.nfsstat_NFS_OK =>
	  WITH resOK = NARROW(res, NfsProt.diropres_NFS_OK).diropres DO
	    Ok(file.vnode, resOK.file, resOK.attributes, self.vnode.mp);
	  END;
	  EXIT; (* retry loop *)
	| NfsProt.nfsstat_NFSERR_STALE =>
	  (* resolve stale nfs_fh and redo nfs operation *)
	  NfsStaleLookup(self.vnode);
	ELSE
	  dprint(Errno.Fmt(res.status) &
		 ", errno=" & Fmt.Int(res.status) & "\n");
	  RAISE NameServer.Error(NameServer.EC.Panic);
	END;

      END;
    | DirectoryT(dir) =>
      IF SIDMap.UseMe THEN
        (* Check access rights *)
        IF NOT SecurityManager.CheckSimplePermissions( self.mnode.sid,
                                                       WRITE )  THEN
          RAISE NameServer.Error(NameServer.EC.Unauthorized);
        END;
        (* Add link to SID map *)
        <*ASSERT self.mnode#NIL*>
        dir.mnode := SIDMap.WalkNode(NSName.ToText(name),
				     self.mnode, TRUE);
      END;

      args.where.name := NSName.ToText(name);
      args.attributes.mode := 8_777; (* XXX fix this *)
      args.attributes.uid := myUID;
      args.attributes.gid := myUID;

      LOOP
        args.where.dir                := self.vnode.file;
        TRY
          res := self.vnode.mp.rpc.NFSPROC_MKDIR(args);
        EXCEPT
        | RPC.Failed (e) =>
          dprint("RPC failure: " & e.info & "\n");
          RAISE NameServer.Error(NameServer.EC.Panic);
        END;

        CASE res.status OF
        | NfsProt.nfsstat_NFS_OK =>
          WITH resOK = NARROW(res,NfsProt.diropres_NFS_OK).diropres DO
	    dir.vnode.name := NSName.DeepCopy(name);
	    Ok(dir.vnode, resOK.file, resOK.attributes, self.vnode.mp);
          END;
          EXIT; (* retry loop *)
        | NfsProt.nfsstat_NFSERR_STALE =>
          (* resolve stale nfs_fh and redo nfs operation *)
          NfsStaleLookup(self.vnode);
        ELSE
          dprint(Errno.Fmt(res.status) &
            ", errno=" & Fmt.Int(res.status) & "\n");
            RAISE NameServer.Error(NameServer.EC.Panic);
        END;
      END;
    ELSE
      RAISE NameServer.Error(NameServer.EC.Panic);
    END;
    (* Directory.T.attach(self, name, obj); *)
  END Attach;

PROCEDURE Detach (self: DirectoryT; READONLY name: NameServer.Name)
  RAISES {NameServer.Error} =
  VAR
    args: NfsProt.diropargs;
    res: NfsProt.nfsstat;
  BEGIN

    (* remove the entry from our lousy cache. *)
    TRY
      Directory.T.detach(self, name);
    EXCEPT
    ELSE
      (* Exception may happen when the name is not in cache.
	 Simply ignore it.*)
    END;
    
    args.name := NSName.ToText(name);
    args.dir := self.vnode.file;
    TRY
      res := self.vnode.mp.rpc.NFSPROC_REMOVE(args);
    EXCEPT
    | RPC.Failed (e) =>
      IO.PutError("Nfs.Remove: " & e.info & "\n");
      RAISE NameServer.Error(NameServer.EC.NameNotFound);
    END;
    
    IF res # NfsProt.nfsstat_NFS_OK THEN
      IO.PutError("Nfs.Detach: " & Errno.Fmt(res) & ".\n");
    END;
  END Detach;

PROCEDURE Mkfile (self: DirectoryT; name: NameServer.Name): File.T =
  VAR
    fp : FileT;
  BEGIN
    fp := NEW(FileT).init();
    (* other fields are initialized in attach *)
    fp.id := FileId.Inc(FileIdStart);
    (* Note that access check is done in attach *)
    self.attach(name, fp);
    RETURN fp;
  END Mkfile;

FUNCTIONAL
PROCEDURE NfsCookieToNSCookie (c: NfsProt.nfscookie): NameServer.Cookie =
  BEGIN
    RETURN VIEW(c, Ctypes.unsigned_int);
  END NfsCookieToNSCookie;

FUNCTIONAL
PROCEDURE NSCookieToNfsCookie (c: NameServer.Cookie): NfsProt.nfscookie =
  VAR i : Ctypes.unsigned_int := c;
  BEGIN
    <*ASSERT BYTESIZE(c) = BYTESIZE(NameServer.Cookie)*>
    RETURN VIEW(i, NfsProt.nfscookie);
  END NSCookieToNfsCookie;
  
  
PROCEDURE DirectoryGetEntries(
    self: DirectoryT;
    nsCookie: NameServer.Cookie;
    VAR ent: ARRAY OF NameServer.Entry): CARDINAL =
  TYPE
    Res   = NfsProt.readdirres;
    ResOK = NfsProt.readdirres_NFS_OK;
    Args  = NfsProt.readdirargs;
  VAR
    nEnt: CARDINAL := 0;
    cookie: NfsProt.nfscookie;
    res : Res;
    args: Args;
  BEGIN
    IF SIDMap.UseMe THEN
      IF NOT SecurityManager.CheckSimplePermissions( self.mnode.sid,
                                                     READ )   THEN
        (* We should really raise here,
         * but would have to change NameServer.i3,
         * so we don't *)
        RETURN nEnt; (* Nothing for the caller *)
      END;
    END;
    IO.Put("nfsreaddir: " & Fmt.Int(nsCookie, 16) & ".\n");
    cookie := NSCookieToNfsCookie(nsCookie);
    
    (* Set up the directory operation arguments. *)
    args.dir    := self.vnode.file;
    args.count  := 1024; (* no smaller than this or Transarc translator breaks *)

    LOOP
      args.cookie := cookie;

      TRY 
        (* Look up a file name *)
        res := self.vnode.mp.rpc.NFSPROC_READDIR(args);
      EXCEPT
      | RPC.Failed (e) =>
        IO.PutError("RPC failure: " & e.info & "\n");
      END;

      CASE res.status OF
      | NfsProt.nfsstat_NFS_OK =>
        WITH resOK = NARROW(res,ResOK) DO
          VAR entry : REF NfsProt.entry;
          BEGIN
            entry := resOK.reply.entries;
            WHILE entry # NIL AND nEnt <= LAST(ent) DO
	      ent[nEnt].name := NSName.FromText(entry.name);
	      ent[nEnt].cookie := NfsCookieToNSCookie(entry.cookie);
	      cookie := entry.cookie;
	      VIEW(ent[nEnt].id, Ctypes.unsigned_int) := entry.fileid;
	      INC(nEnt);
	      entry := entry.nextentry;
	    END;
          END;
          (* reached the end of the directory *)
	  IF nEnt > LAST(ent) THEN EXIT; END;
          IF resOK.reply.eof THEN EXIT; END;
        END;
      | NfsProt.nfsstat_NFSERR_STALE =>
        IO.PutError(Errno.Fmt(res.status));
        IO.Put(", errno=");
        IO.PutInt(res.status);
        IO.Put(" NEED TO DO LOCAL INVALIDATE\n");
        EXIT;
      ELSE
        IO.PutError(Errno.Fmt(res.status));
        IO.Put(", errno=");
        IO.PutInt(res.status);
        IO.Put("\n");
        EXIT;
      END;
    END;
    RETURN nEnt;
  END DirectoryGetEntries;

(* ------------------- SYMLINK SERVICES ------------------- *)

TYPE LinkT = NameServer.Alias OBJECT
    vnode     : VnodeT;
  METHODS
    init(): LinkT := LinkInit;
  OVERRIDES
    getObject := LinkGetObject;
  END;

PROCEDURE LinkInit(self: LinkT): LinkT =
  BEGIN
    self.vnode.mu := NEW(MUTEX);
    RETURN self;
  END LinkInit; 


PROCEDURE LinkGetObject(self : LinkT): REFANY = 
  BEGIN
    TRY
      RETURN FileSystem.Lookup(self.vnode.parent, NfsReadLink(self.vnode));
    EXCEPT 
    | Error.E(e) =>
      IO.PutError("NFS linkgetobject failed with ");
      IO.Put(e.message());
      IO.Put("\n");
      RETURN NIL;
    | NameServer.Error =>
      RETURN NIL;
    END;
  END LinkGetObject;

(* ------------------- FILE SERVICES ------------------- *)

(* The "FileT" defines file services for this filesystem.  A
   FileT is a subtype of fscore's File.T. *)

REVEAL FileT = File.T BRANDED OBJECT
    vnode : VnodeT;
    mnode : SIDMap.T; (* Pointer to the corresponding SID mapping *)
    mode  : INTEGER;
  METHODS
    init(): FileT := FileInit;
  OVERRIDES
    read     := FileRead;
    write    := FileWrite;
    writeRef := WriteRef;
    close    := FileClose;
    open     := FileOpen;
    stat     := MethodFileStat;
  END;

PROCEDURE FileInit(self: FileT): FileT = 
  BEGIN
    (* Make sure we only initialize once *)
    IF self.vnode.mu = NIL THEN
      self.vnode.mu := NEW(MUTEX);
    END;
    RETURN self;
  END FileInit;

PROCEDURE commonRead(
    READONLY vnode     : VnodeT;
    offset   : File.OffsetT;
    VAR data : ARRAY OF CHAR): CARDINAL
  RAISES {Error.E} = 
  VAR
    nread : CARDINAL;
    want  : CARDINAL;
  BEGIN
    (* compute max bytes that will fit into the data buffer. *)
    nread := NUMBER(data);
    want  := nread;

    CASE vnode.attr.type OF
    | NfsProt.ftype_NFNON, NfsProt.ftype_NFREG =>
      NfsReadFile(vnode,offset, want, data);
      (* update nread to the actual number of bytes read from the NFS server *)
      RETURN nread - want;
    ELSE
      IO.PutError("NFS commonRead trying to read non file/link.\n");
      RETURN 0;
    END;
  END commonRead;

PROCEDURE FileRead (self: FileT;
		   VAR data: ARRAY OF CHAR;
		   offset: File.OffsetT): CARDINAL 
  RAISES {Error.E}= 
  VAR 
    bytes : INTEGER;
  BEGIN
    (* Authentication check *)
    IF SIDMap.UseMe THEN
      IF NOT SecurityManager.CheckSimplePermissions( self.mnode.sid,
                                                     READ )   THEN
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_UNAUTHORIZED));
      END;
    END;

    bytes := MIN(NUMBER(data),self.vnode.attr.size-offset);
    (* sanity check *)
    IF bytes <= 0 OR
      offset > self.vnode.attr.size THEN
      RETURN 0;
    END;

    WITH nfsdata = SUBARRAY(data,0,bytes) DO
      bytes := commonRead(self.vnode,offset,nfsdata);
    END;
    RETURN bytes;
  END FileRead;

VAR asynchAlias: PROCEDURE (self: FileT;
			    buf: REF ARRAY OF CHAR;
			    bytes: CARDINAL;
			    offset: File.OffsetT;
			    from: CARDINAL);

PROCEDURE AsynchWriteThread (self: FileT;
			     buf: REF ARRAY OF CHAR;
			     bytes: CARDINAL;
			     offset: File.OffsetT;
			     from: CARDINAL) =
  VAR x: CARDINAL;
  BEGIN
    TRY
      x := FileWrite(self, SUBARRAY(buf^, from, bytes), offset);
      IF x < bytes THEN
	IO.PutError("Nfs.AsynchWrite: write partially failed ("
		    & Fmt.Int(x) & ") bytes.\n");
      END;
    EXCEPT
    | Error.E(e) =>
      IO.PutError("Nfs.AsynchWrite: " & e.message());
    END;
    CharArray.Free(buf);
  END AsynchWriteThread;
  
PROCEDURE WriteRef (self: FileT;
		    buf: REF ARRAY OF CHAR;
		    bytes: CARDINAL;
		    offset: File.OffsetT;
		    from: CARDINAL;
		    VAR retain: BOOLEAN): CARDINAL RAISES {Error.E} =
  BEGIN
    IF AsynchWrite THEN
      retain := TRUE;
      asynchAlias(self, buf, bytes, offset, from);
      Strand.Yield();
      RETURN bytes;
    ELSE
      retain := FALSE;
      RETURN FileWrite(self, SUBARRAY(buf^, from, bytes), offset);
    END;
  END WriteRef;
  
PROCEDURE FileWrite (self: FileT;
		     READONLY data: ARRAY OF CHAR;
		     offset: File.OffsetT): CARDINAL RAISES {Error.E} =

  VAR
    res   : NfsProt.attrstat;
    args  : NfsProt.writeargs;
    index : CARDINAL; (* index into 'data' buffer *)
    left  : CARDINAL; (* bytes left to be sent *)
    bs    : CARDINAL; (* block size *)
  BEGIN
    (* Authentication check *)
    IF SIDMap.UseMe THEN
      IF NOT SecurityManager.CheckSimplePermissions( self.mnode.sid,
                                                     WRITE )  THEN
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_UNAUTHORIZED));
      END;
    END;

    (* set the NFS file handler *)
    args.file := self.vnode.file;

    (* args.beginoffset := 0; ignored by server *)
    (* args.totalcount := 0; ignored by server *)

    index := FIRST(data);
    left := NUMBER(data);

    (* loop to write out data in < wSize chunks *)
    WHILE left > 0 DO
      (* offset into the file *)
      args.offset := offset;
      (* args.data := NIL;  ignored by server *)

      (* compute size of chunk to be sent to server *)
      bs := MIN(left, wSize);

      TRY 
        (* send buffer to server *)
	res := self.vnode.mp.rpc.NFSPROC_WRITE(args,SUBARRAY(data, index, bs),bs);
      EXCEPT
      | RPC.Failed (e) =>
	IO.PutError("Nfs.Write: " & e.info & "\n");
        EXIT;
      END;
      IF res.status # NfsProt.nfsstat_NFS_OK THEN 
        dprint(Errno.Fmt(res.status) &
          ", errno=" & Fmt.Int(res.status) & "\n");
        EXIT; (* from WHILE *)
      END;
      INC(index,bs);
      INC(offset,bs);
      DEC(left,bs);
    END;

    IF res.status = NfsProt.nfsstat_NFS_OK THEN
      WITH resOK = NARROW(res, NfsProt.attrstat_NFS_OK) DO
        self.vnode.attr := resOK.attributes;
      END;
    END; 

    RETURN NUMBER(data) - left;
  END FileWrite;

PROCEDURE FileOpen (
    self : FileT;
    mode : INTEGER): File.T
  RAISES { Error.E } <*NOWARN*> =
  TYPE
    Res   = NfsProt.attrstat;
    ResOK = NfsProt.attrstat_NFS_OK;
    Args  = NfsProt.nfs_fh;
  VAR
    res: Res;
    args: Args;
  BEGIN
    IF self.vnode.gotAttr THEN RETURN self; END;
    
    LOOP
      args := self.vnode.file;
      TRY
        (* There is no NFS open.  We'll just get attributes instead *)
        res := self.vnode.mp.rpc.NFSPROC_GETATTR(args);
      EXCEPT
      | RPC.Failed (e) =>
        IO.PutError("RPC failure: " & e.info & "\n");      
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_NO_ENTRY));
      END;
      CASE res.status OF
      | NfsProt.nfsstat_NFS_OK =>
        self.mode := mode;
	self.vnode.gotAttr := TRUE;
        WITH resOK = NARROW(res,ResOK) DO
          LOCK self.vnode.mu DO
            self.vnode.attr := resOK.attributes;
          END;
        END;
        RETURN self;
      | NfsProt.nfsstat_NFSERR_STALE =>
        (* resolve stale nfs_fh and redo nfs operation *)
        TRY
          NfsStaleLookup(self.vnode);
        EXCEPT
        | NameServer.Error =>
          RAISE Error.E(NEW(File.ErrorT).init(File.FS_NO_ENTRY));
        END;
      ELSE
        IO.PutError("NFS fileopen error: ");
        IO.Put(Errno.Fmt(res.status));
        IO.Put("\n");
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_NO_ENTRY));
      END;
    END;
  END FileOpen;

PROCEDURE FileClose (<* UNUSED *> self: FileT) 
  RAISES {Error.E} <*NOWARN*> =
  BEGIN
    (* do nothing *)
  END FileClose;

PROCEDURE MethodFileStat (self: FileT; VAR stat : FileStat.T) =
  TYPE
    Res   = NfsProt.attrstat;
    ResOK = NfsProt.attrstat_NFS_OK;
    Args  = NfsProt.nfs_fh;
  VAR
    res  : Res;
    args : Args;

  PROCEDURE Error() = 
    BEGIN
      FileStat.Init(stat);
    END Error;

  PROCEDURE Ok(VAR attr: NfsProt.fattr) = 
    BEGIN
      (* file in the stat structures *)
      stat.dev := 0;
      stat.ino := 2;
      stat.mode := attr.mode;
      stat.nlink := attr.nlink;
      stat.uid := attr.uid;
      stat.gid := attr.gid;
      stat.rdev := attr.rdev;
      stat.size := attr.size;
      stat.atime := attr.atime.seconds;
      stat.mtime := attr.mtime.seconds;
      stat.ctime := attr.ctime.seconds;
      stat.blksize := attr.blocksize;
      stat.blocks := attr.blocksize;
      stat.flags := 0;
      stat.gen := 0;
      stat.ino := attr.fileid;
    END Ok;

  BEGIN
    (* Authentication check *)
    IF SIDMap.UseMe THEN
      IF NOT SecurityManager.CheckSimplePermissions( self.mnode.sid,
                                                     READ )   THEN
        Error();
        RETURN;
      END;
    END;
    IF self.vnode.gotAttr THEN
      Ok(self.vnode.attr);
      RETURN;
    END;
    
    LOOP
      (* Set up the directory operation arguments. *)
      args := self.vnode.file;

      TRY 
        (* Get file attributes. *)
        res := self.vnode.mp.rpc.NFSPROC_GETATTR(args);
      EXCEPT
      | RPC.Failed (e) =>
        dprint("RPC failure: " & e.info & "\n");
        Error();
        RETURN;
      END;

      CASE res.status OF
      | NfsProt.nfsstat_NFS_OK =>
        WITH resOK = NARROW(res,ResOK) DO
          (* update my attributes *)
          LOCK self.vnode.mu DO
            self.vnode.attr := resOK.attributes;
          END;
          Ok(self.vnode.attr);
        END;
        EXIT;
      | NfsProt.nfsstat_NFSERR_STALE =>
        <* ASSERT FALSE *>
        (* resolve stale nfs_fh and redo nfs operation *)
      ELSE
        dprint(Errno.Fmt(res.status) &
          ", errno=" & Fmt.Int(res.status) & "\n");
        Error();
        EXIT;
      END;
    END;
  END MethodFileStat;

(* ------------------- MOUNT SERVICES ------------------- *)

(* The type "MountT" represents a mount point exported by this file
   system.  The NFS file system maintains which NFS server we've
   mounted from.  A MountT is a subtype of a DirectoryT. *)

TYPE MountT = DirectoryT OBJECT
    lock     : MUTEX;                        (* Should be a ReaderWriter lock *)
    rpc      : NfsProt.NFS_VERSIONClient; (* nfs RPC handle  *)
    mountRPC : NfsMountProt.MOUNTVERSClient; (* mount RPC handle              *)
    dirPath  : TEXT;                         (* path name to mount point      *)
    host     : RPCOS.InaddrT;
END;

(* The type "FileSystemT" is used to register the filesystem with
   fscore.  A FileSystemT is a subtype of fscore's FileSystem.T. *)

TYPE FileSystemT = FileSystem.T OBJECT
  OVERRIDES
    newfs := Newfs;
    deletefs := Deletefs;
  END;

PROCEDURE Newfs (<*UNUSED*> self : FileSystemT; 
		 READONLY name : NameServer.Name) : Directory.T = 
  CONST
    mountProgNum = NfsMountProt.MOUNTPROG_prognum;
    mountVersNum = NfsMountProt.MOUNTVERS_versnum;
    nfsProgNum   = NfsProt.NFS_PROGRAM_prognum;
    nfsVersNum   = NfsProt.NFS_VERSION_versnum;
  VAR
    bindingInfo : RPCSun.BindingInfo;
    dirPath     : TEXT;
    fhstatus    : NfsMountProt.fhstatus;
    nfs_fh      : NfsProt.nfs_fh;
    hostName    : TEXT;
    mountRPC    : NfsMountProt.MOUNTVERSClient;
    nfsRPC      : NfsProt.NFS_VERSIONClient; (* nfs RPC handle  *)
    host        : RPCOS.InaddrT;
    mp: MountT;
    
    PROCEDURE ExtractHostPath() RAISES {Error.E} =
      VAR
	colon := -1;
      BEGIN
	FOR i := name.from TO name.end-1 DO
	  IF name.str[i] = ':' THEN
	    colon := i;
	    EXIT;
	  END;
	END;
        IF colon = -1 THEN 
          dprint("Mount failure: improper nfs mount name.\n");
          RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
        END;
	hostName := Text.FromChars(SUBARRAY(name.str^, name.from,
					    colon-name.from));
        dirPath := Text.FromChars(SUBARRAY(name.str^, colon+1,
					   name.end-colon-1));
      END ExtractHostPath;

    PROCEDURE MountFailed() RAISES {Error.E} = 
      BEGIN
        (* Destroy RPC client information. *)
        mountRPC.GetClient().Destroy();
        WITH ec = fhstatus.fhs_status DO
          dprint(Errno.Fmt(ec) & ", errno=" & Fmt.Int(ec) & "\n");
          RAISE Error.E(NEW(File.ErrorT).init(ec));
        END;
      END MountFailed; 
  BEGIN
    mp := NEW(MountT).init(FileId.Inc(FileIdStart));
    TRY
      (* Given the devname extract colon separated hostName and dirPath. *)
      ExtractHostPath();
      dprint("Doing lookup on " & hostName & " with dirpath " & dirPath & "\n");

      TRY
        (* Look up the ip address of the host that we extracted from the
           mount path. *) 
        host := RPCOS.LookupHost(hostName);

        (* Set up rpc BIND information for mount protocol. *)
        bindingInfo := BindingInfo(host,0,mountProgNum,mountVersNum);
        mountRPC := NfsMountProt.ImportMOUNTVERS(bindingInfo);
      EXCEPT
      | RPC.Failed (e) =>
        dprint("RPC failure: " & e.info & "\n");
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
      END;

      (* Set up unix credentials with hostname, root {uid,gid} *)
      mountRPC.GetClient().cred := GetAuth(localhostname,0,0);

      TRY
        (* mount NFS filesystem *)
        fhstatus := mountRPC.MOUNTPROC_MNT(dirPath);
      EXCEPT
      | RPC.Failed (e) =>
        dprint("RPC failure: " & e.info & "\n");
        mountRPC.GetClient().Destroy();
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_CANT_MOUNT));
      END;

      (* Check if mount request succeed. *)
      IF fhstatus.fhs_status # 0 THEN MountFailed(); END;

      (* Record the nfs file handle to be used with subsequent nfs
         operations. *)
      WITH fhstatus_0 = NARROW(fhstatus,NfsMountProt.fhstatus_0) DO
        nfs_fh.data := fhstatus_0.fhs_fhandle;
      END;

      TRY
        bindingInfo := BindingInfo(host,0,nfsProgNum,nfsVersNum);
        nfsRPC := NfsProt.ImportNFS_VERSION(bindingInfo);
      EXCEPT
      | RPC.Failed (e) => 
        dprint("RPC failure: " & e.info & "\n");
        (* Destroy mount RPC client information. *)
        MountFailed();
      END;
      nfsRPC.GetClient().cred := GetAuth(localhostname,myUID,myGID);

      mp.lock     := NEW(MUTEX);
      mp.mountRPC := mountRPC; (* hold on to it for unmount *)
      mp.dirPath  := dirPath;
      mp.host     := host;     (* NFS server ip address *)
      mp.rpc  := nfsRPC;
      mp.vnode.mp := mp;
      mp.vnode.file := nfs_fh;   (* root file handle *)
      
      (* XXX where's attr? *)
      (* Set up link into the SID mapping *)
      IF SIDMap.UseMe THEN
        mp.mnode := SIDMap.WalkTree( SIDMap.pathDelimiter,
				     FileSystem.mountPath.t,
				     SIDMap.GetRoot());
      END;
      RETURN mp;
    EXCEPT
    | Error.E(e) =>
      IO.PutError("Nfs.Mount failed with ");
      IO.Put(e.message());
      IO.Put("\n");
      RAISE NameServer.Error(NameServer.EC.NameNotFound);
    END;
  END Newfs;

PROCEDURE Deletefs (<*UNUSED*>self: FileSystemT; root: Directory.T)
  RAISES {Error.E} =
  VAR
    mp: MountT := root;
  BEGIN
    (* go through unmount procedure for nfs. *)
    TRY
      LOCK mp.lock DO (* XXX Writer lock *)
	(* unmount NFS filesystem *)
	mp.mountRPC.MOUNTPROC_UMNT(mp.dirPath);
	mp.mountRPC.GetClient().Destroy();
      END;
    EXCEPT
    | RPC.Failed (e) =>
      IO.PutError("WARNING: RPC op failed with " & e.info & "\n");
    END;
  END Deletefs;

(* ------------------- LOW LEVEL FS SERVICES ------------------- *)
VAR
  localhostname  : TEXT;

CONST debug = FALSE;

CONST
  wSize = 8192;
  rSize = 8192;

PROCEDURE dprint (READONLY s: TEXT) =
  BEGIN
     IO.Put(s);
  END dprint;

VAR stamp : RPCOS.UINT32 := 0;
PROCEDURE GetStamp(): RPCOS.UINT32 =
  BEGIN
    INC(stamp); RETURN stamp;
  END GetStamp;

PROCEDURE GetAuth(myHostName: TEXT; uid,gid: RPCOS.UINT32): 
  RPCAuth.Credentials = 
  VAR
    auth : RPCAuth.auth_unix;
    sink : XDRMem.Sink;
    cred : RPCAuth.Credentials_1;
  BEGIN
    auth.stamp := GetStamp();
    auth.machinename := myHostName;
    auth.uid := uid;
    auth.gid := gid;
    auth.gids := NIL;
    cred := NEW(RPCAuth.Credentials_1);
    cred.flavor := RPCAuth.AUTH_UNIX;
    cred.oa := NEW(RPCAuth.opaque_auth,128);
    sink := XDRMem.NewSink(cred.oa);
    RPCAuth_x.Put_auth_unix(sink,auth);<*NOWARN*>
    RETURN cred;
  END GetAuth;


PROCEDURE BindingInfo(
    host: RPCOS.InaddrT;
    port: RPCOS.UINT16;
    progNum : RPCOS.UINT32;
    versNum : RPCOS.UINT32): RPCSun.BindingInfo = 
  VAR
    binfo : RPCSun.BindingInfo;
  BEGIN      
    binfo := RPCSun.CreateBindingInfo(
                 host, 
                 progNum,
                 versNum,
                 port, (* let portmapper do the lookup *)
                 RPCSun.Protocol.UDP);
    RETURN binfo;
  END BindingInfo;

PROCEDURE NfsRawLookup(VAR vnode: VnodeT; name: NameServer.Name) : NfsProt.diropres 
  RAISES { NameServer.Error} = 
  TYPE
    Args  = NfsProt.diropargs;
    Res   = NfsProt.diropres;
  VAR
    args : Args;
    res  : Res;
  BEGIN
    args.name := NSName.ToText(name);
    args.dir  := vnode.file;
    TRY 
      res := vnode.mp.rpc.NFSPROC_LOOKUP(args);
    EXCEPT
    | RPC.Failed (e) =>
      IO.PutError("RPC failure: " & e.info & "\n");
      RAISE NameServer.Error(NameServer.EC.NameNotFound);
    END;
    RETURN res;
  END NfsRawLookup;

PROCEDURE NfsStaleLookup(VAR vnode: VnodeT) RAISES {NameServer.Error} = 
  TYPE
    Res   = NfsProt.diropres;
  VAR
    res  : Res;
  BEGIN
    LOOP
      res := NfsRawLookup(vnode.parent.vnode, vnode.name);
      CASE res.status OF
      | NfsProt.nfsstat_NFS_OK =>
        WITH fresh = NARROW(res,NfsProt.diropres_NFS_OK) DO
          vnode.file := fresh.diropres.file;
        END;
        EXIT;
      | NfsProt.nfsstat_NFSERR_STALE =>
        NfsStaleLookup(vnode.parent.vnode); (* recurse up *)
      | NfsProt.nfsstat_NFSERR_NOENT =>
	(* its not there anymore, remove it from incore cache *)
	Directory.T.detach(vnode.parent, vnode.name);
	RAISE NameServer.Error(NameServer.EC.NameNotFound);
      ELSE
        IO.PutError("NfsStaleLookup error: ");
        IO.Put(Errno.Fmt(res.status));
        IO.Put("\n");
        RAISE NameServer.Error(NameServer.EC.NameNotFound);
      END
    END;
  END NfsStaleLookup;

PROCEDURE NfsLookup(self: DirectoryT; name: NameServer.Name): REFANY
  RAISES {NameServer.Error} = 
  TYPE
    Res   = NfsProt.diropres;
  VAR
    res  : Res;
  BEGIN
    LOOP
      res := NfsRawLookup(self.vnode,name);
      CASE res.status OF
      | NfsProt.nfsstat_NFS_OK =>
        WITH resOK = NARROW(res,NfsProt.diropres_NFS_OK) DO
          RETURN MakeEntry(self,name,
                           resOK.diropres.attributes,
                           resOK.diropres.file);
        END;
      | NfsProt.nfsstat_NFSERR_STALE =>
        NfsStaleLookup(self.vnode);
      | NfsProt.nfsstat_NFSERR_NOENT =>
        RAISE NameServer.Error(NameServer.EC.NameNotFound);
      ELSE
        IO.PutError("NfsLookup error: ");
        IO.Put(Errno.Fmt(res.status));
        IO.Put("\n");
        RAISE NameServer.Error(NameServer.EC.NameNotFound);
      END;
    END;
  END NfsLookup; 

PROCEDURE NfsReadFile(
    READONLY vnode : VnodeT;
    offset        : File.OffsetT;
    VAR want      : CARDINAL;
    VAR data      : ARRAY OF CHAR) (* total bytes requested  *)
  RAISES {Error.E} = 
  TYPE
    Res   = NfsProt.readres;
    Args  = NfsProt.readargs;
  VAR
    res  : Res;
    args : Args;
    rsize: INTEGER;
    bytes: CARDINAL;
    pos  : CARDINAL;
  BEGIN
    (* Set up the read from file arguments. *)
    args.file := vnode.file;

    (* compute how many bytes to get on the first request *)
    bytes := MIN(want, rSize);

    (* start writing from the beginning of "data" *)
    pos   := 0;

    (* loop until we get all of the data *)
    WHILE want > 0 DO
      (* setup NFS read RPC args. *)
      args.offset := offset;
      args.count  := bytes;
      (* args.totalcount := 0; UNUSED BY NFS PROTOCOL *)
      
      TRY 
        (* Read from file. *)
        IF debug THEN
          IO.Put("Reading ");IO.PutInt(bytes);
          IO.Put(" at offset ");IO.PutInt(offset);IO.Put("\n");
        END;
        WITH nfsdata = SUBARRAY(data,pos,bytes) DO
          vnode.mp.rpc.NFSPROC_READ(args,res,nfsdata,rsize);
        END;
      EXCEPT
      | RPC.Failed (e) =>
        IO.PutError("RPC failure: ");
        IO.Put(e.info);
        IO.Put("\n");
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_INVALID_PARAMETER));
      END;

      (* check RPC return status *)
      IF res.status # 0 THEN 
        IO.PutError(Errno.Fmt(res.status));
        IO.Put(", errno=");
        IO.PutInt(res.status);
        IO.Put("\n");
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_INVALID_PARAMETER));
      END;

      (* compute new position into buffer *)
      INC(pos, rsize);

      (* increase offset into file *)
      INC(offset,rsize);

      (* decrease want by the number of bytes received from
         server. *)
      DEC(want, rsize);

      (* setup bytes for next request. *)
      bytes := MIN(want,rSize); 
    END;
  END NfsReadFile; 

PROCEDURE NfsReadLink(READONLY vnode: VnodeT): TEXT
  RAISES {Error.E} = 
  TYPE
    Res   = NfsProt.readlinkres;
    ResOK = NfsProt.readlinkres_NFS_OK;
    Args  = NfsProt.nfs_fh;
  VAR
    res  : Res;
    args : Args;
  BEGIN
    (* Set up the read from file arguments. *)
    args := vnode.file;
    TRY 
      (* Read from file. *)
      res := vnode.mp.rpc.NFSPROC_READLINK(args);
    EXCEPT
    | RPC.Failed (e) =>
      IO.PutError("RPC failure: ");
      IO.Put(e.info);
      IO.Put("\n");
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_INVALID_PARAMETER));
    END;
    IF res.status # 0 THEN 
      IO.PutError(Errno.Fmt(res.status));
      IO.Put(", errno=");
      IO.PutInt(res.status);
      IO.Put("\n");
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_INVALID_PARAMETER));
    END;
    WITH resOK = NARROW(res,ResOK) DO
      RETURN resOK.data;
    END;
  END NfsReadLink;

(* ------------------- FileSystem Registration ------------------- *)


BEGIN
  EVAL NfsInterface.Export();

  TRY
    FileSystem.Register("nfs", NEW(FileSystemT));
    localhostname := Glob.Lookup(Shell.Vars(), "hostnamefull");
    asynchAlias := Dispatcher.GetAsynchAlias(AsynchWriteThread);
  EXCEPT
  | Dispatcher.Error(e) =>
    IO.PutError("Nfs.Init: disp error.\n");
  | Error.E(e) =>
    IO.PutError(e.message()&" during Nfs initialization.\n");
  | Glob.Error =>
    IO.PutError("Nfs.Init: hostnamefull lookup failed.\n");
  END;
END Nfs.

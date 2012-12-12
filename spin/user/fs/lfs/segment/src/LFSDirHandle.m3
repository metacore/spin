INTERFACE LFSDirHandle;

IMPORT File;


TYPE
  DH = File.DirHandle BRANDED OBJECT
    
  OVERRIDES
    read  := ReadDir;
    close := CloseDir;
  END;


END LFSDirHandle.i3.




getDirEntries(pos : INTEGER; VAR (*OUT*)buf : ARRAY OF DirEnt.T)
: INTEGER RAISES {Error.E};
(* Read directory entries from POS. Return the number of
   entries read. *)



(* call causes a DirHandle to be instantiated and initialized from
   data on disk *)
PROCEDURE OpenDir (mp: MountPoint; path: TEXT): File.DirHandle
  RAISES {Error.E} =
  VAR
    dh : DH;
  BEGIN

    res := GetHandle(mp,mp.nfs_fh,path);
    IF res.status = NfsProt.nfsstat_NFS_OK THEN
      dh := NEW(DH);
      WITH resOK = NARROW(res,NfsProt.diropres_NFS_OK) DO
        dh.nfsRPC := mp.nfsRPC;
        dh.dir    := resOK.diropres.file;
        dh.cookie := NfsProt.nfscookie{VAL(0,CHAR),VAL(0,CHAR),VAL(0,CHAR),VAL(0,CHAR)};
(*      dh.dirlist :=  what is this? *)
      END;
      RETURN dh;
    ELSE
      (* XXX: handle this error better *)
      IO.Put("Can't open directory\n");
      RETURN NIL;
    END;
  END OpenDir;

PROCEDURE ReadDir (dh: DH): TEXT =
  TYPE
    Res   = NfsProt.readdirres;
    ResOK = NfsProt.readdirres_NFS_OK;
    Args  = NfsProt.readdirargs;
  VAR
    res : Res;
    args: Args;
  BEGIN
    (* Set up the directory operation arguments. *)
    args.dir    := dh.dir;
    args.cookie := dh.cookie;
    args.count  := 1024;

    TRY 
      (* XXX reader lock on mount pointer *)
      (* Look up a file name *)
      res := dh.nfsRPC.NFSPROC_READDIR(args);
    EXCEPT
    | RPC.Failed (e) =>
      dprint("RPC failure: " & e.info & "\n");
      (* XXX reader unlock on mount pointer *)
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_INVALID_PARAMETER));
    END;

    IF res.status # NfsProt.nfsstat_NFS_OK THEN 
      dprint(Errno.ErrorMessages[res.status] &
        ", errno=" & Fmt.Int(res.status) & "\n");
      RAISE Error.E(NEW(Errno.ErrorT).init(res.status));
    END;

    WITH resOK = NARROW(res,ResOK) DO
      IF resOK.reply.eof THEN
        IF debug THEN
          IO.Put("End of directory\n");
        END;
        RETURN NIL;
      ELSE
        IF debug THEN
          IO.Put("Dir as file named "); IO.Put(resOK.reply.entries.name); IO.Put("\n");
        END;
        dh.cookie := resOK.reply.entries.cookie;
        RETURN resOK.reply.entries.name;
      END;
    END;
  END ReadDir;


PROCEDURE CloseDir (<* UNUSED *> dh: DH) =
  BEGIN
    (* Clear the cache maybe if we had one *)
  END CloseDir;

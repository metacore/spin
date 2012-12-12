MODULE Nfsd_x;
IMPORT XDR, Nfsd, Mbuf;

PROCEDURE Get_nfsstat(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfsstat)
  RAISES {XDR.Failed} =
  BEGIN
    v := XDR.GetWord32(so,pos);
  END Get_nfsstat;

PROCEDURE Put_nfsstat(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfsstat): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN XDR.PutWord32(si,pos,v);
  END Put_nfsstat;

PROCEDURE Get_ftype(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.ftype)
  RAISES {XDR.Failed} =
  BEGIN
    v := XDR.GetWord32(so,pos);
  END Get_ftype;

PROCEDURE Put_ftype(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.ftype): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN XDR.PutWord32(si,pos,v);
  END Put_ftype;

PROCEDURE Get_nfs_fh(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfs_fh)
  RAISES {XDR.Failed} =
  BEGIN
    XDR.GetBytes(so,pos,v);
  END Get_nfs_fh;

PROCEDURE Put_nfs_fh(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfs_fh): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN XDR.PutBytes(si,pos,v);
  END Put_nfs_fh;

PROCEDURE Get_nfstime(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfstime)
  RAISES {XDR.Failed} =
  BEGIN
    v.seconds := XDR.GetWord32(so,pos);
    v.useconds := XDR.GetWord32(so,pos);
  END Get_nfstime;

PROCEDURE Put_nfstime(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfstime): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN XDR.PutWord32(si,pos,v.seconds) + 
           XDR.PutWord32(si,pos,v.useconds);
  END Put_nfstime;

PROCEDURE Get_fattr(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.fattr)
  RAISES {XDR.Failed} =
  BEGIN
    Get_ftype(so,pos,v.type);
    v.mode := XDR.GetWord32(so,pos);
    v.nlink := XDR.GetWord32(so,pos);
    v.uid := XDR.GetWord32(so,pos);
    v.gid := XDR.GetWord32(so,pos);
    v.size := XDR.GetWord32(so,pos);
    v.blocksize := XDR.GetWord32(so,pos);
    v.rdev := XDR.GetWord32(so,pos);
    v.blocks := XDR.GetWord32(so,pos);
    v.fsid := XDR.GetWord32(so,pos);
    v.fileid := XDR.GetWord32(so,pos);
    Get_nfstime(so,pos,v.atime);
    Get_nfstime(so,pos,v.mtime);
    Get_nfstime(so,pos,v.ctime);
  END Get_fattr;

PROCEDURE Put_fattr(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.fattr): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN Put_ftype(si,pos,v.type) + 
           XDR.PutWord32(si,pos,v.mode) +
           XDR.PutWord32(si,pos,v.nlink) + 
           XDR.PutWord32(si,pos,v.uid) +
           XDR.PutWord32(si,pos,v.gid) +
           XDR.PutWord32(si,pos,v.size) +
           XDR.PutWord32(si,pos,v.blocksize) +
           XDR.PutWord32(si,pos,v.rdev) +
           XDR.PutWord32(si,pos,v.blocks) +
           XDR.PutWord32(si,pos,v.fsid) +
           XDR.PutWord32(si,pos,v.fileid) +
           Put_nfstime(si,pos,v.atime) +
           Put_nfstime(si,pos,v.mtime) + 
           Put_nfstime(si,pos,v.ctime);
  END Put_fattr;

PROCEDURE Get_sattr(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.sattr)
  RAISES {XDR.Failed} =
  BEGIN
    v.mode := XDR.GetWord32(so,pos);
    v.uid := XDR.GetWord32(so,pos);
    v.gid := XDR.GetWord32(so,pos);
    v.size := XDR.GetWord32(so,pos);
    Get_nfstime(so,pos,v.atime);
    Get_nfstime(so,pos,v.mtime);
  END Get_sattr;

PROCEDURE Put_sattr(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.sattr): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN XDR.PutWord32(si,pos,v.mode) +
           XDR.PutWord32(si,pos,v.uid) +
           XDR.PutWord32(si,pos,v.gid) +
           XDR.PutWord32(si,pos,v.size) +
           Put_nfstime(si,pos,v.atime) +
           Put_nfstime(si,pos,v.mtime);
  END Put_sattr;

PROCEDURE Get_filename(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.filename)
  RAISES {XDR.Failed} =
  BEGIN
    XDR.GetTextAsChars(so,pos,v.len,v.name);
  END Get_filename;

PROCEDURE Put_filename(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.filename): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN XDR.PutCharsAsText(si,pos,SUBARRAY(v.name,0,v.len));
  END Put_filename;

PROCEDURE Get_nfspath(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfspath)
  RAISES {XDR.Failed} =
  BEGIN
    XDR.GetTextAsChars(so,pos,v.len,v.name);
  END Get_nfspath;

PROCEDURE Put_nfspath(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfspath): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN XDR.PutCharsAsText(si,pos,SUBARRAY(v.name,0,v.len));
  END Put_nfspath;

PROCEDURE Put_attrstat(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.attrstat): CARDINAL
  RAISES {XDR.Failed} =
  VAR
    size : CARDINAL;
  BEGIN
    size := Put_nfsstat(si,pos,v.status);
    IF v.status = Nfsd.nfsstat_NFS_OK THEN
      size := size + Put_fattr(si,pos,v.attributes);
    END;
    RETURN size;
  END Put_attrstat;

PROCEDURE Get_sattrargs(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.sattrargs)
  RAISES {XDR.Failed} =
  BEGIN
    Get_nfs_fh(so,pos,v.file);
    Get_sattr(so,pos,v.attributes);
  END Get_sattrargs;

PROCEDURE Get_diropargs(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.diropargs)
  RAISES {XDR.Failed} =
  BEGIN
    Get_nfs_fh(so,pos,v.dir);
    Get_filename(so,pos,v.name);
  END Get_diropargs;

PROCEDURE Put_diropargs(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.diropargs): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN Put_nfs_fh(si,pos,v.dir) + 
           Put_filename(si,pos,v.name);
  END Put_diropargs;

PROCEDURE Get_diropokres(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.diropokres)
  RAISES {XDR.Failed} =
  BEGIN
    Get_nfs_fh(so,pos,v.file);
    Get_fattr(so,pos,v.attributes);
  END Get_diropokres;

PROCEDURE Put_diropokres(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.diropokres): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN Put_nfs_fh(si,pos,v.file) + 
           Put_fattr(si,pos,v.attributes);
  END Put_diropokres;

PROCEDURE Put_diropres(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.diropres): CARDINAL
  RAISES {XDR.Failed} =
  VAR 
    size : CARDINAL;
  BEGIN
    size := Put_nfsstat(si,pos,v.status);
    IF v.status = Nfsd.nfsstat_NFS_OK THEN 
      size := size + Put_diropokres(si,pos,v.ok);
    END;
    RETURN size;
  END Put_diropres;

PROCEDURE Put_readres(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.readres): CARDINAL
  RAISES {XDR.Failed} =
  VAR
    size : CARDINAL;
  BEGIN
    size := Put_nfsstat(si,pos,v.status);
    IF v.status = Nfsd.nfsstat_NFS_OK THEN 
      size := size + Put_fattr(si,pos,v.attributes);
    END;
    RETURN size;
  END Put_readres;

PROCEDURE Put_readlinkres(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.readlinkres): CARDINAL
  RAISES {XDR.Failed} =
  VAR
    size : CARDINAL;
  BEGIN
    size := Put_nfsstat(si,pos,v.status);
    IF v.status = Nfsd.nfsstat_NFS_OK THEN 
      size := size + Put_nfspath(si,pos,v.data);
    END;
    RETURN size;
  END Put_readlinkres;

PROCEDURE Get_readargs(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.readargs)
  RAISES {XDR.Failed} =
  BEGIN
    Get_nfs_fh(so,pos,v.file);
    v.offset := XDR.GetWord32(so,pos);
    v.count := XDR.GetWord32(so,pos);
    v.totalcount := XDR.GetWord32(so,pos);
  END Get_readargs;

PROCEDURE Get_writeargs(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.writeargs)
  RAISES {XDR.Failed} =
  BEGIN
    Get_nfs_fh(so,pos,v.file);
    v.beginoffset := XDR.GetWord32(so,pos);
    v.offset := XDR.GetWord32(so,pos);
    v.totalcount := XDR.GetWord32(so,pos);
    WITH len = XDR.GetWord32(so,pos) DO
      IF len < 0 OR len > Nfsd.MAXDATA THEN
        RAISE XDR.Failed(XDR.Failure.SourceBadLen);
      END;
      v.data := NEW(REF ARRAY OF CHAR, len);
    END;
    XDR.GetBytes(so,pos,v.data^);
  END Get_writeargs;

PROCEDURE Get_createargs(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.createargs)
  RAISES {XDR.Failed} =
  BEGIN
    Get_diropargs(so,pos,v.where);
    Get_sattr(so,pos,v.attributes);
  END Get_createargs;

PROCEDURE Get_renameargs(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.renameargs)
  RAISES {XDR.Failed} =
  BEGIN
    Get_diropargs(so,pos,v.from);
    Get_diropargs(so,pos,v.to);
  END Get_renameargs;

PROCEDURE Put_renameargs(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.renameargs): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN Put_diropargs(si,pos,v.from) + 
           Put_diropargs(si,pos,v.to);
  END Put_renameargs;

PROCEDURE Get_linkargs(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.linkargs)
  RAISES {XDR.Failed} =
  BEGIN
    Get_nfs_fh(so,pos,v.from);
    Get_diropargs(so,pos,v.to);
  END Get_linkargs;

PROCEDURE Put_linkargs(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.linkargs): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN Put_nfs_fh(si,pos,v.from) + 
           Put_diropargs(si,pos,v.to);
  END Put_linkargs;

PROCEDURE Get_symlinkargs(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.symlinkargs)
  RAISES {XDR.Failed} =
  BEGIN
    Get_diropargs(so,pos,v.from);
    Get_nfspath(so,pos,v.to);
    Get_sattr(so,pos,v.attributes);
  END Get_symlinkargs;

PROCEDURE Put_symlinkargs(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.symlinkargs): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN Put_diropargs(si,pos,v.from) + 
           Put_nfspath(si,pos,v.to) +
           Put_sattr(si,pos,v.attributes);
  END Put_symlinkargs;

PROCEDURE Get_nfscookie(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfscookie)
  RAISES {XDR.Failed} =
  BEGIN
    XDR.GetBytes(so,pos,v);
  END Get_nfscookie;

PROCEDURE Put_nfscookie(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfscookie): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN XDR.PutBytes(si,pos,v);
  END Put_nfscookie;

PROCEDURE Get_readdirargs(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.readdirargs)
  RAISES {XDR.Failed} =
  BEGIN
    Get_nfs_fh(so,pos,v.dir);
    Get_nfscookie(so,pos,v.cookie);
    v.count := XDR.GetWord32(so,pos);
  END Get_readdirargs;

PROCEDURE Put_readdirargs(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.readdirargs): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN Put_nfs_fh(si,pos,v.dir) +
           Put_nfscookie(si,pos,v.cookie) +
           XDR.PutWord32(si,pos,v.count);
  END Put_readdirargs;

PROCEDURE Get_statfsokres(VAR so: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.statfsokres)
  RAISES {XDR.Failed} =
  BEGIN
    v.tsize := XDR.GetWord32(so,pos);
    v.bsize := XDR.GetWord32(so,pos);
    v.blocks := XDR.GetWord32(so,pos);
    v.bfree := XDR.GetWord32(so,pos);
    v.bavail := XDR.GetWord32(so,pos);
  END Get_statfsokres;

PROCEDURE Put_statfsokres(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.statfsokres): CARDINAL
  RAISES {XDR.Failed} =
  BEGIN
    RETURN XDR.PutWord32(si,pos,v.tsize) +
           XDR.PutWord32(si,pos,v.bsize) +
           XDR.PutWord32(si,pos,v.blocks) +
           XDR.PutWord32(si,pos,v.bfree) +
           XDR.PutWord32(si,pos,v.bavail);
  END Put_statfsokres;

PROCEDURE Put_statfsres(si: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.statfsres): CARDINAL
  RAISES {XDR.Failed} =
  VAR
    size : CARDINAL;
  BEGIN
    size := Put_nfsstat(si,pos,v.status);
    IF v.status = Nfsd.nfsstat_NFS_OK THEN 
      size := size + Put_statfsokres(si,pos,v.ok);
    END;
    RETURN size;
  END Put_statfsres;

BEGIN
END Nfsd_x.

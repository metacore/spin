(*
 * HISTORY
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed read/write implementation.
 *
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	This file was automatically generated using m3rpcgen.
 *
 *)

MODULE NfsProt_x;

IMPORT Ctypes;  <*NOWARN*>
IMPORT XDR, Thread, NfsProt;

PROCEDURE Get_nfsstat(so: XDR.Source; VAR v: NfsProt.nfsstat)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    v := XDR.GetInteger(so);
  END Get_nfsstat;

PROCEDURE Put_nfsstat(si: XDR.Sink; READONLY v: NfsProt.nfsstat)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutInteger(si, v);
  END Put_nfsstat;

PROCEDURE Get_ftype(so: XDR.Source; VAR v: NfsProt.ftype)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    v := XDR.GetInteger(so);
  END Get_ftype;

PROCEDURE Put_ftype(si: XDR.Sink; READONLY v: NfsProt.ftype)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutInteger(si, v);
  END Put_ftype;

PROCEDURE Get_nfs_fh(so: XDR.Source; VAR v: NfsProt.nfs_fh)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.GetBytes(so, v.data);
  END Get_nfs_fh;

PROCEDURE Put_nfs_fh(si: XDR.Sink; READONLY v: NfsProt.nfs_fh)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutBytes(si, v.data);
  END Put_nfs_fh;

PROCEDURE Get_nfstime(so: XDR.Source; VAR v: NfsProt.nfstime)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    v.seconds := XDR.GetInteger(so);
    v.useconds := XDR.GetInteger(so);
  END Get_nfstime;

PROCEDURE Put_nfstime(si: XDR.Sink; READONLY v: NfsProt.nfstime)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutInteger(si, v.seconds);
    XDR.PutInteger(si, v.useconds);
  END Put_nfstime;

PROCEDURE Get_fattr(so: XDR.Source; VAR v: NfsProt.fattr)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_ftype(so, v.type);
    v.mode := XDR.GetInteger(so);
    v.nlink := XDR.GetInteger(so);
    v.uid := XDR.GetInteger(so);
    v.gid := XDR.GetInteger(so);
    v.size := XDR.GetInteger(so);
    v.blocksize := XDR.GetInteger(so);
    v.rdev := XDR.GetInteger(so);
    v.blocks := XDR.GetInteger(so);
    v.fsid := XDR.GetInteger(so);
    v.fileid := XDR.GetInteger(so);
    Get_nfstime(so, v.atime);
    Get_nfstime(so, v.mtime);
    Get_nfstime(so, v.ctime);
  END Get_fattr;

PROCEDURE Put_fattr(si: XDR.Sink; READONLY v: NfsProt.fattr)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_ftype(si, v.type);
    XDR.PutInteger(si, v.mode);
    XDR.PutInteger(si, v.nlink);
    XDR.PutInteger(si, v.uid);
    XDR.PutInteger(si, v.gid);
    XDR.PutInteger(si, v.size);
    XDR.PutInteger(si, v.blocksize);
    XDR.PutInteger(si, v.rdev);
    XDR.PutInteger(si, v.blocks);
    XDR.PutInteger(si, v.fsid);
    XDR.PutInteger(si, v.fileid);
    Put_nfstime(si, v.atime);
    Put_nfstime(si, v.mtime);
    Put_nfstime(si, v.ctime);
  END Put_fattr;

PROCEDURE Get_sattr(so: XDR.Source; VAR v: NfsProt.sattr)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    v.mode := XDR.GetInteger(so);
    v.uid := XDR.GetInteger(so);
    v.gid := XDR.GetInteger(so);
    v.size := XDR.GetInteger(so);
    Get_nfstime(so, v.atime);
    Get_nfstime(so, v.mtime);
  END Get_sattr;

PROCEDURE Put_sattr(si: XDR.Sink; READONLY v: NfsProt.sattr)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutInteger(si, v.mode);
    XDR.PutInteger(si, v.uid);
    XDR.PutInteger(si, v.gid);
    XDR.PutInteger(si, v.size);
    Put_nfstime(si, v.atime);
    Put_nfstime(si, v.mtime);
  END Put_sattr;

PROCEDURE Get_filename(so: XDR.Source; VAR v: NfsProt.filename)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    v := XDR.GetText(so);
  END Get_filename;

PROCEDURE Put_filename(si: XDR.Sink; READONLY v: NfsProt.filename)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutText(si, v);
  END Put_filename;

PROCEDURE Get_nfspath(so: XDR.Source; VAR v: NfsProt.nfspath)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    v := XDR.GetText(so);
  END Get_nfspath;

PROCEDURE Put_nfspath(si: XDR.Sink; READONLY v: NfsProt.nfspath)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutText(si, v);
  END Put_nfspath;

PROCEDURE Get_attrstat(so: XDR.Source; VAR v: NfsProt.attrstat)
		RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    disc: NfsProt.nfsstat;
  BEGIN
    v := NEW(NfsProt.attrstat);
    Get_nfsstat(so, disc);
    CASE disc OF
        NfsProt.nfsstat_NFS_OK =>
          VAR w := NEW(NfsProt.attrstat_NFS_OK);
          BEGIN
            Get_fattr(so, w.attributes);
          v := w;
          END;
      ELSE
        VAR w := NEW(NfsProt.attrstat_Default);
        BEGIN
        v := w;
        END;
    END;
    v.status := disc;
  END Get_attrstat;

PROCEDURE Put_attrstat(si: XDR.Sink; READONLY v: NfsProt.attrstat)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfsstat(si, v.status);
    TYPECASE v OF
      | NfsProt.attrstat_NFS_OK (v) =>
          Put_fattr(si, v.attributes);
      | NfsProt.attrstat_Default =>
    ELSE
      RAISE XDR.Failed(NEW(XDR.Failure,
          info := "Bad object subtype encountered."));
    END;
  END Put_attrstat;

PROCEDURE Get_sattrargs(so: XDR.Source; VAR v: NfsProt.sattrargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_nfs_fh(so, v.file);
    Get_sattr(so, v.attributes);
  END Get_sattrargs;

PROCEDURE Put_sattrargs(si: XDR.Sink; READONLY v: NfsProt.sattrargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfs_fh(si, v.file);
    Put_sattr(si, v.attributes);
  END Put_sattrargs;

PROCEDURE Get_diropargs(so: XDR.Source; VAR v: NfsProt.diropargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_nfs_fh(so, v.dir);
    Get_filename(so, v.name);
  END Get_diropargs;

PROCEDURE Put_diropargs(si: XDR.Sink; READONLY v: NfsProt.diropargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfs_fh(si, v.dir);
    Put_filename(si, v.name);
  END Put_diropargs;

PROCEDURE Get_diropokres(so: XDR.Source; VAR v: NfsProt.diropokres)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_nfs_fh(so, v.file);
    Get_fattr(so, v.attributes);
  END Get_diropokres;

PROCEDURE Put_diropokres(si: XDR.Sink; READONLY v: NfsProt.diropokres)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfs_fh(si, v.file);
    Put_fattr(si, v.attributes);
  END Put_diropokres;

PROCEDURE Get_diropres(so: XDR.Source; VAR v: NfsProt.diropres)
		RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    disc: NfsProt.nfsstat;
  BEGIN
    v := NEW(NfsProt.diropres);
    Get_nfsstat(so, disc);
    CASE disc OF
        NfsProt.nfsstat_NFS_OK =>
          VAR w := NEW(NfsProt.diropres_NFS_OK);
          BEGIN
            Get_diropokres(so, w.diropres);
          v := w;
          END;
      ELSE
        VAR w := NEW(NfsProt.diropres_Default);
        BEGIN
        v := w;
        END;
    END;
    v.status := disc;
  END Get_diropres;

PROCEDURE Put_diropres(si: XDR.Sink; READONLY v: NfsProt.diropres)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfsstat(si, v.status);
    TYPECASE v OF
      | NfsProt.diropres_NFS_OK (v) =>
          Put_diropokres(si, v.diropres);
      | NfsProt.diropres_Default =>
    ELSE
      RAISE XDR.Failed(NEW(XDR.Failure,
          info := "Bad object subtype encountered."));
    END;
  END Put_diropres;

PROCEDURE Get_readlinkres(so: XDR.Source; VAR v: NfsProt.readlinkres)
		RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    disc: NfsProt.nfsstat;
  BEGIN
    v := NEW(NfsProt.readlinkres);
    Get_nfsstat(so, disc);
    CASE disc OF
        NfsProt.nfsstat_NFS_OK =>
          VAR w := NEW(NfsProt.readlinkres_NFS_OK);
          BEGIN
            Get_nfspath(so, w.data);
          v := w;
          END;
      ELSE
        VAR w := NEW(NfsProt.readlinkres_Default);
        BEGIN
        v := w;
        END;
    END;
    v.status := disc;
  END Get_readlinkres;

PROCEDURE Put_readlinkres(si: XDR.Sink; READONLY v: NfsProt.readlinkres)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfsstat(si, v.status);
    TYPECASE v OF
      | NfsProt.readlinkres_NFS_OK (v) =>
          Put_nfspath(si, v.data);
      | NfsProt.readlinkres_Default =>
    ELSE
      RAISE XDR.Failed(NEW(XDR.Failure,
          info := "Bad object subtype encountered."));
    END;
  END Put_readlinkres;

PROCEDURE Get_readargs(so: XDR.Source; VAR v: NfsProt.readargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_nfs_fh(so, v.file);
    v.offset := XDR.GetInteger(so);
    v.count := XDR.GetInteger(so);
    v.totalcount := XDR.GetInteger(so);
  END Get_readargs;

PROCEDURE Put_readargs(si: XDR.Sink; READONLY v: NfsProt.readargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfs_fh(si, v.file);
    XDR.PutInteger(si, v.offset);
    XDR.PutInteger(si, v.count);
    XDR.PutInteger(si, v.totalcount);
  END Put_readargs;

PROCEDURE Get_readres(
    so: XDR.Source; 
    VAR v: NfsProt.readres; 
    VAR data: ARRAY OF CHAR;
    VAR len: INTEGER)
  RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_nfsstat(so, v.status);
    CASE v.status OF
    | NfsProt.nfsstat_NFS_OK =>
      BEGIN
        Get_fattr(so, v.attributes);
        len := XDR.GetInteger(so);
        IF len < 0 OR len > NUMBER(data) THEN
          RAISE XDR.Failed(NEW(XDR.Failure,info := "Array bounds error"));
        END;
        WITH nfsdata = SUBARRAY(data,0,len) DO
          XDR.GetBytes(so, nfsdata);
        END;
      END;
    ELSE
    END;
  END Get_readres;

PROCEDURE Get_writeargs(so: XDR.Source; VAR v: NfsProt.writeargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_nfs_fh(so, v.file);
    v.beginoffset := XDR.GetInteger(so);
    v.offset := XDR.GetInteger(so);
    v.totalcount := XDR.GetInteger(so);
    WITH len = XDR.GetInteger(so) DO
      IF len < 0 OR len > NfsProt.NFS_MAXDATA THEN
        RAISE XDR.Failed(NEW(XDR.Failure,
                             info := "Array bounds error"));
      END;
      v.data := NEW(REF ARRAY OF CHAR, len);
    END;
    XDR.GetBytes(so, v.data^);
  END Get_writeargs;

PROCEDURE Put_writeargs(si: XDR.Sink;
    READONLY v: NfsProt.writeargs;
    READONLY data: ARRAY OF CHAR;
    <*UNUSED*> READONLY len: INTEGER)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfs_fh(si, v.file);
    XDR.PutInteger(si, v.beginoffset);
    XDR.PutInteger(si, v.offset);
    XDR.PutInteger(si, v.totalcount);
    IF NUMBER(data) > NfsProt.NFS_MAXDATA THEN
      RAISE XDR.Failed(NEW(XDR.Failure, info := "Array bounds error"));
    ELSE
      XDR.PutInteger(si, NUMBER(data));
      XDR.PutBytes(si, data);
    END;
  END Put_writeargs;

PROCEDURE Get_createargs(so: XDR.Source; VAR v: NfsProt.createargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_diropargs(so, v.where);
    Get_sattr(so, v.attributes);
  END Get_createargs;

PROCEDURE Put_createargs(si: XDR.Sink; READONLY v: NfsProt.createargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_diropargs(si, v.where);
    Put_sattr(si, v.attributes);
  END Put_createargs;

PROCEDURE Get_renameargs(so: XDR.Source; VAR v: NfsProt.renameargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_diropargs(so, v.from);
    Get_diropargs(so, v.to);
  END Get_renameargs;

PROCEDURE Put_renameargs(si: XDR.Sink; READONLY v: NfsProt.renameargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_diropargs(si, v.from);
    Put_diropargs(si, v.to);
  END Put_renameargs;

PROCEDURE Get_linkargs(so: XDR.Source; VAR v: NfsProt.linkargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_nfs_fh(so, v.from);
    Get_diropargs(so, v.to);
  END Get_linkargs;

PROCEDURE Put_linkargs(si: XDR.Sink; READONLY v: NfsProt.linkargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfs_fh(si, v.from);
    Put_diropargs(si, v.to);
  END Put_linkargs;

PROCEDURE Get_symlinkargs(so: XDR.Source; VAR v: NfsProt.symlinkargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_diropargs(so, v.from);
    Get_nfspath(so, v.to);
    Get_sattr(so, v.attributes);
  END Get_symlinkargs;

PROCEDURE Put_symlinkargs(si: XDR.Sink; READONLY v: NfsProt.symlinkargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_diropargs(si, v.from);
    Put_nfspath(si, v.to);
    Put_sattr(si, v.attributes);
  END Put_symlinkargs;

PROCEDURE Get_nfscookie(so: XDR.Source; VAR v: NfsProt.nfscookie)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.GetBytes(so, v);
  END Get_nfscookie;

PROCEDURE Put_nfscookie(si: XDR.Sink; READONLY v: NfsProt.nfscookie)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutBytes(si, v);
  END Put_nfscookie;

PROCEDURE Get_readdirargs(so: XDR.Source; VAR v: NfsProt.readdirargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Get_nfs_fh(so, v.dir);
    Get_nfscookie(so, v.cookie);
    v.count := XDR.GetInteger(so);
  END Get_readdirargs;

PROCEDURE Put_readdirargs(si: XDR.Sink; READONLY v: NfsProt.readdirargs)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfs_fh(si, v.dir);
    Put_nfscookie(si, v.cookie);
    XDR.PutInteger(si, v.count);
  END Put_readdirargs;

PROCEDURE Get_entry(so: XDR.Source; VAR v: NfsProt.entry)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    v.fileid := XDR.GetInteger(so);
    Get_filename(so, v.name);
    Get_nfscookie(so, v.cookie);
    IF NOT XDR.GetBoolean(so) THEN
      v.nextentry := NIL;
    ELSE
      v.nextentry := NEW(REF NfsProt.entry);
      Get_entry(so, v.nextentry^);
    END;
  END Get_entry;

PROCEDURE Put_entry(si: XDR.Sink; READONLY v: NfsProt.entry)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutInteger(si, v.fileid);
    Put_filename(si, v.name);
    Put_nfscookie(si, v.cookie);
    IF v.nextentry = NIL THEN
      XDR.PutBoolean(si, FALSE);
    ELSE
      XDR.PutBoolean(si, TRUE);
      Put_entry(si, v.nextentry^);
    END;
  END Put_entry;

PROCEDURE Get_dirlist(so: XDR.Source; VAR v: NfsProt.dirlist)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    IF NOT XDR.GetBoolean(so) THEN
      v.entries := NIL;
    ELSE
      v.entries := NEW(REF NfsProt.entry);
      Get_entry(so, v.entries^);
    END;
    v.eof := XDR.GetBoolean(so);
  END Get_dirlist;

PROCEDURE Put_dirlist(si: XDR.Sink; READONLY v: NfsProt.dirlist)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    IF v.entries = NIL THEN
      XDR.PutBoolean(si, FALSE);
    ELSE
      XDR.PutBoolean(si, TRUE);
      Put_entry(si, v.entries^);
    END;
    XDR.PutBoolean(si, v.eof);
  END Put_dirlist;

PROCEDURE Get_readdirres(so: XDR.Source; VAR v: NfsProt.readdirres)
		RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    disc: NfsProt.nfsstat;
  BEGIN
    v := NEW(NfsProt.readdirres);
    Get_nfsstat(so, disc);
    CASE disc OF
        NfsProt.nfsstat_NFS_OK =>
          VAR w := NEW(NfsProt.readdirres_NFS_OK);
          BEGIN
            Get_dirlist(so, w.reply);
          v := w;
          END;
      ELSE
        VAR w := NEW(NfsProt.readdirres_Default);
        BEGIN
        v := w;
        END;
    END;
    v.status := disc;
  END Get_readdirres;

PROCEDURE Put_readdirres(si: XDR.Sink; READONLY v: NfsProt.readdirres)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfsstat(si, v.status);
    TYPECASE v OF
      | NfsProt.readdirres_NFS_OK (v) =>
          Put_dirlist(si, v.reply);
      | NfsProt.readdirres_Default =>
    ELSE
      RAISE XDR.Failed(NEW(XDR.Failure,
          info := "Bad object subtype encountered."));
    END;
  END Put_readdirres;

PROCEDURE Get_statfsokres(so: XDR.Source; VAR v: NfsProt.statfsokres)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    v.tsize := XDR.GetInteger(so);
    v.bsize := XDR.GetInteger(so);
    v.blocks := XDR.GetInteger(so);
    v.bfree := XDR.GetInteger(so);
    v.bavail := XDR.GetInteger(so);
  END Get_statfsokres;

PROCEDURE Put_statfsokres(si: XDR.Sink; READONLY v: NfsProt.statfsokres)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutInteger(si, v.tsize);
    XDR.PutInteger(si, v.bsize);
    XDR.PutInteger(si, v.blocks);
    XDR.PutInteger(si, v.bfree);
    XDR.PutInteger(si, v.bavail);
  END Put_statfsokres;

PROCEDURE Get_statfsres(so: XDR.Source; VAR v: NfsProt.statfsres)
		RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    disc: NfsProt.nfsstat;
  BEGIN
    v := NEW(NfsProt.statfsres);
    Get_nfsstat(so, disc);
    CASE disc OF
        NfsProt.nfsstat_NFS_OK =>
          VAR w := NEW(NfsProt.statfsres_NFS_OK);
          BEGIN
            Get_statfsokres(so, w.reply);
          v := w;
          END;
      ELSE
        VAR w := NEW(NfsProt.statfsres_Default);
        BEGIN
        v := w;
        END;
    END;
    v.status := disc;
  END Get_statfsres;

PROCEDURE Put_statfsres(si: XDR.Sink; READONLY v: NfsProt.statfsres)
		RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    Put_nfsstat(si, v.status);
    TYPECASE v OF
      | NfsProt.statfsres_NFS_OK (v) =>
          Put_statfsokres(si, v.reply);
      | NfsProt.statfsres_Default =>
    ELSE
      RAISE XDR.Failed(NEW(XDR.Failure,
          info := "Bad object subtype encountered."));
    END;
  END Put_statfsres;


BEGIN
END NfsProt_x.

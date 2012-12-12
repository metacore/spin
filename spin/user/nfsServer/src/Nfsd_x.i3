INTERFACE Nfsd_x;
IMPORT Mbuf, Nfsd, XDR;

PROCEDURE Get_nfsstat(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfsstat)
  RAISES{XDR.Failed};
PROCEDURE Put_nfsstat(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfsstat): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_ftype(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.ftype)
  RAISES{XDR.Failed};
PROCEDURE Put_ftype(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.ftype): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_nfs_fh(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfs_fh)
  RAISES{XDR.Failed};
PROCEDURE Put_nfs_fh(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfs_fh): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_nfstime(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfstime)
  RAISES{XDR.Failed};
PROCEDURE Put_nfstime(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfstime): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_fattr(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.fattr)
  RAISES{XDR.Failed};
PROCEDURE Put_fattr(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.fattr): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_sattr(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.sattr)
  RAISES{XDR.Failed};
PROCEDURE Put_sattr(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.sattr): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_filename(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.filename)
  RAISES{XDR.Failed};
PROCEDURE Put_filename(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.filename): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_nfspath(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfspath)
  RAISES{XDR.Failed};
PROCEDURE Put_nfspath(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfspath): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Put_attrstat(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.attrstat): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_sattrargs(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.sattrargs)
  RAISES{XDR.Failed};
PROCEDURE Get_diropargs(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.diropargs)
  RAISES{XDR.Failed};
PROCEDURE Put_diropargs(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.diropargs): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_diropokres(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.diropokres)
  RAISES{XDR.Failed};
PROCEDURE Put_diropokres(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.diropokres): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Put_diropres(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.diropres): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Put_readres(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.readres): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Put_readlinkres(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.readlinkres): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_readargs(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.readargs)
  RAISES{XDR.Failed};
PROCEDURE Get_writeargs(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.writeargs)
  RAISES{XDR.Failed};
PROCEDURE Get_createargs(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.createargs)
  RAISES{XDR.Failed};
PROCEDURE Get_renameargs(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.renameargs)
  RAISES{XDR.Failed};
PROCEDURE Put_renameargs(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.renameargs): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_linkargs(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.linkargs)
  RAISES{XDR.Failed};
PROCEDURE Put_linkargs(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.linkargs): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_symlinkargs(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.symlinkargs)
  RAISES{XDR.Failed};
PROCEDURE Put_symlinkargs(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.symlinkargs): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_nfscookie(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.nfscookie)
  RAISES{XDR.Failed};
PROCEDURE Put_nfscookie(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.nfscookie): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_readdirargs(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.readdirargs)
  RAISES{XDR.Failed};
PROCEDURE Put_readdirargs(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.readdirargs): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Get_statfsokres(VAR source: Mbuf.T; VAR pos: CARDINAL; VAR v: Nfsd.statfsokres)
  RAISES{XDR.Failed};
PROCEDURE Put_statfsokres(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.statfsokres): CARDINAL
  RAISES{XDR.Failed};
PROCEDURE Put_statfsres(sink: Mbuf.T; VAR pos: CARDINAL; READONLY v: Nfsd.statfsres): CARDINAL
  RAISES{XDR.Failed};
END Nfsd_x.

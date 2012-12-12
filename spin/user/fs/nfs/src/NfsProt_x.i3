(*
 * This file is based on code automatically generated using m3rpcgen.
 *
 * HISTORY
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Removed unused interface and changed read/write interface.
 *
 * 09-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE NfsProt_x;

IMPORT XDR, Thread, NfsProt;

PROCEDURE Get_nfsstat(so: XDR.Source; VAR v: NfsProt.nfsstat)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_nfsstat(si: XDR.Sink; READONLY v: NfsProt.nfsstat)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_ftype(so: XDR.Source; VAR v: NfsProt.ftype)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_ftype(si: XDR.Sink; READONLY v: NfsProt.ftype)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_nfs_fh(so: XDR.Source; VAR v: NfsProt.nfs_fh)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_nfs_fh(si: XDR.Sink; READONLY v: NfsProt.nfs_fh)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_nfstime(so: XDR.Source; VAR v: NfsProt.nfstime)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_nfstime(si: XDR.Sink; READONLY v: NfsProt.nfstime)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_fattr(so: XDR.Source; VAR v: NfsProt.fattr)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_fattr(si: XDR.Sink; READONLY v: NfsProt.fattr)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_sattr(so: XDR.Source; VAR v: NfsProt.sattr)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_sattr(si: XDR.Sink; READONLY v: NfsProt.sattr)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_filename(so: XDR.Source; VAR v: NfsProt.filename)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_filename(si: XDR.Sink; READONLY v: NfsProt.filename)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_nfspath(so: XDR.Source; VAR v: NfsProt.nfspath)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_nfspath(si: XDR.Sink; READONLY v: NfsProt.nfspath)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_attrstat(so: XDR.Source; VAR v: NfsProt.attrstat)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_attrstat(si: XDR.Sink; READONLY v: NfsProt.attrstat)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_sattrargs(so: XDR.Source; VAR v: NfsProt.sattrargs)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_sattrargs(si: XDR.Sink; READONLY v: NfsProt.sattrargs)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_diropargs(so: XDR.Source; VAR v: NfsProt.diropargs)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_diropargs(si: XDR.Sink; READONLY v: NfsProt.diropargs)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_diropokres(so: XDR.Source; VAR v: NfsProt.diropokres)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_diropokres(si: XDR.Sink; READONLY v: NfsProt.diropokres)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_diropres(so: XDR.Source; VAR v: NfsProt.diropres)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_diropres(si: XDR.Sink; READONLY v: NfsProt.diropres)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_readlinkres(so: XDR.Source; VAR v: NfsProt.readlinkres)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_readlinkres(si: XDR.Sink; READONLY v: NfsProt.readlinkres)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_readargs(so: XDR.Source; VAR v: NfsProt.readargs)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_readargs(si: XDR.Sink; READONLY v: NfsProt.readargs)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_readres(so: XDR.Source; VAR v: NfsProt.readres; VAR data: ARRAY OF CHAR; VAR len: INTEGER)
		RAISES {XDR.Failed, Thread.Alerted};
(*
PROCEDURE Put_readres(si: XDR.Sink; READONLY v: NfsProt.readres)
		RAISES {XDR.Failed, Thread.Alerted};
*)

PROCEDURE Get_writeargs(so: XDR.Source; VAR v: NfsProt.writeargs)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_writeargs(si: XDR.Sink; READONLY v: NfsProt.writeargs; READONLY data: ARRAY OF CHAR; READONLY len: INTEGER)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_createargs(so: XDR.Source; VAR v: NfsProt.createargs)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_createargs(si: XDR.Sink; READONLY v: NfsProt.createargs)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_renameargs(so: XDR.Source; VAR v: NfsProt.renameargs)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_renameargs(si: XDR.Sink; READONLY v: NfsProt.renameargs)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_linkargs(so: XDR.Source; VAR v: NfsProt.linkargs)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_linkargs(si: XDR.Sink; READONLY v: NfsProt.linkargs)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_symlinkargs(so: XDR.Source; VAR v: NfsProt.symlinkargs)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_symlinkargs(si: XDR.Sink; READONLY v: NfsProt.symlinkargs)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_nfscookie(so: XDR.Source; VAR v: NfsProt.nfscookie)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_nfscookie(si: XDR.Sink; READONLY v: NfsProt.nfscookie)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_readdirargs(so: XDR.Source; VAR v: NfsProt.readdirargs)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_readdirargs(si: XDR.Sink; READONLY v: NfsProt.readdirargs)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_entry(so: XDR.Source; VAR v: NfsProt.entry)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_entry(si: XDR.Sink; READONLY v: NfsProt.entry)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_dirlist(so: XDR.Source; VAR v: NfsProt.dirlist)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_dirlist(si: XDR.Sink; READONLY v: NfsProt.dirlist)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_readdirres(so: XDR.Source; VAR v: NfsProt.readdirres)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_readdirres(si: XDR.Sink; READONLY v: NfsProt.readdirres)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_statfsokres(so: XDR.Source; VAR v: NfsProt.statfsokres)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_statfsokres(si: XDR.Sink; READONLY v: NfsProt.statfsokres)
		RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE Get_statfsres(so: XDR.Source; VAR v: NfsProt.statfsres)
		RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE Put_statfsres(si: XDR.Sink; READONLY v: NfsProt.statfsres)
		RAISES {XDR.Failed, Thread.Alerted};

END NfsProt_x.

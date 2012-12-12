(*
 * HISTORY
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed read/write interface.
 *
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	This file was automatically generated using m3rpcgen.
 *)

MODULE NfsProt_c EXPORTS NfsProt;

IMPORT RPC, RPCSun, XDR, Thread;
IMPORT Ctypes;  <*NOWARN*>
IMPORT NfsProt_x;  <*NOWARN*>


TYPE NFS_VERSIONPrivate = NFS_VERSIONClient OBJECT
OVERRIDES
  NFSPROC_NULL := NFSPROC_NULL2;
  NFSPROC_GETATTR := NFSPROC_GETATTR2;
  NFSPROC_SETATTR := NFSPROC_SETATTR2;
  NFSPROC_ROOT := NFSPROC_ROOT2;
  NFSPROC_LOOKUP := NFSPROC_LOOKUP2;
  NFSPROC_READLINK := NFSPROC_READLINK2;
  NFSPROC_READ := NFSPROC_READ2;
  NFSPROC_WRITECACHE := NFSPROC_WRITECACHE2;
  NFSPROC_WRITE := NFSPROC_WRITE2;
  NFSPROC_CREATE := NFSPROC_CREATE2;
  NFSPROC_REMOVE := NFSPROC_REMOVE2;
  NFSPROC_RENAME := NFSPROC_RENAME2;
  NFSPROC_LINK := NFSPROC_LINK2;
  NFSPROC_SYMLINK := NFSPROC_SYMLINK2;
  NFSPROC_MKDIR := NFSPROC_MKDIR2;
  NFSPROC_RMDIR := NFSPROC_RMDIR2;
  NFSPROC_READDIR := NFSPROC_READDIR2;
  NFSPROC_STATFS := NFSPROC_STATFS2;
  GetClient := NFS_VERSIONGetClient;
END;

PROCEDURE ImportNFS_VERSION(b: RPCSun.BindingInfo): NFS_VERSION
  RAISES {RPC.Failed, Thread.Alerted} =
  BEGIN
    RETURN NEW(NFS_VERSIONPrivate,
		mu := NEW(MUTEX),
		cl := RPCSun.ImportService(b));
  END ImportNFS_VERSION;

PROCEDURE NFS_VERSIONGetClient(o: NFS_VERSIONPrivate): RPCSun.Client =
  BEGIN
    RETURN o.cl;
  END NFS_VERSIONGetClient;

PROCEDURE NFSPROC_NULL2(o: NFS_VERSIONPrivate)
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR
    tr: RPCSun.Transaction;
    so: XDR.Source;
  BEGIN
    tr := o.cl.StartCall(0);
    so := o.cl.SendCall(tr);
    o.cl.EndCall(tr);
  END NFSPROC_NULL2;

PROCEDURE NFSPROC_GETATTR2(o: NFS_VERSIONPrivate; READONLY inParm: nfs_fh): attrstat
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR
    tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: attrstat;
  BEGIN
    TRY
      tr := o.cl.StartCall(1);
      NfsProt_x.Put_nfs_fh(tr.sink, inParm);
      so := o.cl.SendCall(tr);
      NfsProt_x.Get_attrstat(so, outParm);
      o.cl.EndCall(tr);
      RETURN outParm;
    EXCEPT
    | XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "getattr2: xdr failure",
		     subArg := e));
    END;
  END NFSPROC_GETATTR2;

PROCEDURE NFSPROC_SETATTR2(o: NFS_VERSIONPrivate; READONLY inParm: sattrargs): attrstat
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: attrstat;
  BEGIN
    tr := o.cl.StartCall(2);
    TRY
      NfsProt_x.Put_sattrargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_attrstat(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_SETATTR2;

PROCEDURE NFSPROC_ROOT2(o: NFS_VERSIONPrivate)
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
  BEGIN
    tr := o.cl.StartCall(3);
    so := o.cl.SendCall(tr);
    o.cl.EndCall(tr);
  END NFSPROC_ROOT2;

PROCEDURE NFSPROC_LOOKUP2(o: NFS_VERSIONPrivate; READONLY inParm: diropargs): diropres
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: diropres;
  BEGIN
    tr := o.cl.StartCall(4);
    TRY
      NfsProt_x.Put_diropargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_diropres(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_LOOKUP2;

PROCEDURE NFSPROC_READLINK2(o: NFS_VERSIONPrivate; READONLY inParm: nfs_fh): readlinkres
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: readlinkres;
  BEGIN
    tr := o.cl.StartCall(5);
    TRY
      NfsProt_x.Put_nfs_fh(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_readlinkres(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_READLINK2;

PROCEDURE NFSPROC_READ2(
    o: NFS_VERSIONPrivate; 
    READONLY inParm: readargs; 
    VAR v: readres; 
    VAR data: ARRAY OF CHAR;
    VAR len: INTEGER)
  RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR
    tr: RPCSun.Transaction;
    so: XDR.Source;
  BEGIN
    tr := o.cl.StartCall(6);
    TRY
      NfsProt_x.Put_readargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_readres(so, v, data, len);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
  END NFSPROC_READ2;

PROCEDURE NFSPROC_WRITECACHE2(o: NFS_VERSIONPrivate)
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
  BEGIN
    tr := o.cl.StartCall(7);
    so := o.cl.SendCall(tr);
    o.cl.EndCall(tr);
  END NFSPROC_WRITECACHE2;

PROCEDURE NFSPROC_WRITE2(o: NFS_VERSIONPrivate;
    READONLY inParm: writeargs;
    READONLY data: ARRAY OF CHAR;
    READONLY len: INTEGER): attrstat
	RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: attrstat;
  BEGIN
    TRY
      tr := o.cl.StartCall(8);
      NfsProt_x.Put_writeargs(tr.sink, inParm, data, len);
      so := o.cl.SendCall(tr);
      NfsProt_x.Get_attrstat(so, outParm);
      o.cl.EndCall(tr);
      RETURN outParm;
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
  END NFSPROC_WRITE2;

PROCEDURE NFSPROC_CREATE2(o: NFS_VERSIONPrivate; READONLY inParm: createargs): diropres
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: diropres;
  BEGIN
    tr := o.cl.StartCall(9);
    TRY
      NfsProt_x.Put_createargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_diropres(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_CREATE2;

PROCEDURE NFSPROC_REMOVE2(o: NFS_VERSIONPrivate; READONLY inParm: diropargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: nfsstat;
  BEGIN
    tr := o.cl.StartCall(10);
    TRY
      NfsProt_x.Put_diropargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_nfsstat(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_REMOVE2;

PROCEDURE NFSPROC_RENAME2(o: NFS_VERSIONPrivate; READONLY inParm: renameargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: nfsstat;
  BEGIN
    tr := o.cl.StartCall(11);
    TRY
      NfsProt_x.Put_renameargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_nfsstat(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_RENAME2;

PROCEDURE NFSPROC_LINK2(o: NFS_VERSIONPrivate; READONLY inParm: linkargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: nfsstat;
  BEGIN
    tr := o.cl.StartCall(12);
    TRY
      NfsProt_x.Put_linkargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_nfsstat(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_LINK2;

PROCEDURE NFSPROC_SYMLINK2(o: NFS_VERSIONPrivate; READONLY inParm: symlinkargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: nfsstat;
  BEGIN
    tr := o.cl.StartCall(13);
    TRY
      NfsProt_x.Put_symlinkargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_nfsstat(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_SYMLINK2;

PROCEDURE NFSPROC_MKDIR2(o: NFS_VERSIONPrivate; READONLY inParm: createargs): diropres
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: diropres;
  BEGIN
    tr := o.cl.StartCall(14);
    TRY
      NfsProt_x.Put_createargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_diropres(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_MKDIR2;

PROCEDURE NFSPROC_RMDIR2(o: NFS_VERSIONPrivate; READONLY inParm: diropargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: nfsstat;
  BEGIN
    tr := o.cl.StartCall(15);
    TRY
      NfsProt_x.Put_diropargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_nfsstat(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_RMDIR2;

PROCEDURE NFSPROC_READDIR2(o: NFS_VERSIONPrivate; READONLY inParm: readdirargs): readdirres
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: readdirres;
  BEGIN
    tr := o.cl.StartCall(16);
    TRY
      NfsProt_x.Put_readdirargs(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_readdirres(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_READDIR2;

PROCEDURE NFSPROC_STATFS2(o: NFS_VERSIONPrivate; READONLY inParm: nfs_fh): statfsres
		RAISES {RPC.Failed, Thread.Alerted} =
  <* FATAL RPCSun.Erred *>
  VAR tr: RPCSun.Transaction;
    so: XDR.Source;
    outParm: statfsres;
  BEGIN
    tr := o.cl.StartCall(17);
    TRY
      NfsProt_x.Put_nfs_fh(tr.sink, inParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.ZeroTimesFailure,
		     info := "arg marshalling failed",
		     subArg := e));
    END;
    so := o.cl.SendCall(tr);
    TRY
      NfsProt_x.Get_statfsres(so, outParm);
    EXCEPT
    XDR.Failed(e) =>
      RAISE
      RPC.Failed(NEW(RPC.Failure,
		     info := "result marshalling failed",
		     subArg := e));
    END;
    o.cl.EndCall(tr);
    RETURN outParm;
  END NFSPROC_STATFS2;

BEGIN
END NfsProt_c.


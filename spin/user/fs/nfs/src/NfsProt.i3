(*
 * HISTORY
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed read/write interface and removed an unimplemented
 *	procedure.
 *
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	This file was automatically generated using m3rpcgen.
 *	
 *)

INTERFACE NfsProt;

IMPORT RPC, RPCSun, Thread;
IMPORT Ctypes;  <*NOWARN*>
CONST NFS_PORT = 2049;
CONST NFS_MAXDATA = 8192;
CONST NFS_MAXPATHLEN = 1024;
CONST NFS_MAXNAMLEN = 255;
CONST NFS_FHSIZE = 32;
CONST NFS_COOKIESIZE = 4;
CONST NFS_FIFO_DEV = -1;
CONST NFSMODE_FMT = 0170000;
CONST NFSMODE_DIR = 0040000;
CONST NFSMODE_CHR = 0020000;
CONST NFSMODE_BLK = 0060000;
CONST NFSMODE_REG = 0100000;
CONST NFSMODE_LNK = 0120000;
CONST NFSMODE_SOCK = 0140000;
CONST NFSMODE_FIFO = 0010000;

TYPE nfsstat = [0..99];
CONST
	nfsstat_NFS_OK = 0;
	nfsstat_NFSERR_PERM = 1;
	nfsstat_NFSERR_NOENT = 2;
	nfsstat_NFSERR_IO = 5;
	nfsstat_NFSERR_NXIO = 6;
	nfsstat_NFSERR_ACCES = 13;
	nfsstat_NFSERR_EXIST = 17;
	nfsstat_NFSERR_NODEV = 19;
	nfsstat_NFSERR_NOTDIR = 20;
	nfsstat_NFSERR_ISDIR = 21;
	nfsstat_NFSERR_FBIG = 27;
	nfsstat_NFSERR_NOSPC = 28;
	nfsstat_NFSERR_ROFS = 30;
	nfsstat_NFSERR_NAMETOOLONG = 63;
	nfsstat_NFSERR_NOTEMPTY = 66;
	nfsstat_NFSERR_DQUOT = 69;
	nfsstat_NFSERR_STALE = 70;
	nfsstat_NFSERR_WFLUSH = 99;

	nfsstat_names = ARRAY [0..99] OF TEXT{
		"NFS_OK", "NFSERR_PERM", "NFSERR_NOENT", NIL, NIL, 
		"NFSERR_IO", "NFSERR_NXIO", NIL, NIL, NIL, NIL, NIL, NIL, 
		"NFSERR_ACCES", NIL, NIL, NIL, "NFSERR_EXIST", NIL, 
		"NFSERR_NODEV", "NFSERR_NOTDIR", "NFSERR_ISDIR", NIL, NIL, 
		NIL, NIL, NIL, "NFSERR_FBIG", "NFSERR_NOSPC", NIL, 
		"NFSERR_ROFS", NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, 
		NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, 
		NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, 
		"NFSERR_NAMETOOLONG", NIL, NIL, "NFSERR_NOTEMPTY", NIL, NIL, 
		"NFSERR_DQUOT", "NFSERR_STALE", NIL, NIL, NIL, NIL, NIL, 
		NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, 
		NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, 
		"NFSERR_WFLUSH"};

TYPE ftype = [0..8];
CONST
	ftype_NFNON = 0;
	ftype_NFREG = 1;
	ftype_NFDIR = 2;
	ftype_NFBLK = 3;
	ftype_NFCHR = 4;
	ftype_NFLNK = 5;
	ftype_NFSOCK = 6;
	ftype_NFBAD = 7;
	ftype_NFFIFO = 8;

	ftype_names = ARRAY [0..8] OF TEXT{
		"NFNON", "NFREG", "NFDIR", "NFBLK", "NFCHR", "NFLNK", 
		"NFSOCK", "NFBAD", "NFFIFO"};

TYPE nfs_fh = 
	RECORD
        data: ARRAY [0..NFS_FHSIZE - 1] OF CHAR;
	END;

TYPE nfstime = 
	RECORD
        seconds: Ctypes.unsigned_int;
        useconds: Ctypes.unsigned_int;
	END;

TYPE fattr = 
	RECORD
        type: ftype;
        mode: Ctypes.unsigned_int;
        nlink: Ctypes.unsigned_int;
        uid: Ctypes.unsigned_int;
        gid: Ctypes.unsigned_int;
        size: Ctypes.unsigned_int;
        blocksize: Ctypes.unsigned_int;
        rdev: Ctypes.unsigned_int;
        blocks: Ctypes.unsigned_int;
        fsid: Ctypes.unsigned_int;
        fileid: Ctypes.unsigned_int;
        atime: nfstime;
        mtime: nfstime;
        ctime: nfstime;
	END;

TYPE sattr = 
	RECORD
        mode: Ctypes.unsigned_int;
        uid: Ctypes.unsigned_int;
        gid: Ctypes.unsigned_int;
        size: Ctypes.unsigned_int;
        atime: nfstime;
        mtime: nfstime;
	END;

TYPE filename = TEXT;

TYPE nfspath = TEXT;

TYPE attrstat = OBJECT
	status: nfsstat;
	END;
	attrstat_NFS_OK =
	    attrstat BRANDED "NfsProt_attrstat_NFS_OK" OBJECT
                attributes: fattr;
		END;
	attrstat_Default =
	    attrstat BRANDED "NfsProt_attrstat_Default" OBJECT
		END;

TYPE sattrargs = 
	RECORD
        file: nfs_fh;
        attributes: sattr;
	END;

TYPE diropargs = 
	RECORD
        dir: nfs_fh;
        name: filename;
	END;

TYPE diropokres = 
	RECORD
        file: nfs_fh;
        attributes: fattr;
	END;

TYPE diropres = OBJECT
	status: nfsstat;
	END;
	diropres_NFS_OK =
	    diropres BRANDED "NfsProt_diropres_NFS_OK" OBJECT
                diropres: diropokres;
		END;
	diropres_Default =
	    diropres BRANDED "NfsProt_diropres_Default" OBJECT
		END;

TYPE readlinkres = OBJECT
	status: nfsstat;
	END;
	readlinkres_NFS_OK =
	    readlinkres BRANDED "NfsProt_readlinkres_NFS_OK" OBJECT
                data: nfspath;
		END;
	readlinkres_Default =
	    readlinkres BRANDED "NfsProt_readlinkres_Default" OBJECT
		END;

TYPE readargs = 
	RECORD
        file: nfs_fh;
        offset: Ctypes.unsigned_int;
        count: Ctypes.unsigned_int;
        totalcount: Ctypes.unsigned_int;
	END;

TYPE readres = 
        RECORD
	status: nfsstat;
        attributes: fattr;
        END;

TYPE writeargs = 
	RECORD
        file: nfs_fh;
        beginoffset: Ctypes.unsigned_int;
        offset: Ctypes.unsigned_int;
        totalcount: Ctypes.unsigned_int;
        data: REF ARRAY OF CHAR;
	END;

TYPE createargs = 
	RECORD
        where: diropargs;
        attributes: sattr;
	END;

TYPE renameargs = 
	RECORD
        from: diropargs;
        to: diropargs;
	END;

TYPE linkargs = 
	RECORD
        from: nfs_fh;
        to: diropargs;
	END;

TYPE symlinkargs = 
	RECORD
        from: diropargs;
        to: nfspath;
        attributes: sattr;
	END;

TYPE nfscookie = ARRAY [0..NFS_COOKIESIZE - 1] OF CHAR;

TYPE readdirargs = 
	RECORD
        dir: nfs_fh;
        cookie: nfscookie;
        count: Ctypes.unsigned_int;
	END;

TYPE entry = 
	RECORD
        fileid: Ctypes.unsigned_int;
        name: filename;
        cookie: nfscookie;
        nextentry: REF entry;
	END;

TYPE dirlist = 
	RECORD
        entries: REF entry;
        eof: BOOLEAN;
	END;

TYPE readdirres = OBJECT
	status: nfsstat;
	END;
	readdirres_NFS_OK =
	    readdirres BRANDED "NfsProt_readdirres_NFS_OK" OBJECT
                reply: dirlist;
		END;
	readdirres_Default =
	    readdirres BRANDED "NfsProt_readdirres_Default" OBJECT
		END;

TYPE statfsokres = 
	RECORD
        tsize: Ctypes.unsigned_int;
        bsize: Ctypes.unsigned_int;
        blocks: Ctypes.unsigned_int;
        bfree: Ctypes.unsigned_int;
        bavail: Ctypes.unsigned_int;
	END;

TYPE statfsres = OBJECT
	status: nfsstat;
	END;
	statfsres_NFS_OK =
	    statfsres BRANDED "NfsProt_statfsres_NFS_OK" OBJECT
                reply: statfsokres;
		END;
	statfsres_Default =
	    statfsres BRANDED "NfsProt_statfsres_Default" OBJECT
		END;

CONST NFS_PROGRAM_prognum = 100003;

CONST NFS_VERSION_versnum = 2;

TYPE NFS_VERSION = OBJECT
	METHODS
	  NFSPROC_NULL()
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_GETATTR(READONLY inParm: nfs_fh): attrstat
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_SETATTR(READONLY inParm: sattrargs): attrstat
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_ROOT()
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_LOOKUP(READONLY inParm: diropargs): diropres
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_READLINK(READONLY inParm: nfs_fh): readlinkres
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_READ(READONLY inParm: readargs; VAR v: readres; VAR data: ARRAY OF CHAR; VAR len: INTEGER)
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_WRITECACHE()
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_WRITE(READONLY inParm: writeargs; READONLY data: ARRAY OF CHAR; READONLY len: INTEGER): attrstat
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_CREATE(READONLY inParm: createargs): diropres
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_REMOVE(READONLY inParm: diropargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_RENAME(READONLY inParm: renameargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_LINK(READONLY inParm: linkargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_SYMLINK(READONLY inParm: symlinkargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_MKDIR(READONLY inParm: createargs): diropres
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_RMDIR(READONLY inParm: diropargs): nfsstat
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_READDIR(READONLY inParm: readdirargs): readdirres
		RAISES {RPC.Failed, Thread.Alerted};
	  NFSPROC_STATFS(READONLY inParm: nfs_fh): statfsres
		RAISES {RPC.Failed, Thread.Alerted};
	END;

TYPE NFS_VERSIONClient = NFS_VERSION OBJECT
  mu: MUTEX := NIL;
  cl: RPCSun.Client := NIL;
METHODS
  GetClient(): RPCSun.Client;
END;

PROCEDURE ImportNFS_VERSION(b: RPCSun.BindingInfo): NFS_VERSION
		RAISES {RPC.Failed, Thread.Alerted};

TYPE T = diropres;
CONST Brand = "NfsProt";

END NfsProt.

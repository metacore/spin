INTERFACE Nfsd;
IMPORT Ctypes;
IMPORT Mbuf, RPC, XDR;
CONST
  hostOrderPort : Ctypes.unsigned_int = 16_0801;
  netOrderPort  : Ctypes.unsigned_int = 16_0108;

CONST
  (* Port mapper constants. *)
  Prog : Ctypes.unsigned_int = 100003;
  Vers : Ctypes.unsigned_int = 2;

CONST MAXDATA    = 8192;
CONST MAXPATHLEN = 1024;
CONST MAXNAMLEN  = 255;
CONST FHSIZE     = 32;
CONST COOKIESIZE = 4;
CONST FIFO_DEV   = -1;
CONST MODE_FMT   = 0170000;
CONST MODE_DIR   = 0040000;
CONST MODE_CHR   = 0020000;
CONST MODE_BLK   = 0060000;
CONST MODE_REG   = 0100000;
CONST MODE_LNK   = 0120000;
CONST MODE_SOCK  = 0140000;
CONST MODE_FIFO  = 0010000;

TYPE nfsstat = [0..99];
CONST
        nfsstat_NFS_OK             = 0;
        nfsstat_NFSERR_PERM        = 1;
        nfsstat_NFSERR_NOENT       = 2;
        nfsstat_NFSERR_IO          = 5;
        nfsstat_NFSERR_NXIO        = 6;
        nfsstat_NFSERR_ACCES       = 13;
        nfsstat_NFSERR_EXIST       = 17;
        nfsstat_NFSERR_NODEV       = 19;
        nfsstat_NFSERR_NOTDIR      = 20;
        nfsstat_NFSERR_ISDIR       = 21;
        nfsstat_NFSERR_FBIG        = 27;
        nfsstat_NFSERR_NOSPC       = 28;
        nfsstat_NFSERR_ROFS        = 30;
        nfsstat_NFSERR_NAMETOOLONG = 63;
        nfsstat_NFSERR_NOTEMPTY    = 66;
        nfsstat_NFSERR_DQUOT       = 69;
        nfsstat_NFSERR_STALE       = 70;
        nfsstat_NFSERR_WFLUSH      = 99;

TYPE nfs_fh = ARRAY [1..FHSIZE] OF CHAR;

TYPE nfs_text = RECORD
  len  : CARDINAL;
  name : ARRAY [1..MAXNAMLEN] OF CHAR;
END;

TYPE nfs_opaquedata = RECORD
  len  : CARDINAL;
  data : REF ARRAY OF CHAR;
END;


TYPE filename = nfs_text;
TYPE nfspath = nfs_text;

TYPE ftype = [0..8];
CONST
        ftype_NFNON  = 0;
        ftype_NFREG  = 1;
        ftype_NFDIR  = 2;
        ftype_NFBLK  = 3;
        ftype_NFCHR  = 4;
        ftype_NFLNK  = 5;
        ftype_NFSOCK = 6;
        ftype_NFBAD  = 7;
        ftype_NFFIFO = 8;

TYPE nfstime = 
	RECORD
        seconds: Ctypes.unsigned_int;
        useconds: Ctypes.unsigned_int;
	END;

TYPE fattr = RECORD
        type      : ftype;
        mode      : Ctypes.unsigned_int;
        nlink     : Ctypes.unsigned_int;
        uid       : Ctypes.unsigned_int;
        gid       : Ctypes.unsigned_int;
        size      : Ctypes.unsigned_int;
        blocksize : Ctypes.unsigned_int;
        rdev      : Ctypes.unsigned_int;
        blocks    : Ctypes.unsigned_int;
        fsid      : Ctypes.unsigned_int;
        fileid    : Ctypes.unsigned_int;
        atime     : nfstime;
        mtime     : nfstime;
        ctime     : nfstime;
        END;

TYPE sattr = RECORD
  mode: Ctypes.unsigned_int;
  uid: Ctypes.unsigned_int;
  gid: Ctypes.unsigned_int;
  size: Ctypes.unsigned_int;
  atime: nfstime;
  mtime: nfstime;
END;


TYPE attrstat = RECORD
  status: nfsstat;
  attributes: fattr;
END;

TYPE sattrargs = RECORD
  file       : nfs_fh;
  attributes : sattr;
END;

TYPE diropargs = RECORD
  dir  : nfs_fh;
  name : filename;
END;


TYPE statfsokres = RECORD
  tsize  : Ctypes.unsigned_int;
  bsize  : Ctypes.unsigned_int;
  blocks : Ctypes.unsigned_int;
  bfree  : Ctypes.unsigned_int;
  bavail : Ctypes.unsigned_int;
END;

TYPE statfsres = RECORD
  status : nfsstat;
  ok : statfsokres;
END;


TYPE diropokres = RECORD
  file       : nfs_fh;
  attributes : fattr;
END;

TYPE diropres = RECORD
  status : nfsstat;
  ok     : diropokres;
END;

TYPE nfscookie = ARRAY [1..COOKIESIZE] OF CHAR;
TYPE readdirargs = RECORD
  dir    : nfs_fh;
  cookie : nfscookie;
  count  : Ctypes.unsigned_int;
END;

(* XXX not used
TYPE entry = RECORD
  fileid    : Ctypes.unsigned_int;
  name      : filename;
  cookie    : nfscookie;
  nextentry : REF entry;
END;

TYPE dirlist = RECORD
  entries : REF entry;
  eof     : BOOLEAN;
END;
*)

TYPE readdirres = RECORD
  status : nfsstat;
END;

TYPE readlinkres = RECORD
  status: nfsstat;
  data: nfspath;
END;

TYPE readres = RECORD
  status     : nfsstat;
  attributes : fattr;
  data       : nfs_opaquedata;
END;

TYPE readargs = RECORD
  file       : nfs_fh;
  offset     : Ctypes.unsigned_int;
  count      : Ctypes.unsigned_int;
  totalcount : Ctypes.unsigned_int;
END;

TYPE createargs = RECORD
  where: diropargs;
  attributes: sattr;
END;

TYPE renameargs = RECORD
  from: diropargs;
  to: diropargs;
END;

TYPE linkargs = RECORD
  from: nfs_fh;
  to: diropargs;
END;

TYPE symlinkargs = RECORD
  from: diropargs;
  to: nfspath;
  attributes: sattr;
END;

TYPE writeargs = RECORD
  file        : nfs_fh;
  beginoffset : Ctypes.unsigned_int;
  offset      : Ctypes.unsigned_int;
  totalcount  : Ctypes.unsigned_int;
  data        : REF ARRAY OF CHAR;
END;


(* procedure numbers *)
CONST 
  PROC_NULL       = 0;
  PROC_GETATTR    = 1;
  PROC_SETATTR    = 2;
  PROC_ROOT       = 3;
  PROC_LOOKUP     = 4;
  PROC_READLINK   = 5;
  PROC_READ       = 6;
  PROC_WRITECACHE = 7;
  PROC_WRITE      = 8;
  PROC_CREATE     = 9;
  PROC_REMOVE     = 10;
  PROC_RENAME     = 11;
  PROC_LINK       = 12;
  PROC_SYMLINK    = 13;
  PROC_MKDIR      = 14;
  PROC_RMDIR      = 15;
  PROC_READDIR    = 16;
  PROC_STATFS     = 17;

PROCEDURE GETATTR  (READONLY inParm: nfs_fh;      VAR outParm: attrstat);
PROCEDURE SETATTR  (READONLY inParm: sattrargs;   VAR outParm: attrstat);
PROCEDURE LOOKUP   (READONLY inParm: diropargs;   VAR outParm: diropres);
PROCEDURE READLINK (READONLY inParm: nfs_fh;      VAR outParm: readlinkres);
(* PROCEDURE READ     (READONLY inParm: readargs;    VAR outParm: readres); *)
PROCEDURE PROC_READ2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;
    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed};
PROCEDURE WRITE    (READONLY inParm: writeargs;   VAR outParm: attrstat);
PROCEDURE CREATE   (READONLY inParm: createargs;  VAR outParm: diropres);
PROCEDURE REMOVE   (READONLY inParm: diropargs;   VAR outParm: nfsstat);
PROCEDURE RENAME   (READONLY inParm: renameargs;  VAR outParm: nfsstat);
PROCEDURE LINK     (READONLY inParm: linkargs;    VAR outParm: nfsstat);
PROCEDURE SYMLINK  (READONLY inParm: symlinkargs; VAR outParm: nfsstat);
PROCEDURE MKDIR    (READONLY inParm: createargs;  VAR outParm: diropres);
PROCEDURE RMDIR    (READONLY inParm: diropargs;   VAR outParm: nfsstat);
(* PROCEDURE READDIR  (READONLY inParm: readdirargs; VAR outParm: readdirres);*)
PROCEDURE PROC_READDIR2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed};
PROCEDURE STATFS   (READONLY inParm: nfs_fh;      VAR outParm: statfsres);

PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE Uninit(verbose:BOOLEAN);



VAR debug : BOOLEAN := FALSE;
END Nfsd.

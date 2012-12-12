INTERFACE Mountd;
IMPORT Ctypes, Nfsd;

CONST
  hostOrderPort : Ctypes.unsigned_int = 16_006e;
  netOrderPort  : Ctypes.unsigned_int = 16_6e00;
CONST
  (* Port mapper constants. *)
  Prog : Ctypes.unsigned_int = 100005;
  Vers : Ctypes.unsigned_int = 1;

CONST MNTPATHLEN = 1024;
CONST MNTNAMLEN = 255;

TYPE fhandle = Nfsd.nfs_fh;

(* mount in args *)
TYPE dirpath = RECORD
  len : CARDINAL;
  name : ARRAY [1..MNTPATHLEN] OF CHAR;
END;

(* mount out args if successfull *)
TYPE fhstatus = RECORD
  status: Ctypes.unsigned_int;
  fh : fhandle;
END;

(* procedure numbers *)
CONST 
  PROC_NULL      = 0;
  PROC_MNT       = 1;
  PROC_DUMP      = 2;
  PROC_UMNT      = 3;
  PROC_UMNTALL   = 4;
  PROC_EXPORT    = 5;
  PROC_EXPORTALL = 6;


PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE Uninit(verbose:BOOLEAN);
VAR debug : BOOLEAN := FALSE;

END Mountd.

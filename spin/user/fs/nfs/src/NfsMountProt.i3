(*
 * This file was automatically generated using m3rpcgen.
 *)

INTERFACE NfsMountProt;

IMPORT RPC, RPCSun, Thread;
IMPORT Ctypes;  <*NOWARN*>
CONST MNTPATHLEN = 1024;
CONST MNTNAMLEN = 255;
CONST FHSIZE = 32;

TYPE fhandle = ARRAY [0..FHSIZE - 1] OF CHAR;

TYPE fhstatus = OBJECT
	fhs_status: Ctypes.unsigned_int;
	END;
	fhstatus_0 =
	    fhstatus BRANDED "NfsMountProt_fhstatus_0" OBJECT
                fhs_fhandle: fhandle;
		END;
	fhstatus_Default =
	    fhstatus BRANDED "NfsMountProt_fhstatus_Default" OBJECT
		END;

TYPE dirpath = TEXT;

TYPE name = TEXT;

TYPE mountlist = REF mountbody;

TYPE mountbody = 
	RECORD
        ml_hostname: name;
        ml_directory: dirpath;
        ml_next: mountlist;
	END;

TYPE groups = REF groupnode;

TYPE groupnode = 
	RECORD
        gr_name: name;
        gr_next: groups;
	END;

TYPE exports = REF exportnode;

TYPE exportnode = 
	RECORD
        ex_dir: dirpath;
        ex_groups: groups;
        ex_next: exports;
	END;

CONST MOUNTPROG_prognum = 100005;

CONST MOUNTVERS_versnum = 1;

TYPE MOUNTVERS = OBJECT
	METHODS
	  MOUNTPROC_NULL()
		RAISES {RPC.Failed, Thread.Alerted};
	  MOUNTPROC_MNT(READONLY inParm: dirpath): fhstatus
		RAISES {RPC.Failed, Thread.Alerted};
	  MOUNTPROC_DUMP(): mountlist
		RAISES {RPC.Failed, Thread.Alerted};
	  MOUNTPROC_UMNT(READONLY inParm: dirpath)
		RAISES {RPC.Failed, Thread.Alerted};
	  MOUNTPROC_UMNTALL()
		RAISES {RPC.Failed, Thread.Alerted};
	  MOUNTPROC_EXPORT(): exports
		RAISES {RPC.Failed, Thread.Alerted};
	  MOUNTPROC_EXPORTALL(): exports
		RAISES {RPC.Failed, Thread.Alerted};
	END;

TYPE MOUNTVERSClient = MOUNTVERS OBJECT
	METHODS
	  GetClient(): RPCSun.Client;
	END;

PROCEDURE ImportMOUNTVERS(b: RPCSun.BindingInfo): MOUNTVERS
		RAISES {RPC.Failed, Thread.Alerted};

END NfsMountProt.

INTERFACE NfsdImpl;
IMPORT Nfsd;
PROCEDURE LookupMountPoint(READONLY name: ARRAY OF CHAR; VAR dir: Nfsd.nfs_fh):BOOLEAN;
PROCEDURE Init2(verbose:BOOLEAN);
PROCEDURE Uninit2(verbose:BOOLEAN);
END NfsdImpl.

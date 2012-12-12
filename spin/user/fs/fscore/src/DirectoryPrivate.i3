(* 
 * HISTORY
 * 09-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE DirectoryPrivate;
IMPORT Directory, NameServer;
IMPORT File;
IMPORT FileStat;
IMPORT Error;
IMPORT StatFs;


PROCEDURE methodCreate(
    self : Directory.T; 
    name : NameServer.Name;
    root : NameServer.T): NameServer.T;

PROCEDURE methodLookup(
    self          : Directory.T;
    VAR name          : NameServer.Name; 
    getalias      : BOOLEAN): REFANY 
  RAISES {NameServer.Error};

PROCEDURE methodMkfile(
    self : Directory.T;
    name : NameServer.Name): File.T;
  
PROCEDURE methodStat(
    self: Directory.T;
    VAR s: FileStat.T) RAISES {Error.E};

PROCEDURE Statfs(self: Directory.T; VAR statfs: StatFs.T) RAISES {Error.E};
  
END DirectoryPrivate.
